{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
module Galua.MachUtils where


import           Control.Concurrent (newEmptyMVar)
import           Control.Concurrent.STM (atomically, newEmptyTMVar)
import           Control.Exception (try)
import           Control.Monad (when)
import           Data.IORef
import           Data.Foldable (for_)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Vector.Mutable as IOVector
import           Data.ByteString (ByteString)
import           Foreign (ForeignPtr, Ptr, nullPtr, newForeignPtr, FunPtr
                         , castFunPtr )
import           Foreign.StablePtr
                   (StablePtr, newStablePtr, castStablePtrToPtr)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import           System.Exit
import           System.IO (hPutStrLn, stderr)
import           System.IO.Error (isDoesNotExistError)
import           System.Environment (lookupEnv)

import           Galua.Code
import           Galua.Mach
import           Galua.ImportCode(parseLuaBytecode)
import           Galua.Reference
import           Galua.Value
import           Galua.FunValue(FunctionValue(..))
import           Galua.LuaString
import           Galua.CObjInfo(CObjInfo,cfunInfoFun)
import           Galua.Util.SmallVec(SmallVec)
import qualified Galua.Util.Stack as Stack
import qualified Galua.Util.SizedVector as SV
import           Galua.Util.Cache
import           Galua.Util.IOURef


import           Process (readProcessWithExitCodeBS)




data VMState  = FinishedOk !(SmallVec Value)
              | FinishedWithError !Value
              | Running !VM !NextStep
              | RunningInC !VM




-- | Make a blank C-style exec env to be used when creating threads.
newThreadExecEnv :: IO ExecEnv
newThreadExecEnv =
  do stack  <- SV.new
     api    <- newIORef NoApiCall
     upvals <- IOVector.new 0
     return $! ExecInC
               CExecEnv { cExecStack        = stack
                        , cExecUpvals       = upvals
                        , cExecFunction     = blankCFunName
                        , cExecApiCall      = api
                        , cExecClosure      = Nil
                        }


--------------------------------------------------------------------------------
-- Exceptions
--------------------------------------------------------------------------------
luaError' :: String -> IO NextStep
luaError' str =
  do s <- fromByteString (packUtf8 str)
     return (ThrowError (String s))


newMachineEnv :: AllocRef -> MachConfig -> IO MachineEnv
newMachineEnv aref machConfig =
  do let refLoc = RefLoc { refLocCaller = MachSetup
                         , refLocSite   = MachSetup
                         }
     machGlobals       <- newTable aref refLoc 0 0
     machRegistry      <- newTable aref refLoc 0 0
     machNextChunkId   <- newIORef 0
     machMetatablesRef <- newIORef Map.empty
     machLuaServer     <- atomically newEmptyTMVar
     machCServer       <- newEmptyMVar
     machGarbage       <- newIORef []

     machStablePtr <- newStablePtr ExternalLuaState
       { extLuaStateLuaServer = machLuaServer
       , extLuaStateCServer   = machCServer
       }

     machMainThreadRef <- allocNewThread machStablePtr aref refLoc ThreadRunning
     setTableRaw machRegistry (Number 1) (Thread machMainThreadRef)
     setTableRaw machRegistry (Number 2) (Table machGlobals)


     machNameCache     <- newIORef (cacheEmpty 50000)
     machCFunInfo      <- cfunInfoFun
     machJIT           <- newIORef Map.empty
     return MachineEnv { .. }


foreign import ccall "galua_allocate_luaState"
  allocateLuaState :: Ptr () -> Int -> IO (Ptr ())

foreign import ccall "&galua_free_luaState"
  freeLuaState :: FunPtr (Ptr () -> IO ())

newCPtr :: StablePtr ExternalLuaState -> Int -> IO (ForeignPtr ())
newCPtr initialToken threadId =
  do luastate <- allocateLuaState (castStablePtrToPtr initialToken) threadId
     when (nullPtr == luastate)
       (fail "lua_State allocation failed")
     newForeignPtr freeLuaState luastate

--------------------------------------------------------------------------------
-- Allocation of values
--------------------------------------------------------------------------------


machNewThread :: VM -> IO (Reference Thread)
machNewThread vm =
  do let aref = vmAllocRef vm
     loc <- machRefLoc vm
     let sptr = machStablePtr (vmMachineEnv vm)
     allocNewThread sptr aref loc ThreadNew

allocNewThread ::
  StablePtr ExternalLuaState ->
  AllocRef ->
  RefLoc ->
  ThreadStatus -> IO (Reference Thread)
allocNewThread sptr aref refLoc st =
  do refId <- newRefId aref

     threadCPtr <- newCPtr sptr  refId


     threadStatus <- newIORef st
     stExecEnv    <- newIORef =<< newThreadExecEnv
     stHandlers   <- newIORef []
     stStack      <- newIORef Stack.empty
     stPC         <- newIOURef 0

     let th = MkThread{..}
     newRefWithId aref refId refLoc th



--------------------------------------------------------------------------------
-- Mapping from C to Lua
--------------------------------------------------------------------------------


machLookupRef :: AllocType a => VM -> Int -> IO (Maybe (Reference a))
machLookupRef vm n = lookupRef (vmAllocRef vm) n

extToThreadRef :: VM -> Int -> IO (Reference Thread)
extToThreadRef vm tid =
  do mb <- machLookupRef vm tid
     case mb of
       Nothing -> fail "extToThreadRef: Unexpectedly invalid weak thread reference"
       Just x  -> return x

--------------------------------------------------------------------------------




machIsMainThread :: VM -> Reference Thread -> Bool
machIsMainThread vm t = t == machMainThreadRef (vmMachineEnv vm)

machLookupCFun :: VM -> CFun -> IO CObjInfo
machLookupCFun vm f =
  do let menv        = vmMachineEnv vm
         cacheRef    = machNameCache menv
         getCFunInfo = machCFunInfo menv
     {- Note that there is a potential race when we are updating the cache.
     We don't need to worry about it, because the cache is a "best effort"
     strucutre and we should be getting reasonable results even if some
     updates get lost. -}
     cache <- readIORef cacheRef
     case cacheLookup f cache of
       Just (i,newCache) ->
         do writeIORef cacheRef $! newCache
            return i
       Nothing ->
         do o <- getCFunInfo (castFunPtr f)
            writeIORef cacheRef $! cacheInsert f o cache
            return o



-- | Build a closure for a function using the given value as its only upvalue
chunkToClosure ::
  VM ->
  Maybe String  {- ^ source name -}   ->
  ByteString    {- ^ function text -} ->
  Value         {- ^ first upvalue (i.e., "upvalues") -} ->
  IO (Either String (Reference Closure))
chunkToClosure vm name bytes env =
  do res <- parser
     case res of
       Left e -> return (Left e)
       Right (Chunk numUpval func) ->
         do let initial 0 = env
                initial _ = Nil

                menv = vmMachineEnv vm


            (upvalues,modNum) <-
              do upvalues <- IOVector.new numUpval
                 for_ [0 .. numUpval-1 ] $ \i ->
                   IOVector.write upvalues i =<< newIORef (initial i)

                 modNum   <- atomicModifyIORef' (machNextChunkId menv)
                               $ \r -> (r + 1, r)
                 return (upvalues, modNum)

            let fid = rootFun modNum
            clo <- machNewClosure vm (LuaFunction fid func) upvalues
            machOnChunkLoad (machConfig menv) name bytes modNum func
            return (Right clo)

  where
  luaSignature = "\x1bLua"
  bytesL = L.fromStrict bytes
  parser | luaSignature `B.isPrefixOf` bytes = parseLuaBytecode name bytesL
         | otherwise                         = parseLua name bytesL

activateThread :: Reference Closure -> SmallVec Value -> Reference Thread ->
                                                                        IO ()
activateThread cRef vs tRef =
  setThreadField threadStatus tRef
     (ThreadSuspended (return (FunCall cRef vs Nothing (return . FunReturn))))

parseLua :: Maybe String -> L.ByteString -> IO (Either String Chunk)
parseLua mbName src =
  do luac <- fromMaybe "luac" <$> lookupEnv "LUAC"
     res <- try (readProcessWithExitCodeBS luac["-o","-","-"] src)
     case res of
       Left e ->
         do when (isDoesNotExistError e)
                 (hPutStrLn stderr "WARNING: luac not found in path (configurable with $LUAC)")
            return (Left (show e))
       Right (ExitFailure{},_,err) -> return $! Left $! unpackUtf8 $! L.toStrict err
       Right (ExitSuccess,chunk,_) -> parseLuaBytecode mbName chunk



