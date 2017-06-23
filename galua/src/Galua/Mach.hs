{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
module Galua.Mach where


import           Control.Concurrent (MVar, newEmptyMVar)
import           Control.Concurrent.STM (TMVar, atomically, newEmptyTMVar)
import           Control.Exception (try)
import           Control.Monad (when)
import           Control.Monad.IO.Class
import           Data.IORef
import           Data.Foldable (for_)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Vector (Vector)
import           Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as IOVector
import           Data.ByteString (ByteString)
import           Foreign (ForeignPtr, Ptr, nullPtr, newForeignPtr, FunPtr
                         , castFunPtr )
import           Foreign.C (CInt)
import           Foreign.StablePtr
                   (StablePtr, newStablePtr, castStablePtrToPtr)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import           System.Exit
import           System.IO (hPutStrLn, stderr)
import           System.IO.Error (isDoesNotExistError)
import           System.Environment (lookupEnv)

import           Galua.Code
import           Galua.ImportCode(parseLuaBytecode)
import           Galua.Micro.AST(BlockName,BlockStmt)
import           Galua.Reference
import           Galua.Value
import           Galua.FunValue(FunctionValue(..))
import           Galua.LuaString
import           Galua.CObjInfo(CObjInfo,cfunInfoFun)
import           Galua.Util.SmallVec(SmallVec)
import           Galua.Util.Stack(Stack)
import qualified Galua.Util.Stack as Stack
import qualified Galua.Util.SizedVector as SV
import           Galua.Util.Cache
import           Galua.Util.Weak (MakeWeak(..), mkWeakIORef')
import           Galua.Util.IOURef


import           Process (readProcessWithExitCodeBS)

data VM = VM
  { vmMachineEnv  :: !MachineEnv
    -- ^ Global machine state

  , vmBlocked     :: !(Stack (Reference Thread))
    -- ^ Blocked coroutines (they resumed something)

  , vmCurThread   :: (Reference Thread)
    -- ^ This is what's executing at the moment.

  , vmCurExecEnv  :: ExecEnv
    -- ^ This is a copy of the current execution environment for
    -- the current thread for quicker access.

  , vmAllocRef    :: {-# UNPACK #-} !AllocRef
    -- ^ This is used to allocate Lua references.

  }


emptyVM :: AllocRef -> MachineEnv -> IO VM
emptyVM aref menv =
  do let vmCurThread = machMainThreadRef menv
     vmCurExecEnv <- getThreadField stExecEnv vmCurThread
     return VM { vmMachineEnv = menv
               , vmBlocked    = Stack.empty
               , vmAllocRef   = aref
               , ..
               }


data VMState  = FinishedOk !(SmallVec Value)
              | FinishedWithError !Value
              | Running !VM !NextStep
              | RunningInC !VM


data CCallState
  = CReturned Int
  | CReEntry ApiCall (VM -> IO NextStep)

data CNextStep = CAbort | CResume | CCallback (IO CInt)


-- | Machine instructions to do with control flow.
data NextStep
  = Goto {-# UNPACK #-} !Int
    -- ^ Continue executing this function at a new address

  | FunCall {-# UNPACK #-} !(Reference Closure) -- call this
            !(SmallVec Value)                   -- using these arguments
            !(Maybe Handler)                    -- using this handler
            !(SmallVec Value -> IO NextStep)    -- result goes here

  | FunReturn !(SmallVec Value)
    -- ^ Function execution succeeded with results

  | ErrorReturn !Value
    -- ^ Function execution succeeded with results

  | FunTailcall {-# UNPACK #-} !(Reference Closure) !(SmallVec Value)
    -- ^ Call a function and return its result

  | ThrowError !Value
    -- ^ An error occured

  | WaitForC
    -- ^ Yield to C, wait for a response from the C reentry thread
    -- See 'CCallState' for possible responses

  | Resume !(Reference Thread) !(ThreadResult -> IO NextStep)
    -- ^ Resume the given suspended thread

  | Yield !(IO NextStep)

  | ThreadExit !(SmallVec Value)
  | ThreadFail !Value

  | ApiStart !ApiCall !(IO NextStep)
  | ApiEnd !(Maybe PrimArgument)

  | Interrupt !NextStep

dumpNextStep :: NextStep -> String
dumpNextStep next =
  case next of
    Goto n          -> "goto " ++ show n
    FunCall r _ _ _ -> "call " ++ show (prettyRef r)
    FunReturn _     -> "return"
    ThreadExit _    -> "thread exit"
    ThreadFail _    -> "thread fail"
    ErrorReturn _   -> "error return"
    FunTailcall r _ -> "tailcall " ++ show (prettyRef r)
    ThrowError _    -> "throw"
    WaitForC        -> "waitforC"
    Resume r _      -> "resume " ++ show (prettyRef r)
    Yield _         -> "yield"
    ApiStart api _  -> "apistart " ++ apiCallMethod api
    ApiEnd _        -> "apiend"
    Interrupt {}    -> "interrupt"



data Handler     = Handler { handlerType :: HandlerType
                           , handlerK    :: Value -> IO NextStep
                           }
data HandlerType = FunHandler (Reference Closure)
                 | DefaultHandler

data StackFrame
  = ErrorFrame
  | CallFrame !Int -- PC
              !ExecEnv
              !(Maybe (Value -> IO NextStep)) -- Error
              !(SmallVec Value -> IO NextStep) -- Normal


data Thread = MkThread
  { threadStatus :: {-# UNPACK #-} !(IORef ThreadStatus)

  , threadCPtr   :: {-# UNPACK #-} !(ForeignPtr ())
    {- ^ The representation of thread used in C.
         This contains space for the setjmp/longjmp buffer, and a reference
         back to this object. -}

  , stExecEnv   :: {-# UNPACK #-} !(IORef ExecEnv)
  , stHandlers  :: {-# UNPACK #-} !(IORef [HandlerType])
  , stStack     :: {-# UNPACK #-} !(IORef (Stack StackFrame))
  , stPC        :: {-# UNPACK #-} !(IOURef Int)

  }


instance MakeWeak Thread where
  makeWeak th = mkWeakIORef' (stStack th) th


data ThreadStatus

  = ThreadSuspended (IO NextStep)
    -- ^ A thread that yielded. Call continuation with result to resume with.

  | ThreadNew
    -- ^ A fresh thread that hasn't been resumed yet

  | ThreadRunning
    -- ^ The currently executing thread.
    -- Invariant: 'vmCurThread' has this status.

  | ThreadNormal (ThreadResult -> IO NextStep)
    -- ^ A thread that resumed another thread.
    -- Invariant: all threads in 'vmBlocked' have this status.

  | ThreadCrashed
    -- ^ A thread that completed executing with an error, stack not unwound

isThreadRunning :: ThreadStatus -> Bool
isThreadRunning ThreadRunning = True
isThreadRunning _             = False

data ThreadResult
  = ThreadYield                 -- ^ The resumed thread yielded back to us.
  | ThreadReturn (SmallVec Value) -- ^ The resuemd thread completed normally.
  | ThreadError Value           -- ^ The resumed thread crashed.


setThreadField ::
  MonadIO m => (Thread -> IORef a) -> Reference Thread -> a -> m ()
setThreadField sel = \ref !val ->
  liftIO (writeIORef (sel (referenceVal ref)) val)
{-# INLINE getThreadField #-}

getThreadField :: MonadIO m => (Thread -> IORef a) -> Reference Thread -> m a
getThreadField sel = \ref ->
  liftIO (readIORef (sel (referenceVal ref)))
{-# INLINE setThreadField #-}

{-# INLINE getThreadPC #-}
getThreadPC :: MonadIO m => Reference Thread -> m Int
getThreadPC = liftIO . readIOURef . stPC . referenceVal

{-# INLINE setThreadPC #-}
setThreadPC :: MonadIO m => Reference Thread -> Int -> m ()
setThreadPC r x = liftIO (writeIOURef (stPC (referenceVal r)) x)

type TypeMetatables = Map ValueType (Reference Table)

data MachineEnv = MachineEnv
  { machGlobals       :: !(Reference Table)
    -- ^ The table of globals, accessable from Lua.

  , machRegistry      :: !(Reference Table)
    -- ^ The "registry", which is similar to the globals but
    -- it is only accessable via the C API.

  , machMetatablesRef :: {-# UNPACK #-} !(IORef TypeMetatables)
    -- ^ Overloaded operations for primitive types.

  , machMainThreadRef :: !(Reference Thread)

  , machNextChunkId   :: {-# UNPACK #-} !(IORef Int)
    -- ^ Used to generate identities when we load new chunks.

  , machConfig        :: !MachConfig
    -- ^ Callbacks for interesting events.

  , machLuaServer     :: !(TMVar CCallState)
    -- ^ The Lua interpreter listens on this MVar for messages
    -- coming in from the C threads

  , machCServer       :: !(MVar CNextStep)
    -- ^ The currently available C thread listens on this MVar
    -- for messages coming from the Lua interpreter

  , machGarbage       :: {-# UNPACK #-} !(IORef [Value])
    -- ^ These things need to be garbage collected.

  , machNameCache     :: {-# UNPACK #-} !(IORef (Cache CFun CObjInfo))
    -- ^ A cache mapping addresses of C functions to human-readable
    -- information about them.

  , machStablePtr     :: {-# UNPACK #-} !(StablePtr ExternalLuaState)

  , machCFunInfo      :: FunPtr () -> IO CObjInfo
  }



data MachConfig = MachConfig
  { machOnChunkLoad :: Maybe String -> ByteString -> Int -> Function -> IO ()
    -- ^ Invoke this when loading a chunk
  , machOnShutdown  :: IO ()
  , machOnQuery     :: String -> IO Value
  }



data ExecEnv = ExecInLua  {-# UNPACK #-} !LuaExecEnv
             | ExecInMLua {-# UNPACK #-} !MLuaExecEnv
             | ExecInC    {-# UNPACK #-} !CExecEnv

-- | Get the up-values for the execution environment.
execUpvals :: ExecEnv -> IOVector (IORef Value)
execUpvals env =
  case env of
    ExecInLua   LuaExecEnv  { luaExecUpvals }   -> luaExecUpvals
    ExecInMLua  MLuaExecEnv { mluaExecUpvals }  -> mluaExecUpvals
    ExecInC     CExecEnv    { cExecUpvals   }   -> cExecUpvals

execClosure :: ExecEnv -> Value
execClosure env =
  case env of
    ExecInLua lenv -> luaExecClosure lenv
    ExecInMLua menv -> mluaExecClosure menv
    ExecInC   cenv -> cExecClosure cenv

execFun :: ExecEnv -> FunctionValue
execFun env =
  case env of
    ExecInLua lenv  -> LuaFunction (luaExecFID lenv) (luaExecFunction lenv)
    ExecInMLua menv -> LuaFunction (mluaExecFID menv) (mluaExecFunction menv)
    ExecInC cenv    -> CFunction (cExecFunction cenv)

execCStack :: ExecEnv -> SV.SizedVector Value
execCStack env =
  case env of
    ExecInC cenv  -> cExecStack cenv
    ExecInLua {}  -> error "[bug] execCStack: not a C execution environment"
    ExecInMLua {} -> error "[bug] execCStack: not a C execution environment"

execApiCall :: ExecEnv -> IORef ApiCallStatus
execApiCall env =
  case env of
    ExecInC cenv -> cExecApiCall cenv
    ExecInLua {} -> error "[bug] execApiCall: not a C execution environment"
    ExecInMLua {} -> error "[bug] execApiCall: not a C execution environment"



data VarResults = NoVarResults
                | VarResults !Reg !(SmallVec Value)


-- | Execution environment for a Lua function
data LuaExecEnv = LuaExecEnv
  { luaExecRegs     :: {-# UNPACK #-} !(IOVector (IORef Value))
    -- ^ local variables

  , luaExecVarargs  :: {-# UNPACK #-} !(IORef [Value])
    -- ^ List of additional function arguments (for vararg functions)

  , luaExecVarress  :: {-# UNPACK #-} !(IORef VarResults)
    {- ^ Stores results of function callls.  Used when we need all results
    of a function, but we don't know how many results will be returned
    by the function.  For example, if all results of a function are
    placed in a table, or passed on to another function. -}

  , luaExecUpvals   :: {-# UNPACK #-} !(IOVector (IORef Value))
  , luaExecCode     :: {-# UNPACK #-} !(Vector OpCode)

    -- The current function
  , luaExecClosure  :: !Value         -- ^ used by debug API
  , luaExecFID      :: !FunId         -- ^ extra info
  , luaExecFunction :: !Function      -- ^ extra info
  }



data MLuaExecEnv = MLuaExecEnv
  { mluaExecRegsValue :: {-# UNPACK #-} !(IOVector Value)
  , mluaExecRegsRefs  :: {-# UNPACK #-} !(IOVector (IORef Value))
    -- ^ Local values, split across two arrays.
    -- INVARIANT: if an index is defined in one of the arrays, it
    -- will be undefined in the other.

  , mluaExecRegsTMP :: {-# UNPACK #-} !(IOVector Value)
    -- ^ Additional--temporary--registers

  , mluaExecArgReg  :: {-# UNPACK #-} !(IORef [Value]) -- varars
  , mluaExecListReg :: {-# UNPACK #-} !(IORef [Value])  -- varres


  , mluaExecUpvals :: {-# UNPACK #-} !(IOVector (IORef Value))
  , mluaExecCode   :: !(Map BlockName (Vector BlockStmt)) -- ^ Our CFG


  -- The current functions
  , mluaExecClosure   :: !Value
  , mluaExecFID       :: !FunId                         -- ^ Our functoin ID
  , mluaExecFunction  :: !Function
  }




-- | Execution environment for a C function
data CExecEnv = CExecEnv
  { cExecStack      :: {-# UNPACK #-} !(SV.SizedVector Value)

  , cExecUpvals     :: {-# UNPACK #-} !(IOVector (IORef Value))

  , cExecApiCall    :: {-# UNPACK #-} !(IORef ApiCallStatus)

    -- The current function
  , cExecClosure    :: !Value         -- ^ used by debug API
  , cExecFunction   :: !CFunName      -- ^ extra info
  }






-- | Get the function id for this stack frame.
execFunId :: ExecEnv -> Maybe FunId
execFunId env =
  case env of
    ExecInLua x -> Just (luaExecFID x)
    ExecInMLua x -> Just (mluaExecFID x)
    ExecInC _   -> Nothing


data ApiCallStatus
  = ApiCallActive !ApiCall
  | ApiCallAborted !ApiCall
  | NoApiCall

data ApiCall = ApiCall
  { apiCallMethod :: !String
  , apiCallReturn :: CObjInfo -- ^ lazily generated location information
  , apiCallArgs   :: ![PrimArgument]
  , apiContinuation :: Maybe (CInt -> Ptr () -> IO CInt)
  }

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

machNewClosure ::
  VM -> FunctionValue -> IOVector (IORef Value) -> IO (Reference Closure)
machNewClosure vm fun upvals =
  do let aref = vmAllocRef vm
     loc  <- machRefLoc vm
     newClosure aref loc fun upvals

machNewTable ::
  VM ->
  Int {- ^ array size -} ->
  Int {- ^ hashtable size -} ->
  IO (Reference Table)
machNewTable vm aSz hSz =
  do let aref = vmAllocRef vm
     loc <- machRefLoc vm
     newTable aref loc aSz hSz

machNewUserData :: VM -> ForeignPtr () -> Int -> IO (Reference UserData)
machNewUserData vm fp n =
  do let aref = vmAllocRef vm
     loc  <- machRefLoc vm
     newUserData aref loc fp n

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

machRefLoc :: VM -> IO RefLoc
machRefLoc vm =
  do let tref = vmCurThread vm
     pc    <- getThreadPC tref
     let eenv = vmCurExecEnv vm
     stack <- getThreadField stStack tref
     return RefLoc { refLocCaller = getCaller stack
                   , refLocSite   = mkLoc eenv pc
                   }
  where
  getCaller st = case Stack.pop st of
                   Just (ErrorFrame, more)       -> getCaller more
                   Just (CallFrame pc env _ _,_) -> mkLoc env pc
                   Nothing -> MachSetup -- XXX: or can this happen?

  mkLoc env pc = case env of
                   ExecInLua lenv  -> InLua (luaExecFID lenv) pc
                   ExecInMLua lenv -> InLua (mluaExecFID lenv) pc
                   ExecInC cenv    -> InC (cExecFunction cenv)




--------------------------------------------------------------------------------
-- Mapping from C to Lua
--------------------------------------------------------------------------------

data ExternalLuaState = ExternalLuaState
  { extLuaStateLuaServer   :: TMVar CCallState
  , extLuaStateCServer     :: MVar CNextStep
  }

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


data instance Reference Thread = ThreadRef
  { threadReferenceId  :: {-# UNPACK #-} !Int
  , threadReferenceLoc :: {-# UNPACK #-} !RefLoc
  , threadReferenceVal :: {-# UNPACK #-} !Thread
  }

instance ReferenceType Thread where
  constructReference = ThreadRef
  referenceId = threadReferenceId
  referenceVal = threadReferenceVal
  referenceLoc = threadReferenceLoc
