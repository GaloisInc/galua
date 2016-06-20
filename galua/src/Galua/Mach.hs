{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Galua.Mach where


import           Control.Concurrent (MVar, newEmptyMVar)
import           Control.Exception (try)
import           Control.Monad (when)
import           Control.Monad.IO.Class
import           Data.IORef
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Data.ByteString (ByteString)
import           Data.Void
import           Foreign (ForeignPtr, Ptr, nullPtr, newForeignPtr,
                            finalizerFree, plusPtr, FunPtr)
import           Foreign.StablePtr
                   (newStablePtr, freeStablePtr, castStablePtrToPtr)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import           System.Exit
import           System.IO (hPutStrLn, stderr)
import           System.IO.Error (isDoesNotExistError)
import           System.Environment (lookupEnv)
import qualified System.Clock as Clock

import           Language.Lua.Bytecode
import           Language.Lua.Bytecode.FunId
import           Language.Lua.Bytecode.Parser

import           Galua.Reference
import           Galua.Stack(Stack)
import           Galua.Value
import           Galua.LuaString
import qualified Galua.Stack as Stack
import qualified Galua.SizedVector as SV

import           GHC.Exts (inline)

import           Process (readProcessWithExitCodeBS)

data VM = VM
  { vmMachineEnv  :: !MachineEnv
    -- ^ Global machine state

  , vmBlocked     :: !(Stack (Reference Thread))
    -- ^ Blocked coroutines (they resumed something)

  , vmCurThread   :: !(Reference Thread)
    -- ^ This is what's executing at the moment.
  }


emptyVM :: MachineEnv -> VM
emptyVM menv =
   VM { vmMachineEnv = menv
      , vmBlocked    = Stack.empty
      , vmCurThread  = machMainThreadRef menv
      }

vmUpdateThread :: MonadIO m => VM -> (Thread -> Thread) -> m ()
vmUpdateThread VM { vmCurThread } f = modifyRef vmCurThread f


data VMState  = FinishedOk [Value]
              | FinishedWithError Value
              | Running VM NextStep


data CCallState
  = CReturned Int
  | CReEntry String [PrimArgument] (Mach ())

data CNextStep = CAbort | CResume | CCallback CFun


-- | Machine instructions to do with control flow.
data NextStep
  = Goto Int
    -- ^ Continue executing this function at a new address

  | FunCall (Reference Closure) [Value]   -- call this
            (Maybe Handler)               -- using this handler
            ([Value] -> NextStep)         -- result goes here

  | FunReturn [Value]
    -- ^ Function execution succeeded with results

  | ErrorReturn Value
    -- ^ Function execution succeeded with results

  | FunTailcall (Reference Closure) [Value]
    -- ^ Call a function and return its result

  | ThrowError Value
    -- ^ An error occured

  | PrimStep (Alloc NextStep)
    -- ^ Do a side effect

  | Resume (Reference Thread) (ThreadResult -> NextStep)
    -- ^ Resume the given suspended thread

  | Yield NextStep

  | ThreadExit [Value]
  | ThreadFail Value

  | ApiStart ApiCall NextStep NextStep
  | ApiEnd ApiCall


dumpNextStep :: NextStep -> String
dumpNextStep next =
  case next of
    Goto n          -> "goto " ++ show n
    FunCall r _ _ _ -> "call " ++ show (prettyRef r)
    FunReturn _     -> "return"
    ThreadExit _  -> "thread exit"
    ThreadFail _  -> "thread fail"
    ErrorReturn _   -> "error return"
    FunTailcall r _ -> "tailcall " ++ show (prettyRef r)
    ThrowError _    -> "throw"
    PrimStep _      -> "prim"
    Resume r _      -> "resume " ++ show (prettyRef r)
    Yield _         -> "yield"
    ApiStart api _ _ -> "apistart " ++ apiCallMethod api
    ApiEnd api       -> "apiend " ++ apiCallMethod api



data Handler     = Handler { handlerType :: HandlerType
                           , handlerK    :: Value -> NextStep
                           }
data HandlerType = FunHandler (Reference Closure)
                 | DefaultHandler

data StackFrame
  = ErrorFrame
  | CallFrame Int -- PC
              ExecEnv
              (Maybe (Value -> NextStep)) -- Error
              ([Value] -> NextStep) -- Normal


data Thread = MkThread
  { threadStatus :: !ThreadStatus

  , threadCPtr   :: !(ForeignPtr ())
    {- ^ The representation of thread used in C.
         This contains space for the setjmp/longjmp buffer, and a reference
         back to this object. -}

  , stExecEnv   :: !ExecEnv
  , stHandlers  :: ![HandlerType]
  , stStack     :: !(Stack StackFrame)
  , stPC        :: !Int

  }


data ThreadStatus

  = ThreadSuspended NextStep
    -- ^ A thread that yielded. Call continuation with result to resume with.

  | ThreadNew
    -- ^ A fresh thread that hasn't been resumed yet

  | ThreadRunning
    -- ^ The currently executing thread.
    -- Invariant: 'vmCurThread' has this status.

  | ThreadNormal (ThreadResult -> NextStep)
    -- ^ A thread that resumed another thread.
    -- Invariant: all threads in 'vmBlocked' have this status.

  | ThreadCrashed
    -- ^ A thread that completed executing with an error, stack not unwound

isThreadRunning :: ThreadStatus -> Bool
isThreadRunning ThreadRunning = True
isThreadRunning _             = False

data ThreadResult
  = ThreadYield                 -- ^ The resumed thread yielded back to us.
  | ThreadReturn [Value]        -- ^ The resuemd thread completed normally.
  | ThreadError Value           -- ^ The resumed thread crashed.





setThreadStatus :: MonadIO m => Reference Thread -> ThreadStatus -> m ()
setThreadStatus ref st = modifyRef ref $ \t -> t { threadStatus = st }

getThreadStatus :: MonadIO m => Reference Thread -> m ThreadStatus
getThreadStatus ref = fmap threadStatus (readRef ref)


newtype Mach a = Mach { unMach :: VM -> (a -> NextStep) -> NextStep }

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

  , machLuaServer     :: !(MVar CCallState)
    -- ^ The Lua interpreter listens on this MVar for messages
    -- coming in from the C threads

  , machCServer       :: !(MVar CNextStep)
    -- ^ The currently available C thread listens on this MVar
    -- for messages coming from the Lua interpreter

  , machGarbage       :: {-# UNPACK #-} !(IORef [Value])
    -- ^ These things need to be garbage collected.

  , machProfiling     :: {-# UNPACK #-} !ProfilingInfo
    -- ^ Profiling information
  }

data ProfilingInfo = ProfilingInfo
  { profCallCounters  :: {-# UNPACK #-} !(IORef (Map FunName Int))
  , profAllocCounters :: {-# UNPACK #-} !(IORef (Map CodeLoc Int))
  , profFunctionTimers :: {-# UNPACK #-} !(IORef (Map FunName FunctionRuntimes))
    -- ^ How many times was a particular function called.
  }

data FunctionRuntimes = FunctionRuntimes
  { runtimeIndividual :: {-# UNPACK #-} !Clock.TimeSpec -- ^ Time spent in this fuction directly
  , runtimeCumulative :: {-# UNPACK #-} !Clock.TimeSpec -- ^ Time spent in this function its calls
  , runtimeCounter    :: !Int -- ^ Number of active stack frames
  }

data MachConfig = MachConfig
  { machOnChunkLoad :: Maybe String -> ByteString -> Int -> Function -> IO ()
    -- ^ Invoke this when loading a chunk
  }



data ExecEnv = ExecEnv
  { execStack    :: !(SV.SizedVector (IORef Value))
  , execUpvals   :: !(Vector (IORef Value))
  , execFunction :: !FunctionValue
  , execVarargs  :: !(IORef [Value])
  , execApiCall  :: !(IORef ApiCallStatus)
  , execClosure  :: !Value
  , execCreateTime :: {-# UNPACK #-} !Clock.TimeSpec
  , execChildTime :: {-# UNPACK #-} !Clock.TimeSpec
  }

data ApiCallStatus
  = ApiCallActive !ApiCall !NextStep
  | ApiCallAborted !ApiCall
  | NoApiCall

data ApiCall = ApiCall
  { apiCallMethod :: !String
  , apiCallArgs   :: ![PrimArgument]
  }

-- | Make a blank exec env to be used when creating threads.
newThreadExecEnv :: IO ExecEnv
newThreadExecEnv =
  do stack <- SV.new
     api   <- newIORef NoApiCall
     var   <- newIORef []
     time  <- Clock.getTime Clock.ProcessCPUTime
     return ExecEnv { execStack    = stack
                    , execUpvals   = Vector.empty
                    , execFunction = CFunction blankCFunName
                    , execVarargs  = var
                    , execApiCall  = api
                    , execClosure  = Nil
                    , execCreateTime = time
                    , execChildTime  = 0
                    }

instance Functor Mach where
  fmap f m = Mach $ \env k -> unMach m env $ \x -> k (f x)
  x <$ m   = Mach $ \env k -> unMach m env $ \_ -> k x
  {-# INLINE fmap #-}
  {-# INLINE (<$) #-}

instance Applicative Mach where
  pure x  = Mach $ \_   k -> k x
  m <*> n = Mach $ \env k -> unMach m env $ \f ->
                             unMach n env $ \x -> k (f x)
  m  *> n = Mach $ \env k -> unMach m env $ \_ ->
                             unMach n env k
  m <*  n = Mach $ \env k -> unMach m env $ \x ->
                             unMach n env $ \_ -> k x
  {-# INLINE (*>) #-}
  {-# INLINE (<*) #-}
  {-# INLINE (<*>) #-}
  {-# INLINE pure #-}

instance Monad Mach where
  m >>= f  = Mach $ \env k -> unMach m env $ \a ->
                              unMach (f a) env k
  fail e   = Mach $ \_ _   -> error e
  (>>)     = (*>)
  {-# INLINE (>>=) #-}
  {-# INLINE return #-}
  {-# INLINE (>>) #-}

instance MonadIO Mach where
  liftIO m = liftAlloc (liftIO m)

instance NameM Mach where
  newRef refLoc = liftAlloc . newRef refLoc
  lookupRef = liftAlloc . lookupRef
  {-# INLINE newRef #-}
  {-# INLINE lookupRef #-}

liftAlloc :: Alloc a -> Mach a
liftAlloc m = Mach $ \_ k -> PrimStep (fmap k m)


machTailcall :: Reference Closure -> [Value] -> Mach a
machTailcall f vs = abort (FunTailcall f vs)

machCall :: Reference Closure -> [Value] -> Mach [Value]
machCall f vs = Mach $ \_ -> FunCall f vs Nothing

machTry :: HandlerType -> Reference Closure -> [Value] -> Mach (Either Value [Value])
machTry h f vs = Mach $ \_ k ->
  FunCall f vs (Just Handler { handlerType = h
                             , handlerK = k . Left
                             })
               (k . Right)

machCallPrim :: Prim -> [Value] -> Mach Void
machCallPrim p vs = machReturn =<< p vs

machReturn :: [Value] -> Mach a
machReturn xs = abort (FunReturn xs)

machThrow :: Value -> Mach a
machThrow e = abort (ThrowError e)

machResume :: Reference Thread -> Mach ThreadResult
machResume t = Mach $ \_ -> Resume t

machYield :: Mach ()
machYield = Mach $ \_ k -> Yield (k ())

machApiCall :: ApiCall -> Mach () -> Mach ()
machApiCall apiCall (Mach op) =
  Mach $ \vm k -> ApiStart apiCall (op vm (\_ -> ApiEnd apiCall)) (k ())


machVM :: Mach VM
machVM = Mach $ \e k -> k e

machGoto :: Int -> Mach a
machGoto n = abort (Goto n)

machRefLoc :: Mach RefLoc
machRefLoc =
  do th <- readRef =<< machCurrentThread
     return RefLoc { refLocCaller = getCaller (stStack th)
                   , refLocSite   = mkLoc (stExecEnv th) (stPC th)
                   }
  where
  getCaller st = case Stack.pop st of
                   Just (ErrorFrame, more)       -> getCaller more
                   Just (CallFrame pc env _ _,_) -> mkLoc env pc
                   Nothing -> MachSetup -- XXX: or can this happen?

  mkLoc env pc = case execFunction env of
                   CFunction nm      -> InC nm
                   LuaFunction fid _ -> InLua fid pc



-- private
abort :: NextStep -> Mach a
abort x = Mach $ \_ _ -> x






------------------------------------------------------------------------
-- Derived machine functions
------------------------------------------------------------------------

getsMachEnv :: (MachineEnv -> a) -> Mach a
getsMachEnv f = fmap f machCurrentEnv

-- | Inspect the current execution environment after applying
-- a function.
getsExecEnv :: (ExecEnv -> a) -> Mach a
getsExecEnv f =
  do ref <- machCurrentThread
     th  <- readRef ref
     return (f (stExecEnv th))

getsVM :: (VM -> a) -> Mach a
getsVM f = fmap f machVM

newMachineEnv :: MachConfig -> Alloc MachineEnv
newMachineEnv machConfig =
  do let refLoc = RefLoc { refLocCaller = MachSetup
                         , refLocSite   = MachSetup
                         }
     machGlobals       <- newTable refLoc 0 0
     machRegistry      <- newTable refLoc 0 0
     machNextChunkId   <- liftIO (newIORef 0)
     machMetatablesRef <- liftIO (newIORef Map.empty)
     machLuaServer     <- liftIO newEmptyMVar
     machCServer       <- liftIO newEmptyMVar
     machGarbage       <- liftIO (newIORef [])
     machProfiling     <- liftIO newProfilingInfo
     machMainThreadRef <- allocNewThread refLoc
                                      machLuaServer machCServer ThreadRunning
     setTableRaw machRegistry (Number 1) (Thread machMainThreadRef)
     setTableRaw machRegistry (Number 2) (Table machGlobals)
     return MachineEnv { .. }

newProfilingInfo :: IO ProfilingInfo
newProfilingInfo =
  do profCallCounters  <- newIORef Map.empty
     profAllocCounters <- newIORef Map.empty
     profFunctionTimers <- newIORef Map.empty
     return ProfilingInfo { .. }

foreign import ccall "galua_allocate_luaState"
  allocateLuaState :: Ptr () -> IO (Ptr ())

foreign import ccall "&galua_free_luaState"
  freeLuaState :: FunPtr (Ptr () -> IO ())

newCPtr :: Ptr () -> IO (ForeignPtr ())
newCPtr initialToken =
  do luastate <- allocateLuaState initialToken
     when (nullPtr == luastate)
       (fail "lua_State allocation failed")
     newForeignPtr freeLuaState luastate

runMach :: VM -> Mach Void -> NextStep
runMach m (Mach f) = f m absurd

setTypeMetatable :: ValueType -> Maybe (Reference Table) -> Mach ()
setTypeMetatable typ mb =
  do metatableRef <- getsMachEnv machMetatablesRef
     liftIO $ modifyIORef' metatableRef $
       case mb of
         Just metatable -> Map.insert typ metatable
         Nothing        -> Map.delete typ


getTypeMetatable :: ValueType -> Mach (Maybe (Reference Table))
getTypeMetatable typ =
  do metatablesRef <- getsMachEnv machMetatablesRef
     metatables <- liftIO (readIORef metatablesRef)
     return (Map.lookup typ metatables)

luaError :: String -> Mach b
luaError str =
  do s <- liftIO (fromByteString (packUtf8 str))
     machThrow (String s)

machNewClosure ::
  FunctionValue -> Vector (IORef Value) -> Mach (Reference Closure)
machNewClosure fun upvals =
  do loc <- machRefLoc
     newClosure loc fun upvals

machNewTable ::
  Int {- ^ array size -} ->
  Int {- ^ hashtable size -} ->
  Mach (Reference Table)
machNewTable aSz hSz =
  do loc <- machRefLoc
     ref <- newTable loc aSz hSz
     counts <- getsMachEnv (profAllocCounters . machProfiling)
     liftIO $
        do incrementCounter ref counts
           return ref

machNewUserData :: ForeignPtr () -> Int -> Mach (Reference UserData)
machNewUserData fp n =
  do loc <- machRefLoc
     newUserData loc fp n

machNewThread :: Mach (Reference Thread)
machNewThread =
  do loc     <- machRefLoc
     luaMVar <- getsMachEnv machLuaServer
     cMVar   <- getsMachEnv machCServer
     allocNewThread loc luaMVar cMVar ThreadNew

data ExternalLuaState = ExternalLuaState
  { extLuaStateLuaServer   :: MVar CCallState
  , extLuaStateCServer     :: MVar CNextStep
  , extLuaStateThreadId    :: Int
  }

extToThreadRef :: NameM m => ExternalLuaState -> m (Reference Thread)
extToThreadRef ext =
  do mb <- lookupRef (extLuaStateThreadId ext)
     case mb of
       Nothing -> fail "extToThreadRef: Unexpectedly invalid weak thread reference"
       Just x -> return x


allocNewThread ::
  NameM m =>
  RefLoc ->
  MVar CCallState -> MVar CNextStep -> ThreadStatus -> m (Reference Thread)
allocNewThread refLoc luaMVar cMVar st =
  do ref <- newRef refLoc (error "thread uninitialized")
     liftIO $ do sptr <- newStablePtr ExternalLuaState
                           { extLuaStateLuaServer = luaMVar
                           , extLuaStateCServer   = cMVar
                           , extLuaStateThreadId  = referenceId ref
                           }

                 cptr <- newCPtr (castStablePtrToPtr sptr)
                 addRefFinalizer ref (freeStablePtr sptr)

                 eenv <- newThreadExecEnv
                 writeRef ref MkThread
                                { threadStatus = st
                                , threadCPtr   = cptr

                                , stExecEnv  = eenv
                                , stHandlers = []
                                , stStack    = Stack.empty
                                , stPC       = 0

                                }
     return ref

machCurrentEnv :: Mach MachineEnv
machCurrentEnv = getsVM vmMachineEnv

machCurrentThread :: Mach (Reference Thread)
machCurrentThread = getsVM vmCurThread

machBlockedThreads :: Mach (Stack (Reference Thread))
machBlockedThreads = getsVM vmBlocked

machIsMainThread :: Reference Thread -> Mach Bool
machIsMainThread t =
  do mainThread <- getsMachEnv machMainThreadRef
     return (t == mainThread)

-- | Build a closure for a function using the given value as its only upvalue
chunkToClosure ::
  MachineEnv                        ->
  Maybe String  {- ^ source name -}   ->
  ByteString    {- ^ function text -} ->
  Value         {- ^ first upvalue (i.e., "upvalues") -} ->
  Mach (Either String (Reference Closure))
chunkToClosure menv name bytes env =
  do res <- parser
     case res of
       Left e -> return (Left e)
       Right (Chunk numUpval func) ->
         do let initial 0 = env
                initial _ = Nil
                func' = propagateSources func
            upvalues <- liftIO (Vector.generateM numUpval (newIORef . initial))
            modNum   <- liftIO $ atomicModifyIORef' (machNextChunkId menv)
                               $ \r -> (r + 1, r)
            let fid = rootFun modNum
            clo <- machNewClosure (LuaFunction fid func') upvalues
            liftIO (machOnChunkLoad (machConfig menv) name bytes modNum func)
            return (Right clo)

  where
  luaSignature = "\x1bLua"
  bytesL = L.fromStrict bytes
  parser | luaSignature `B.isPrefixOf` bytes = return (parseLuaBytecode name bytesL)
         | otherwise                         = liftIO (parseLua name bytesL)

activateThread :: Reference Closure -> [Value] -> Reference Thread -> Mach ()
activateThread cRef vs tRef =
  setThreadStatus tRef (ThreadSuspended (FunCall cRef vs Nothing FunReturn))

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
       Right (ExitSuccess,chunk,_) -> return $! parseLuaBytecode mbName chunk

incrementCounter :: Reference a -> IORef (Map CodeLoc Int) -> IO ()
incrementCounter i countRef =
  atomicModifyIORef' countRef $ \counts ->
    let counts' = inline Map.alter inc (refLocSite (referenceLoc i)) counts
        inc old = Just $! maybe 1 succ old
    in (counts', counts' `seq` ())
