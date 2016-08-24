{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Galua.Stepper
  ( Interpreter
  , oneStep
  , runAllSteps
  ) where

import           Galua.Mach
import           Galua.Value
import           Galua.FunValue(funValueCode,FunCode(..))
import           Galua.Reference
import           Galua.CallIntoC
import           Galua.OpcodeInterpreter (execute)
import           Galua.LuaString

import           Galua.Util.Stack (Stack)
import qualified Galua.Util.Stack as Stack
import qualified Galua.Util.SizedVector as SV

import           Language.Lua.Bytecode (Function(..))

import           Control.Monad ((<=<))
import           Control.Monad.IO.Class
import           Control.Concurrent
import           Control.Exception (assert)
import           Data.IORef
import           Data.Foldable (traverse_)
import qualified Data.Map as Map
import qualified System.Clock as Clock

import           GHC.Exts (inline)

type Interpreter = VM -> NextStep -> Alloc (Either Value [Value])

oneStep :: VM -> NextStep -> Alloc VMState
oneStep vm instr = do
  case instr of
    PrimStep m        -> Running vm <$> m
    Goto pc           -> performGoto       vm pc
    FunCall f vs mb k -> performFunCall    vm f vs mb k
    FunTailcall f vs  -> performTailCall   vm f vs
    FunReturn vs      -> performFunReturn  vm vs
    ThreadExit vs     -> performThreadExit vm vs
    ThreadFail e      -> performThreadFail vm e
    ErrorReturn e     -> performErrorReturn vm e
    ThrowError e      -> performThrowError vm e
    Resume tRef k     -> performResume     vm tRef k
    Yield k           -> performYield      vm k
    ApiStart apiCall op k -> performApiStart vm apiCall op k
    ApiEnd label      -> performApiEnd vm label

performApiEnd :: VM -> ApiCall -> Alloc VMState
performApiEnd vm _apiCall = liftIO $
  do eenv <- stExecEnv <$> readRef (vmCurThread vm)
     ApiCallActive _ k <- readIORef (execApiCall eenv)
     writeIORef (execApiCall eenv) NoApiCall
     putMVar (machCServer (vmMachineEnv vm)) CResume
     return $ Running vm k

performApiStart :: VM -> ApiCall -> NextStep -> NextStep -> Alloc VMState
performApiStart vm apiCall next k = liftIO $
  do eenv <- stExecEnv <$> readRef (vmCurThread vm)
     writeIORef (execApiCall eenv) (ApiCallActive apiCall k)
     return $ Running vm next


performGoto :: VM -> Int -> Alloc VMState
performGoto vm pc =
  do vmUpdateThread vm $ \th -> th { stPC = pc }
     return (Running vm (runMach vm (execute pc)))


performTailCall ::
  VM                {- ^ current vm state -} ->
  Reference Closure {- ^ closure to enter -} ->
  [Value]           {- ^ arguments        -} ->
  Alloc VMState
performTailCall vm f vs =
  do liftIO (bumpCallCounter f vm)

     (newEnv, next) <- enterClosure f vs

     execEnv <- stExecEnv <$> readRef (vmCurThread vm)
     liftIO (recordProfTime (execCreateTime newEnv) (vmMachineEnv vm) execEnv)
     liftIO (recordProfEntry (vmMachineEnv vm) (funValueName (execFunction newEnv)))

     vmUpdateThread vm $ \th ->

       let elapsed = execCreateTime newEnv - execCreateTime (stExecEnv th)
       in th { stStack = addElapsedToTop elapsed (stStack th)
             , stExecEnv = newEnv
             }
     return (Running vm (next vm))

addElapsedToTop :: Clock.TimeSpec -> Stack StackFrame -> Stack StackFrame
addElapsedToTop elapsed stack =
  case Stack.pop stack of
    Just (CallFrame pc eenv err k,fs) ->
      Stack.push (CallFrame pc (addChildTime elapsed eenv) err k) fs
    Just (f,fs) -> Stack.push f (addElapsedToTop elapsed fs)
    Nothing     -> stack


performFunCall ::
  VM                    {- ^ current vm state       -} ->
  Reference Closure     {- ^ closure to enter       -} ->
  [Value]               {- ^ arguments              -} ->
  Maybe Handler         {- ^ optional error handler -} ->
  ([Value] -> NextStep) {- ^ return continuation    -} ->
  Alloc VMState
performFunCall vm f vs mb k =
  do liftIO (bumpCallCounter f vm)
     (newEnv, next) <- enterClosure f vs
     liftIO (recordProfEntry (vmMachineEnv vm)
                             (funValueName (execFunction newEnv)))

     vmUpdateThread vm $ \th ->
       let frame       = CallFrame (stPC th) (stExecEnv th) (fmap handlerK mb) k
       in th { stExecEnv  = newEnv
             , stHandlers = consMb (fmap handlerType mb) (stHandlers th)
             , stStack    = Stack.push frame (stStack th)
             }

     return $ Running vm (next vm)

bumpCallCounter :: Reference Closure -> VM -> IO ()
bumpCallCounter clo vm =
  do fun <- readRef clo
     atomicModifyIORef' prof $ \counts ->
       let key     = funValueName (cloFun fun)
           counts' = inline Map.alter inc key counts
           inc old = Just $! maybe 1 succ old
       in (counts', ())

  where prof = profCallCounters $ machProfiling $ vmMachineEnv vm


performThreadExit :: VM -> [Value] -> Alloc VMState
performThreadExit vm vs =
  case Stack.pop (vmBlocked vm) of
    Nothing      -> return (FinishedOk vs)
    Just (t, ts) ->
      do setThreadStatus (vmCurThread vm) ThreadNew
         vmSwitchToNormal vm{ vmCurThread = t, vmBlocked = ts }
                          (ThreadReturn vs)

-- | This function implements the logic for FunReturn. It ends execution
-- for the current execution environment and resumes the next environment
-- on the stack. In the case that the next frame is an error marker, it
-- begins unwinding the stack until a suitable handler is found.
performFunReturn :: VM -> [Value] -> Alloc VMState
performFunReturn vm vs =
  do th <- readRef (vmCurThread vm)

     now <- liftIO (Clock.getTime Clock.ProcessCPUTime)
     let elapsed = now - execCreateTime (stExecEnv th)
     liftIO (recordProfTime now (vmMachineEnv vm) (stExecEnv th))
     let stack' = addElapsedToTop elapsed (stStack th)

     case Stack.pop stack' of

       Nothing ->
          return (Running vm (ThreadExit vs))

       Just (f, fs) ->
         case f of
           ErrorFrame ->
                performErrorReturn vm (trimResult1 vs)

           CallFrame pc fenv errK k ->
             do vmUpdateThread vm $ \MkThread { .. } ->
                  MkThread { stExecEnv  = fenv
                           , stStack    = fs
                           , stPC       = pc
                           , stHandlers = case errK of
                                            Just _  -> tail stHandlers
                                            Nothing -> stHandlers
                           , ..
                           }

                return (Running vm (k vs))

recordProfEntry :: MachineEnv -> FunName -> IO ()
recordProfEntry menv funName =
  do let profRef    = profFunctionTimers (machProfiling menv)

         addCounter Nothing = Just $! FunctionRuntimes
           { runtimeIndividual = 0
           , runtimeCumulative = 0
           , runtimeCounter    = 1
           }

         addCounter (Just rts) = Just $! rts { runtimeCounter = runtimeCounter rts + 1 }

     atomicModifyIORef' profRef $ \prof ->
       (Map.alter addCounter funName prof, ())

recordProfTime :: Clock.TimeSpec -> MachineEnv -> ExecEnv -> IO ()
recordProfTime now menv eenv =
  do let curFunName = funValueName (execFunction eenv)
         profRef    = profFunctionTimers (machProfiling menv)
         elapsed    = now - execCreateTime eenv
         child      = execChildTime eenv


         addTimes rts = Just $! FunctionRuntimes
           { runtimeIndividual = runtimeIndividual rts + (elapsed - child)
           , runtimeCumulative = if runtimeCounter rts == 1
                                   then runtimeCumulative rts + elapsed
                                   else runtimeCumulative rts
           , runtimeCounter    = runtimeCounter    rts - 1
           }

     assert (0 <= child && child <= elapsed) $
       atomicModifyIORef' profRef $ \prof ->
         (Map.update addTimes curFunName prof, ())


addChildTime :: Clock.TimeSpec -> ExecEnv -> ExecEnv
addChildTime elapsed e =
  e { execChildTime = execChildTime e + elapsed }


performThreadFail :: VM -> Value -> Alloc VMState
performThreadFail vm e =
  do let ref = vmCurThread vm
     th <- readRef ref
     liftIO (abortApiCall (machCServer (vmMachineEnv vm)) (stExecEnv th))
     liftIO (abortReentryFrames (machCServer (vmMachineEnv vm)) (stStack th))
     case Stack.pop (vmBlocked vm) of

       Nothing -> return (FinishedWithError e)

       Just (t, ts) ->
         do setThreadStatus (vmCurThread vm) ThreadCrashed
            vmSwitchToNormal vm{ vmCurThread = t, vmBlocked = ts }
                             (ThreadError e)

performThrowError :: VM -> Value -> Alloc VMState
performThrowError vm e =
  do let ref = vmCurThread vm
     th <- readRef ref
     case stHandlers th of
       [] -> return (Running vm (ThreadFail e))

       FunHandler x : _ ->
         do vmUpdateThread vm $ \MkThread { .. } ->
              MkThread { stStack = Stack.push ErrorFrame stStack, .. }
            return (Running vm (FunTailcall x [e]))

       DefaultHandler : _ -> return (Running vm (ErrorReturn e))


abortReentryFrames :: MVar CNextStep -> Stack StackFrame -> IO ()
abortReentryFrames mvar = traverse_ $ \frame ->
  case frame of
    CallFrame _ eenv _ _ -> abortApiCall mvar eenv
    _                    -> return ()

abortApiCall :: MVar CNextStep -> ExecEnv -> IO ()
abortApiCall mvar eenv =
  do let ref = execApiCall eenv
     st <- readIORef ref
     case st of
       NoApiCall        -> return ()
       ApiCallAborted{} -> return ()
       ApiCallActive api _ ->
         do putMVar mvar CAbort
            writeIORef ref (ApiCallAborted api)

performResume ::
  VM ->
  Reference Thread ->
  (ThreadResult -> NextStep) ->
  Alloc VMState
performResume vm tRef finishK =
  do thread <- readRef tRef
     case threadStatus thread of
       ThreadSuspended resumeK ->
         do let oldThread = vmCurThread vm
            setThreadStatus oldThread (ThreadNormal finishK)
            setThreadStatus tRef      ThreadRunning
            let vm' = vm { vmCurThread = tRef
                         , vmBlocked   = Stack.push oldThread (vmBlocked vm)
                         }
            return (Running vm' resumeK)

       _ -> error "performResume: Thread not suspended"


performYield :: VM -> NextStep -> Alloc VMState
performYield vm k =
  do let ref = vmCurThread vm
     th <- readRef ref

     liftIO $
       do let apiRef = execApiCall (stExecEnv th)
          ApiCallActive _ _ <- readIORef apiRef
          writeIORef apiRef NoApiCall
          putMVar (machCServer (vmMachineEnv vm)) CAbort

     case Stack.pop (vmBlocked vm) of
       Nothing -> do str <- liftIO (fromByteString "yield to no one")
                     return $ Running vm $ ThrowError $ String str
       Just (t, ts) ->
         do setThreadStatus (vmCurThread vm) $ ThreadSuspended k
            vmSwitchToNormal vm { vmCurThread = t, vmBlocked = ts } ThreadYield


-- | Unwind the call stack until a handler is found or the stack becomes
-- empty. Resume execution in the error handler if one is found or finish
-- execution with the final error otherwise.
performErrorReturn :: VM -> Value -> Alloc VMState
performErrorReturn vm e =
  do let ref = vmCurThread vm
     th <- readRef ref
     liftIO (abortApiCall (machCServer (vmMachineEnv vm)) (stExecEnv th))
     unwind (stStack th)
  where
  unwind s =
    case Stack.pop s of
      Nothing -> return (FinishedWithError e)
      Just (frame, s') ->
        case frame of
          CallFrame pc fenv (Just k) _ ->
            do vmUpdateThread vm $ \MkThread { .. } ->
                 MkThread { stExecEnv  = fenv
                          , stHandlers = tail stHandlers
                          , stStack    = s'
                          , stPC       = pc
                          , ..
                          }

               return (Running vm (k e))

          CallFrame pc eenv Nothing _ ->
            do vmUpdateThread vm $ \t -> t
                          { stExecEnv = eenv
                          , stStack = s'
                          , stPC    = pc
                          }
               return (Running vm (ErrorReturn e))

          ErrorFrame -> unwind s'




runAllSteps :: Interpreter
runAllSteps vm i =
  do s <- oneStep vm i
     case s of
       FinishedOk vs       -> return (Right vs)
       FinishedWithError v -> return (Left v)
       Running v1 i1       -> runAllSteps v1 i1


vmSwitchToNormal :: VM -> ThreadResult -> Alloc VMState
vmSwitchToNormal vm res =
  do let ref = vmCurThread vm
     th <- readRef ref
     case threadStatus th of
       ThreadNormal k ->
         do setThreadStatus ref ThreadRunning
            return (Running vm (k res))

       _ -> error "[BUG] vmSwitchToNormal: not a normal thread."




consMb :: Maybe a -> [a] -> [a]
consMb Nothing xs = xs
consMb (Just x) xs = x : xs


enterClosure ::
  MonadIO m => Reference Closure -> [Value] -> m (ExecEnv, VM -> NextStep)
enterClosure c vs = liftIO $
  do MkClosure { cloFun, cloUpvalues } <- readRef c

     let (stackElts, vas, start) =
           case funValueCode cloFun of
             LuaOpCodes f ->
                  let n = funcMaxStackSize f
                      (normalArgs,extraArgs) = splitAt (funcNumParams f) vs

                      stack = take n $ normalArgs ++ repeat Nil

                      varargs
                        | funcIsVararg f = extraArgs
                        | otherwise      = []

                  in (stack, varargs, machGoto 0)

             CCode cfun -> (vs, [], execCFunction cfun)

     stack    <- SV.new
     traverse_ (SV.push stack <=< newIORef) stackElts
     vasRef   <- newIORef vas
     apiRef   <- newIORef NoApiCall
     time     <- Clock.getTime Clock.ProcessCPUTime

     let newEnv = ExecEnv { execStack    = stack
                          , execUpvals   = cloUpvalues
                          , execFunction = cloFun
                          , execVarargs  = vasRef
                          , execApiCall  = apiRef
                          , execClosure  = Closure c
                          , execCreateTime = time
                          , execChildTime  = 0
                          }

     return (newEnv, \vm -> runMach vm start)
