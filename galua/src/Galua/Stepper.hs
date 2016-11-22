{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Galua.Stepper
  ( oneStep
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
import           Control.Concurrent.STM (atomically, takeTMVar)
import           Control.Exception (assert)
import           Data.IORef
import           Data.Foldable (traverse_)
import qualified Data.Map as Map
import qualified System.Clock as Clock

import           GHC.Exts (inline)

data Cont r = Cont
  { running           :: VM -> NextStep -> Alloc r
  , runningInC        :: VM -> Alloc r
  , finishedOk        :: [Value] -> Alloc r
  , finishedWithError :: Value -> Alloc r
  }

oneStep :: VM -> NextStep -> Alloc VMState
oneStep = oneStepK
  Cont { running = \a b -> return $! Running a b
       , runningInC = \a -> return $! RunningInC a
       , finishedOk = \a -> return $! FinishedOk a
       , finishedWithError = \a -> return $! FinishedWithError a
       }

{-# INLINE oneStepK #-}
oneStepK :: Cont r -> VM -> NextStep -> Alloc r
oneStepK c vm instr = do
  case instr of
    PrimStep m          -> running c vm =<< m
    Goto pc             -> performGoto          c vm pc
    FunCall f vs mb k   -> performFunCall       c vm f vs mb k
    FunTailcall f vs    -> performTailCall      c vm f vs
    FunReturn vs        -> performFunReturn     c vm vs
    ThreadExit vs       -> performThreadExit    c vm vs
    ThreadFail e        -> performThreadFail    c vm e
    ErrorReturn e       -> performErrorReturn   c vm e
    ThrowError e        -> performThrowError    c vm e
    Resume tRef k       -> performResume        c vm tRef k
    Yield k             -> performYield         c vm k
    ApiStart apiCall op -> performApiStart      c vm apiCall op
    ApiEnd _ res next   -> performApiEnd        c vm res next
    WaitForC            -> runningInC           c vm
    Interrupt n         -> running              c vm n




performApiEnd ::
  Cont r ->
  VM                 {- ^ virtual machine state -} ->
  Maybe PrimArgument {- ^ C return value        -} ->
  NextStep           {- ^ next step             -} ->
  Alloc r
performApiEnd c vm res next =
  do liftIO $ do eenv <- stExecEnv <$> readRef (vmCurThread vm)
                 writeIORef (execApiCall eenv) NoApiCall
                 writeIORef (execLastResult eenv) res
                 putMVar (machCServer (vmMachineEnv vm)) CResume
     running c vm next

performApiStart :: Cont r -> VM -> ApiCall -> NextStep -> Alloc r
performApiStart c vm apiCall next =
  do liftIO $ do eenv <- stExecEnv <$> readRef (vmCurThread vm)
                 writeIORef (execApiCall eenv) (ApiCallActive apiCall)
     running c vm next

performGoto :: Cont r -> VM -> Int -> Alloc r
performGoto c vm pc =
  do vmUpdateThread vm $ \th -> th { stPC = pc }
     running c vm (runMach vm (execute pc))


{-# INLINE performTailCall #-}
performTailCall ::
  Cont r ->
  VM                {- ^ current vm state -} ->
  Reference Closure {- ^ closure to enter -} ->
  [Value]           {- ^ arguments        -} ->
  Alloc r
performTailCall c vm f vs =
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
     running c vm (next vm)

addElapsedToTop :: Clock.TimeSpec -> Stack StackFrame -> Stack StackFrame
addElapsedToTop elapsed stack =
  case Stack.pop stack of
    Just (CallFrame pc eenv err k,fs) ->
      Stack.push (CallFrame pc (addChildTime elapsed eenv) err k) fs
    Just (f,fs) -> Stack.push f (addElapsedToTop elapsed fs)
    Nothing     -> stack


{-# INLINE performFunCall #-}
performFunCall ::
  Cont r ->
  VM                    {- ^ current vm state       -} ->
  Reference Closure     {- ^ closure to enter       -} ->
  [Value]               {- ^ arguments              -} ->
  Maybe Handler         {- ^ optional error handler -} ->
  ([Value] -> NextStep) {- ^ return continuation    -} ->
  Alloc r
performFunCall c vm f vs mb k =
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

     running c vm (next vm)

bumpCallCounter :: Reference Closure -> VM -> IO ()
bumpCallCounter clo vm =
  do fun <- readRef clo
     atomicModifyIORef' prof $ \counts ->
       let key     = funValueName (cloFun fun)
           counts' = inline Map.alter inc key counts
           inc old = Just $! maybe 1 succ old
       in (counts', ())

  where prof = profCallCounters $ machProfiling $ vmMachineEnv vm


{-# INLINE performThreadExit #-}
performThreadExit :: Cont r -> VM -> [Value] -> Alloc r
performThreadExit c vm vs =
  case Stack.pop (vmBlocked vm) of
    Nothing      -> finishedOk c vs
    Just (t, ts) ->
      do setThreadStatus (vmCurThread vm) ThreadNew
         vmSwitchToNormal c vm{ vmCurThread = t, vmBlocked = ts }
                              (ThreadReturn vs)

-- | This function implements the logic for FunReturn. It ends execution
-- for the current execution environment and resumes the next environment
-- on the stack. In the case that the next frame is an error marker, it
-- begins unwinding the stack until a suitable handler is found.
{-# INLINE performFunReturn #-}
performFunReturn :: Cont r -> VM -> [Value] -> Alloc r
performFunReturn c vm vs =
  do th <- readRef (vmCurThread vm)

     now <- liftIO (Clock.getTime Clock.ProcessCPUTime)
     let elapsed = now - execCreateTime (stExecEnv th)
     liftIO (recordProfTime now (vmMachineEnv vm) (stExecEnv th))
     let stack' = addElapsedToTop elapsed (stStack th)

     case Stack.pop stack' of

       Nothing ->
          running c vm (ThreadExit vs)

       Just (f, fs) ->
         case f of
           ErrorFrame ->
                performErrorReturn c vm (trimResult1 vs)

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

                running c vm (k vs)

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


{-# LANGUAGE performThreadFail #-}
performThreadFail :: Cont r -> VM -> Value -> Alloc r
performThreadFail c vm e =
  do let ref = vmCurThread vm
     th <- readRef ref
     liftIO (abortApiCall (machCServer (vmMachineEnv vm)) (stExecEnv th))
     liftIO (abortReentryFrames (machCServer (vmMachineEnv vm)) (stStack th))
     case Stack.pop (vmBlocked vm) of

       Nothing -> finishedWithError c e

       Just (t, ts) ->
         do setThreadStatus (vmCurThread vm) ThreadCrashed
            vmSwitchToNormal c vm{ vmCurThread = t, vmBlocked = ts }
                               (ThreadError e)

performThrowError :: Cont r -> VM -> Value -> Alloc r
performThrowError c vm e =
  do let ref = vmCurThread vm
     th <- readRef ref
     case stHandlers th of
       [] -> running c vm (ThreadFail e)

       FunHandler x : _ ->
         do vmUpdateThread vm $ \MkThread { .. } ->
              MkThread { stStack = Stack.push ErrorFrame stStack, .. }
            running c vm (FunTailcall x [e])

       DefaultHandler : _ -> running c vm (ErrorReturn e)


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
       ApiCallActive api ->
         do putMVar mvar CAbort
            writeIORef ref (ApiCallAborted api)

performResume ::
  Cont r ->
  VM ->
  Reference Thread ->
  (ThreadResult -> NextStep) ->
  Alloc r
performResume c vm tRef finishK =
  do thread <- readRef tRef
     case threadStatus thread of
       ThreadSuspended resumeK ->
         do let oldThread = vmCurThread vm
            setThreadStatus oldThread (ThreadNormal finishK)
            setThreadStatus tRef      ThreadRunning
            let vm' = vm { vmCurThread = tRef
                         , vmBlocked   = Stack.push oldThread (vmBlocked vm)
                         }
            running c vm' resumeK

       _ -> error "performResume: Thread not suspended"


performYield :: Cont r -> VM -> NextStep -> Alloc r
performYield c vm k =
  do let ref = vmCurThread vm
     th <- readRef ref

     liftIO $
       do let apiRef = execApiCall (stExecEnv th)
          writeIORef apiRef NoApiCall
          putMVar (machCServer (vmMachineEnv vm)) CAbort

     case Stack.pop (vmBlocked vm) of
       Nothing -> do str <- liftIO (fromByteString "yield to no one")
                     running c vm (ThrowError (String str))
       Just (t, ts) ->
         do setThreadStatus (vmCurThread vm) $ ThreadSuspended k
            vmSwitchToNormal c vm{ vmCurThread = t, vmBlocked = ts } ThreadYield


-- | Unwind the call stack until a handler is found or the stack becomes
-- empty. Resume execution in the error handler if one is found or finish
-- execution with the final error otherwise.
{-# INLINE performErrorReturn #-}
performErrorReturn :: Cont r -> VM -> Value -> Alloc r
performErrorReturn c vm e =
  do let ref = vmCurThread vm
     th <- readRef ref
     liftIO (abortApiCall (machCServer (vmMachineEnv vm)) (stExecEnv th))
     unwind (stStack th)
  where
  unwind s =
    case Stack.pop s of
      Nothing -> finishedWithError c e
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

               running c vm (k e)

          CallFrame pc eenv Nothing _ ->
            do vmUpdateThread vm $ \t -> t
                          { stExecEnv = eenv
                          , stStack = s'
                          , stPC    = pc
                          }
               running c vm (ErrorReturn e)

          ErrorFrame -> unwind s'


runAllSteps :: VM -> NextStep -> Alloc (Either Value [Value])
runAllSteps vm i = oneStepK cont vm i
  where
  cont = Cont { running    = runAllSteps
              , runningInC = \v1 ->
                 do let luaMVar = machLuaServer (vmMachineEnv vm)
                    cResult <- liftIO (atomically (takeTMVar luaMVar))
                    runAllSteps v1 (runMach v1 (handleCCallState cResult))
              , finishedOk = \vs -> return (Right vs)
              , finishedWithError = \v -> return (Left v)
              }



vmSwitchToNormal :: Cont r -> VM -> ThreadResult -> Alloc r
vmSwitchToNormal c vm res =
  do let ref = vmCurThread vm
     th <- readRef ref
     case threadStatus th of
       ThreadNormal k ->
         do setThreadStatus ref ThreadRunning
            running c vm (k res)

       _ -> error "[BUG] vmSwitchToNormal: not a normal thread."




consMb :: Maybe a -> [a] -> [a]
consMb Nothing xs = xs
consMb (Just x) xs = x : xs


enterClosure ::
  MonadIO m => Reference Closure -> [Value] -> m (ExecEnv, VM -> NextStep)
enterClosure c vs = liftIO $
  do MkClosure { cloFun, cloUpvalues } <- readRef c

     let (stackElts, vas, start, code) =
           case funValueCode cloFun of
             LuaOpCodes f ->
                  let n = funcMaxStackSize f
                      (normalArgs,extraArgs) = splitAt (funcNumParams f) vs

                      stack = take n $ normalArgs ++ repeat Nil

                      varargs
                        | funcIsVararg f = extraArgs
                        | otherwise      = []

                  in (stack, varargs, machGoto 0, funcCode f)

             CCode cfun -> (vs, [], execCFunction cfun, mempty)

     stack    <- SV.new
     traverse_ (SV.push stack <=< newIORef) stackElts
     vasRef   <- newIORef vas
     apiRef   <- newIORef NoApiCall
     resultRef <- newIORef Nothing
     time     <- Clock.getTime Clock.ProcessCPUTime

     let newEnv = ExecEnv { execStack    = stack
                          , execUpvals   = cloUpvalues
                          , execFunction = cloFun
                          , execVarargs  = vasRef
                          , execApiCall  = apiRef
                          , execLastResult = resultRef
                          , execClosure  = Closure c
                          , execCreateTime = time
                          , execChildTime  = 0
                          , execInstructions  = code
                          }

     newEnv `seq` return (newEnv, \vm -> runMach vm start)
