{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module Galua.Stepper
  ( runAllSteps
  , oneStep', Cont(..)
  ) where

import           Galua.Mach
import           Galua.Value
import           Galua.FunValue(funValueCode,FunCode(..))
import           Galua.OpcodeInterpreter (execute)
import           Galua.LuaString
import           Galua.Code
import           Galua.CObjInfo

import           Galua.Util.Stack (Stack)
import qualified Galua.Util.Stack as Stack
import qualified Galua.Util.SizedVector as SV


import           Control.Monad ((<=<))
import           Control.Concurrent
import           Control.Concurrent.STM (atomically, takeTMVar)
import           Data.IORef
import           Data.Foldable (traverse_)
import           Data.Traversable (for)
import           Data.Maybe (fromMaybe)
import           Foreign.Ptr
import           Foreign.C.Types
import           System.IO


data Cont r = Cont
  { running           :: VM -> NextStep -> IO r
  , exitToC           :: CNextStep -> IO r
  , finishedWithError :: Value -> IO r
  }

{-# INLINE oneStep' #-}
oneStep' :: Cont r -> VM -> NextStep -> IO r
oneStep' c !vm instr =
  case instr of
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
    ApiEnd _            -> performApiEnd        c vm
    Interrupt n         -> running              c vm n


{-# INLINE performApiEnd #-}
performApiEnd ::
  Cont r ->
  VM                 {- ^ virtual machine state -} ->
  IO r
performApiEnd c vm =
  do let eenv = vmCurExecEnv vm
     writeIORef (execApiCall eenv) NoApiCall
     writeIORef (machVMRef (vmMachineEnv vm)) vm
     exitToC c CResume


{-# INLINE performApiStart #-}
performApiStart :: Cont r -> VM -> ApiCall -> IO NextStep -> IO r
performApiStart c vm apiCall next =
  do let eenv = vmCurExecEnv vm
     writeIORef (execApiCall eenv) (ApiCallActive apiCall)
     running c vm =<< next

{-# INLINE performGoto #-}
performGoto :: Cont r -> VM -> Int -> IO r
performGoto c vm pc =
  do setThreadPC (vmCurThread vm) pc
     running c vm =<< execute vm pc

{-# INLINE performTailCall #-}
performTailCall ::
  Cont r ->
  VM                {- ^ current vm state -} ->
  Reference Closure {- ^ closure to enter -} ->
  [Value]           {- ^ arguments        -} ->
  IO r
performTailCall = enterClosure


{-# INLINE performFunCall #-}
performFunCall ::
  Cont r ->
  VM                    {- ^ current vm state       -} ->
  Reference Closure     {- ^ closure to enter       -} ->
  [Value]               {- ^ arguments              -} ->
  Maybe Handler         {- ^ optional error handler -} ->
  ([Value] -> IO NextStep) {- ^ return continuation    -} ->
  IO r
performFunCall c vm f vs mb k =
  do let th = vmCurThread vm

     pc       <- getThreadPC th
     let eenv = vmCurExecEnv vm
     stack    <- getThreadField stStack th
     handlers <- getThreadField stHandlers th

     let frame = CallFrame pc eenv (fmap handlerK mb) k

     setThreadField stHandlers th (consMb (fmap handlerType mb) handlers)
     setThreadField stStack th (Stack.push frame stack)

     enterClosure c vm f vs



performThreadExit :: Cont r -> VM -> [Value] -> IO r
performThreadExit c vm vs =
  case Stack.pop (vmBlocked vm) of
    Nothing      -> fail "PANIC: Last stack frame disappeared."
    Just (t, ts) ->
      do setThreadField threadStatus (vmCurThread vm) ThreadNew
         eenv <- getThreadField stExecEnv t
         vmSwitchToNormal c vm{ vmCurThread = t, vmBlocked = ts
                              , vmCurExecEnv = eenv }
                              (ThreadReturn vs)

-- | This function implements the logic for FunReturn. It ends execution
-- for the current execution environment and resumes the next environment
-- on the stack. In the case that the next frame is an error marker, it
-- begins unwinding the stack until a suitable handler is found.
{-# INLINE performFunReturn #-}
performFunReturn :: Cont r -> VM -> [Value] -> IO r
performFunReturn c vm vs =
  do let th = vmCurThread vm

     stack <- getThreadField stStack th

     case Stack.pop stack of

       Nothing ->
          running c vm (ThreadExit vs)

       Just (CallFrame pc fenv errK k, fs) ->
         do setThreadField stExecEnv  th fenv
            setThreadField stStack    th fs
            setThreadPC               th pc

            handlers <- getThreadField stHandlers th
            setThreadField stHandlers th (case errK of
                                            Just _  -> tail handlers
                                            Nothing -> handlers)

            running c vm { vmCurExecEnv = fenv } =<< k vs



{-# INLINE performThreadFail #-}
performThreadFail :: Cont r -> VM -> Value -> IO r
performThreadFail c vm e =
  do let th = vmCurThread vm
         eenv = vmCurExecEnv vm
     stack <- getThreadField stStack th

     -- abortReentryFrames stack
     case Stack.pop (vmBlocked vm) of

       Nothing -> finishedWithError c e

       Just (t, ts) ->
         do setThreadField threadStatus th ThreadCrashed
            newEnv <- getThreadField stExecEnv t
            vmSwitchToNormal c vm{ vmCurThread = t, vmBlocked = ts
                                 , vmCurExecEnv = newEnv }
                               (ThreadError e)

performThrowError :: Cont r -> VM -> Value -> IO r
performThrowError c vm e =
  do let th = vmCurThread vm
     handlers <- getThreadField stHandlers th
     case handlers of

       FunHandler f : _ ->
         do pc       <- getThreadPC th
            let eenv = vmCurExecEnv vm
            stack    <- getThreadField stStack th

            let frame = CallFrame pc eenv Nothing
                          (return . ErrorReturn . trimResult1)

            setThreadField stStack th (Stack.push frame stack)

            enterClosure c vm f [e]

       _ -> running c vm (ErrorReturn e)




performResume ::
  Cont r ->
  VM ->
  Reference Thread ->
  (ThreadResult -> IO NextStep) ->
  IO r
performResume c vm tRef finishK =
  do st <- getThreadField threadStatus tRef
     case st of
       ThreadSuspended resumeK ->
         do let oldThread = vmCurThread vm
            setThreadField threadStatus oldThread (ThreadNormal finishK)
            setThreadField threadStatus tRef      ThreadRunning
            newEnv <- getThreadField stExecEnv tRef
            let vm' = vm { vmCurThread = tRef
                         , vmBlocked   = Stack.push oldThread (vmBlocked vm)
                         , vmCurExecEnv = newEnv
                         }
            running c vm' =<< resumeK

       _ -> error "performResume: Thread not suspended"


performYield :: Cont r -> VM -> IO NextStep -> IO r
performYield c vm k =
  do let eenv   = vmCurExecEnv vm
         apiRef = execApiCall eenv
     writeIORef apiRef NoApiCall

     fail "yield not updated for new execution path"
     -- putMVar (machCServer (vmMachineEnv vm)) CAbort

     case Stack.pop (vmBlocked vm) of
       Nothing -> do str <- fromByteString "yield to no one"
                     running c vm (ThrowError (String str))
       Just (t, ts) ->
         do setThreadField threadStatus (vmCurThread vm) (ThreadSuspended k)
            newEnv <- getThreadField stExecEnv t
            vmSwitchToNormal c vm { vmCurThread = t
                                  , vmBlocked = ts
                                  , vmCurExecEnv = newEnv
                                  } ThreadYield



-- | Unwind the call stack until a handler is found or the stack becomes
-- empty. Resume execution in the error handler if one is found or finish
-- execution with the final error otherwise.
{-# INLINE performErrorReturn #-}
performErrorReturn :: Cont r -> VM -> Value -> IO r
performErrorReturn c vm e =
  do let apiRef = execApiCall (vmCurExecEnv vm)
     st <- readIORef apiRef
     case st of
       NoApiCall        -> doUnwind c vm e
       ApiCallAborted{} -> doUnwind c vm e
       ApiCallErrorReturn {}->
          fail "PANIC: performErrorReturn, error while aborting API call"
       ApiCallActive api ->
         do writeIORef apiRef (ApiCallErrorReturn api e)
            writeIORef (machVMRef (vmMachineEnv vm)) vm
            exitToC c CAbort


doUnwind :: Cont r -> VM -> Value -> IO r
doUnwind c vm e =
  do stack <- getThreadField stStack (vmCurThread vm)
     case Stack.pop stack of
       Nothing -> finishedWithError c e
       Just (frame, s') ->
         case frame of
           CallFrame pc fenv (Just k) _ ->
             do let th = vmCurThread vm
                setThreadField stExecEnv  th fenv
                setThreadField stHandlers th . tail
                                        =<< getThreadField stHandlers th
                setThreadField stStack    th s'
                setThreadPC               th pc

                let newVM = vm { vmCurExecEnv = fenv }
                running c newVM =<< k e

           CallFrame pc eenv Nothing _ ->
             do let th = vmCurThread vm
                setThreadField stExecEnv th eenv
                setThreadField stStack   th s'
                setThreadPC              th pc

                let newVM = vm { vmCurExecEnv = eenv }
                running c newVM (ErrorReturn e)


runAllSteps :: VM -> NextStep -> IO CNextStep
runAllSteps = oneStep' cont
  where
  cont = Cont
    { running           = oneStep' cont
    , exitToC           = return
    , finishedWithError = \v -> return CAbort -- Bad error
    }


vmSwitchToNormal :: Cont r -> VM -> ThreadResult -> IO r
vmSwitchToNormal c vm res =
  do let th = vmCurThread vm
     st <- getThreadField threadStatus th
     case st of
       ThreadNormal k ->
         do setThreadField threadStatus th ThreadRunning
            running c vm =<< k res

       _ -> error "[BUG] vmSwitchToNormal: not a normal thread."




consMb :: Maybe a -> [a] -> [a]
consMb Nothing xs = xs
consMb (Just x) xs = x : xs


enterClosure ::
  Cont r ->
  VM ->
  Reference Closure ->
  [Value] ->
  IO r
enterClosure c vm clos vs =
  do let MkClosure { cloFun, cloUpvalues } = referenceVal clos
         th = vmCurThread vm

     case funValueCode cloFun of
       LuaOpCodes f ->
         do let n = funcMaxStackSize f
                (normalArgs,extraArgs) = splitAt (funcNumParams f) vs

                varargs
                  | funcIsVararg f = extraArgs
                  | otherwise      = []

            stack    <- SV.new
            traverse_ (SV.push stack <=< newIORef) (take n (normalArgs ++ repeat Nil))
            vasRef   <- newIORef varargs
            apiRef   <- newIORef NoApiCall

            let newVM  = vm { vmCurExecEnv = newEnv }
                newEnv = ExecEnv { execStack    = stack
                                 , execUpvals   = cloUpvalues
                                 , execFunction = cloFun
                                 , execVarargs  = vasRef
                                 , execApiCall  = apiRef
                                 , execClosure  = Closure clos
                                 , execInstructions  = funcCode f
                                 }

            setThreadField stExecEnv th newEnv

            running c newVM (Goto 0)

       CCode cfun ->
         do stack    <- SV.new
            traverse_ (SV.push stack <=< newIORef) vs
            vasRef   <- newIORef []
            apiRef   <- newIORef NoApiCall

            let newVM  = vm { vmCurExecEnv = newEnv }
                newEnv = ExecEnv { execStack    = stack
                                 , execUpvals   = cloUpvalues
                                 , execFunction = cloFun
                                 , execVarargs  = vasRef
                                 , execApiCall  = apiRef
                                 , execClosure  = Closure clos
                                 , execInstructions  = mempty
                                 }

            setThreadField stExecEnv th newEnv

            callC c newVM (cfunAddr cfun)

callC :: Cont r -> VM -> CFun -> IO r
callC c vm cfun =
  do let l = threadCPtr (vmCurThread vm)

     getFunInfo <- cfunInfoFun -- :: IO (FunPtr () -> IO CObjInfo)
     objInfo    <- getFunInfo (castFunPtr cfun)
     let name = fromMaybe "unknown" (cObjName objInfo)

     writeIORef (machVMRef (vmMachineEnv vm)) vm
     res <- call_c cfun l

     case res of
       -1 -> abortedReturnFromC c vm
       -2 -> fail "C functions called from Lua must return non-negative number"
       _ | res < 0   -> fail "Panic: capi_entry had invalid return value"
         | otherwise -> normalReturnFromC c vm (fromIntegral res)

normalReturnFromC ::
  Cont r -> VM -> Int {- ^ number of values returned -} -> IO r
normalReturnFromC c vm n =
  do stack   <- execStack <$> getThreadField stExecEnv (vmCurThread vm)
     sz      <- SV.size stack
     results <- for [ sz - n .. sz - 1 ] $ \i ->
                  readIORef =<< SV.get stack i
     running c vm (FunReturn results)

abortedReturnFromC :: Cont r -> VM -> IO r
abortedReturnFromC c vm =
  do let ref = execApiCall (vmCurExecEnv vm)
     st <- readIORef ref
     case st of
       ApiCallErrorReturn api e ->
         do writeIORef ref (ApiCallAborted api)
            doUnwind c vm e
       _ -> fail "PANIC: abortedReturnFromC expected API call state to be aborting"

foreign import ccall "galua_call_c" call_c ::
  CFun -> Ptr () -> IO CInt
