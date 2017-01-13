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
import           Galua.CApi.Types

import qualified Galua.Util.Stack as Stack
import qualified Galua.Util.SizedVector as SV


import           Control.Monad ((<=<))
import           Data.IORef
import           Data.Foldable (traverse_)
import           Data.Traversable (for)
import           Foreign.Ptr
import           Foreign.C.Types


data Cont r = Cont
  { running           :: VM -> NextStep -> IO r
  , returnToC         :: CNextStep -> IO r
  }

{-# INLINE oneStep' #-}
oneStep' :: Cont r -> VM -> NextStep -> IO r
oneStep' !c = \ (!vm) instr ->
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
    Yield vs            -> performYield         c vm vs
    ApiStart apiCall op -> performApiStart      c vm apiCall op
    ApiEnd _            -> performApiEnd        c vm
    Interrupt n         -> running              c vm n


{-# INLINE performApiEnd #-}
performApiEnd ::
  Cont r ->
  VM                 {- ^ virtual machine state -} ->
  IO r
performApiEnd c vm =
  do let ref = execApiCall (vmCurExecEnv vm)
     writeIORef ref NoApiCall
     leavingRTS vm
     returnToC c CReturn


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
performTailCall a b c d = enterClosure a b c d


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

     case Stack.pop (vmBlocked vm) of

       Nothing ->
         fail "PANIC: the main thread exited with failure"

       Just (t, ts) ->
         do setThreadField threadStatus th ThreadCrashed
            newEnv <- getThreadField stExecEnv t
            vmSwitchToNormal c vm{ vmCurThread = t, vmBlocked = ts
                                 , vmCurExecEnv = newEnv }
                               (ThreadError e)

{-# INLINE performThrowError #-}
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




{-# INLINE performResume #-}
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
            (vm'', next) <- resumeK vm'
            running c vm'' next

       _ -> error "performResume: Thread not suspended"


{-# INLINE performYield #-}
performYield :: Cont r -> VM -> [Value] -> IO r
performYield c vm vs =
  do let eenv   = vmCurExecEnv vm
         apiRef = execApiCall eenv

     st <- readIORef apiRef
     case st of
       ApiCallActive api ->
         do writeIORef apiRef NoApiCall
            case Stack.pop (vmBlocked vm) of
              Nothing -> do str <- fromByteString "yield to no one"
                            running c vm (ThrowError (String str))
              Just (t, ts) ->
                do setThreadField threadStatus (vmCurThread vm)
                     (ThreadSuspended (restartFromYield (apiContinuation api)))
                   newEnv <- getThreadField stExecEnv t
                   traverse_ (SV.push (execStack newEnv) <=< newIORef) vs
                   let vm' = vm { vmCurThread = t
                                , vmBlocked = ts
                                , vmCurExecEnv = newEnv
                                }
                   leavingRTS vm'
                   returnToC c CYield
       _ -> fail "performYield: Not in API call?"


restartFromYield :: Maybe (Lua_KFunction, Lua_KContext) -> VM -> IO (VM,NextStep)
restartFromYield mbK vm =
  case mbK of
    Just (k,ctx) ->
      do leavingRTS vm
         callCK k luaYIELD ctx vm
    Nothing ->
      do let th = vmCurThread vm
         eenv <- getThreadField stExecEnv th
         xs <- stackToList (execStack eenv)
         return (vm, FunReturn xs)
  where
    stackToList :: SV.SizedVector (IORef a) -> IO [a]
    stackToList stack =
      do n  <- SV.size stack
         for [0 .. n-1 ] $ \i -> readIORef =<< SV.get stack i



-- | Unwind the call stack until a handler is found or the stack becomes
-- empty. Resume execution in the error handler if one is found or finish
-- execution with the final error otherwise.
{-# INLINE performErrorReturn #-}
performErrorReturn :: Cont r -> VM -> Value -> IO r
performErrorReturn c vm e =
  do let apiRef = execApiCall (vmCurExecEnv vm)
     st <- readIORef apiRef
     case st of
       NoApiCall        -> do (vm',next) <- doUnwind vm e
                              running c vm' next
       ApiCallAborted{} -> do (vm',next) <- doUnwind vm e
                              running c vm' next
       ApiCallErrorReturn {}->
          fail "PANIC: performErrorReturn, error while aborting API call"
       ApiCallYielding {}->
          fail "PANIC: performErrorReturn, yield while aborting API call"
       ApiCallActive api ->
         do writeIORef apiRef (ApiCallErrorReturn api e)
            leavingRTS vm
            returnToC c CError

doUnwind :: VM -> Value -> IO (VM, NextStep)
doUnwind vm e =
  do stack <- getThreadField stStack (vmCurThread vm)
     case Stack.pop stack of
       Nothing -> fail "PANIC: returned before returning from API call"
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
                next <- k e
                return (newVM, next)

           CallFrame pc eenv Nothing _ ->
             do let th = vmCurThread vm
                setThreadField stExecEnv th eenv
                setThreadField stStack   th s'
                setThreadPC              th pc

                let newVM = vm { vmCurExecEnv = eenv }
                return (newVM, ErrorReturn e)


runAllSteps :: VM -> NextStep -> IO CNextStep
runAllSteps = go
  where
  go = oneStep' cont
  cont = Cont
    { running           = go
    , returnToC         = return
    }

leavingRTS :: VM -> IO ()
leavingRTS vm = writeIORef (machVMRef (vmMachineEnv vm)) vm

vmSwitchToNormal :: Cont r -> VM -> ThreadResult -> IO r
vmSwitchToNormal c vm res = running c vm =<< vmSwitchToNormal' vm res

vmSwitchToNormal' :: VM -> ThreadResult -> IO NextStep
vmSwitchToNormal' vm res =
  do let th = vmCurThread vm
     st <- getThreadField threadStatus th
     case st of
       ThreadNormal k ->
         do setThreadField threadStatus th ThreadRunning
            k res

       ThreadSuspended{} -> fail ("PANIC: resuming suspended thread " ++ show (referenceId th))
       ThreadNew     -> fail ("PANIC: resuming new thread " ++ show (referenceId th))
       ThreadRunning -> fail ("PANIC: resuming running thread " ++ show (referenceId th))
       ThreadCrashed -> fail ("PANIC: resuming crashed thread " ++ show (referenceId th))



consMb :: Maybe a -> [a] -> [a]
consMb Nothing xs = xs
consMb (Just x) xs = x : xs


{-# INLINE enterClosure #-}
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

            leavingRTS newVM
            (vm', next) <- callC (cfunAddr cfun) newVM
            running c vm' next


foreign import ccall "galua_call_c" call_c :: CFun -> Ptr () -> IO CInt
foreign import ccall "galua_call_c_k" call_c_k ::
  Lua_KFunction -> CInt -> Lua_KContext -> Ptr () -> IO CInt

callC :: CFun -> VM -> IO (VM,NextStep)
callC f = callC' (call_c f)

callCK :: Lua_KFunction -> CInt -> Lua_KContext -> VM -> IO (VM,NextStep)
callCK f st ctx = callC' (call_c_k f st ctx)

callC' :: (Ptr () -> IO CInt) -> VM -> IO (VM,NextStep)
callC' f vm =
  do let l = threadCPtr (vmCurThread vm)

     res <- f l
     vm' <- readIORef (machVMRef (vmMachineEnv vm))

     case res of
       -1 -> errorReturnFromC vm'
       -2 -> yieldReturnFromC vm'
       -3 -> fail "C functions called from Lua must return non-negative number"
       _ | res < 0   -> fail "Panic: capi_entry had invalid return value"
         | otherwise -> normalReturnFromC vm' (fromIntegral res)
           -- This does not reload the VM, because executing in C
           -- should not change anything important: it should
           -- just modify the values in the registers which
           -- are accessed through references.

normalReturnFromC ::
  VM -> Int {- ^ number of values returned -} -> IO (VM,NextStep)
normalReturnFromC vm n =
  do stack   <- execStack <$> getThreadField stExecEnv (vmCurThread vm)
     sz      <- SV.size stack
     results <- for [ sz - n .. sz - 1 ] $ \i ->
                  readIORef =<< SV.get stack i
     return (vm, FunReturn results)

errorReturnFromC :: VM -> IO (VM,NextStep)
errorReturnFromC vm =
  do let ref = execApiCall (vmCurExecEnv vm)
     st <- readIORef ref
     case st of
       ApiCallErrorReturn api e ->
         do writeIORef ref (ApiCallAborted api)
            doUnwind vm e
       _ -> fail ("PANIC: errorReturnFromC expected API call state to be aborting, "
                      ++ showApiCallStatus st)

yieldReturnFromC :: VM -> IO (VM,NextStep)
yieldReturnFromC vm = do next <- vmSwitchToNormal' vm ThreadYield
                         return (vm,next)
