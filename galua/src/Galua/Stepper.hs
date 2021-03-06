{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module Galua.Stepper
  ( oneStep
  , runAllSteps
  , oneStep', Cont(..)
  ) where

import           Control.Concurrent
import           Control.Concurrent.STM (atomically, takeTMVar)
import           Data.IORef
import           Data.Foldable (traverse_)
import qualified Data.Vector.Mutable as IOVector
import qualified Data.Map as Map
import           Foreign.ForeignPtr



import           Galua.Mach
import           Galua.MachUtils(VMState(..))
import           Galua.Value
import           Galua.FunValue(FunctionValue(..))
import           Galua.CallIntoC
import           Galua.OpcodeInterpreter (execute)
import           Galua.LuaString
import           Galua.Code
import           Galua.Micro.JIT(jit)

import           Galua.Util.Stack (Stack)
import qualified Galua.Util.Stack as Stack
import qualified Galua.Util.SizedVector as SV
import           Galua.Util.SmallVec(SmallVec)
import qualified Galua.Util.SmallVec as SMV
import           Galua.Util.IOURef(readIOWordRef,writeIOWordRef)


data Cont r = Cont
  { running           :: VM -> NextStep -> IO r
  , runningInC        :: VM -> IO r
  , finishedOk        :: SmallVec Value -> IO r
  , finishedWithError :: Value -> IO r
  }

oneStep :: VM -> NextStep -> IO VMState
oneStep = oneStep'
  Cont { running = \a b -> return $! Running a b
       , runningInC = \a -> return $! RunningInC a
       , finishedOk = \a -> return $! FinishedOk a
       , finishedWithError = \a -> return $! FinishedWithError a
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
    WaitForC            -> runningInC           c vm
    Interrupt n         -> running              c vm n


{-# INLINE performApiEnd #-}
performApiEnd ::
  Cont r ->
  VM                 {- ^ virtual machine state -} ->
  IO r
performApiEnd c vm =
  do writeIORef (execApiCall (vmCurExecEnv vm)) NoApiCall
     putMVar (machCServer (vmMachineEnv vm)) CResume
     running c vm WaitForC

{-# INLINE performApiStart #-}
performApiStart :: Cont r -> VM -> ApiCall -> IO NextStep -> IO r
performApiStart c vm apiCall next =
  do writeIORef (execApiCall (vmCurExecEnv vm)) (ApiCallActive apiCall)
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
  (SmallVec Value)    {- ^ arguments        -} ->
  IO r
performTailCall c vm f vs =
  do (newEnv, start) <- enterClosure vm f vs

     let th = vmCurThread vm

     setThreadField stExecEnv th newEnv
     let newVM = vm { vmCurExecEnv = newEnv }

     running c newVM =<< start


{-# INLINE performFunCall #-}
performFunCall ::
  Cont r ->
  VM                    {- ^ current vm state       -} ->
  Reference Closure     {- ^ closure to enter       -} ->
  (SmallVec Value)        {- ^ arguments              -} ->
  Maybe Handler         {- ^ optional error handler -} ->
  (SmallVec Value -> IO NextStep) {- ^ return continuation    -} ->
  IO r
performFunCall c vm f vs mb k =
  do (newEnv, start) <- enterClosure vm f vs

     let th = vmCurThread vm

     pc       <- getThreadPC th
     let eenv = vmCurExecEnv vm
     stack    <- getThreadField stStack th
     handlers <- getThreadField stHandlers th

     let frame = CallFrame pc eenv (fmap handlerK mb) k

     setThreadField stExecEnv th newEnv
     setThreadField stHandlers th (consMb (fmap handlerType mb) handlers)
     frame `seq` setThreadField stStack th (Stack.push frame stack)

     let newVM = vm { vmCurExecEnv = newEnv }
     running c newVM =<< start



performThreadExit :: Cont r -> VM -> SmallVec Value -> IO r
performThreadExit c vm vs =
  case Stack.pop (vmBlocked vm) of
    Nothing      -> finishedOk c vs
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
performFunReturn :: Cont r -> VM -> SmallVec Value -> IO r
performFunReturn c vm vs =
  do let th = vmCurThread vm

     stack <- getThreadField stStack th

     case Stack.pop stack of

       Nothing ->
          running c vm (ThreadExit vs)

       Just (f, fs) ->
         case f of
           ErrorFrame ->
                performErrorReturn c vm (trimResult1 vs)

           CallFrame pc fenv errK k ->
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

     abortApiCall (machCServer (vmMachineEnv vm)) eenv
     abortReentryFrames (machCServer (vmMachineEnv vm)) stack
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
       [] -> running c vm (ThreadFail e)

       FunHandler x : _ ->
         do stack <- getThreadField stStack th
            setThreadField stStack th (Stack.push ErrorFrame stack)
            running c vm (FunTailcall x (SMV.vec1 e))

       DefaultHandler : _ -> running c vm (ErrorReturn e)


abortReentryFrames :: MVar CNextStep -> Stack StackFrame -> IO ()
abortReentryFrames mvar = traverse_ $ \frame ->
  case frame of
    CallFrame _ eenv _ _ -> abortApiCall mvar eenv
    _                    -> return ()

abortApiCall :: MVar CNextStep -> ExecEnv -> IO ()
abortApiCall mvar eenv =
  case eenv of
    ExecInLua {} -> return ()
    ExecInHs {}  -> return ()
    ExecInC cenv ->
     do let ref = cExecApiCall cenv
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
     putMVar (machCServer (vmMachineEnv vm)) CAbort

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
  do let ref = vmCurThread vm
         eenv = vmCurExecEnv vm

     abortApiCall (machCServer (vmMachineEnv vm)) eenv

     stack <- getThreadField stStack ref
     unwind stack
  where
  unwind s =
    case Stack.pop s of
      Nothing -> finishedWithError c e
      Just (frame, s') ->
        case frame of
          CallFrame pc fenv (Just k) _ ->
            do let th = vmCurThread vm
               setThreadField stExecEnv  th fenv
               setThreadField stHandlers th . tail =<< getThreadField stHandlers th
               setThreadField stStack    th s'
               setThreadPC               th pc

               running c vm { vmCurExecEnv = fenv } =<< k e

          CallFrame pc eenv Nothing _ ->
            do let th = vmCurThread vm
               setThreadField stExecEnv th eenv
               setThreadField stStack   th s'
               setThreadPC              th pc

               running c vm { vmCurExecEnv = eenv } (ErrorReturn e)

          ErrorFrame -> unwind s'


runAllSteps :: VM -> NextStep -> IO (Either Value (SmallVec Value))
runAllSteps !vm i = oneStep' cont vm i
  where
  cont = Cont { running    = runAllSteps
              , runningInC = \v1 ->
                 do let luaMVar = machLuaServer (vmMachineEnv vm)
                    cResult <- atomically (takeTMVar luaMVar)
                    next <- handleCCallState v1 cResult
                    runAllSteps v1 next
              , finishedOk = \vs -> return (Right vs)
              , finishedWithError = \v -> return (Left v)
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


{-# INLINE enterClosure #-}
enterClosure ::
  VM -> Reference Closure -> SmallVec Value -> IO (ExecEnv, IO NextStep)
enterClosure vm c vs =
  do let MkClosure { cloFun, cloUpvalues, cloCounter } = referenceVal c
     case cloFun of

       LuaFunction fid f ->
         do entries <- readIOWordRef cloCounter
            -- Only compile functions that get called many times.
            -- This does not help with functions, like the "main" event loop
            -- which are not called than many time, but have loops inside
            -- them...
            if False -- entries >= 0 -- 5000
               then jitLua fid f cloUpvalues
               else do writeIOWordRef cloCounter (entries + 1)
                       interpLua fid f cloUpvalues

       CFunction cfun ->
         do stack <- SV.new
            SMV.forM_ vs (SV.push stack)
            apiRef   <- newIORef NoApiCall

            let eenv = ExecInC CExecEnv
                         { cExecStack     = stack
                         , cExecUpvals    = cloUpvalues
                         , cExecApiCall   = apiRef
                         , cExecClosure   = Closure c
                         , cExecFunction  = cfun
                         }


                fpL   = threadCPtr (referenceVal (vmCurThread vm))
                cServ = machCServer (vmMachineEnv vm)

            eenv `seq` fpL `seq` cServ `seq`
              return (eenv, withForeignPtr fpL $ \l ->
                                                  execCFunction l cServ cfun)

  where
  jitLua fid f cloUpvalues =
    do let jitRef = machJIT (vmMachineEnv vm)
       jitMap <- readIORef jitRef
       compiled <- case Map.lookup fid jitMap of
                     Just code -> return code
                     Nothing ->
                       do putStrLn ("Source: " ++ show (funcSource f) ++
                                         show (funcLineDefined f) ++ "--" ++
                                         show (funcLastLineDefined f))

                          let menv = vmMachineEnv vm
                              globalTable = machGlobals menv
                          metas <- readIORef (machMetatablesRef menv)
                          code  <- jit metas globalTable fid c f
                          writeIORef jitRef $! Map.insert fid code jitMap
                          return code

       let eenv = ExecInHs HsExecEnv
                    { hsExecUpvals     = cloUpvalues
                    , hsExecClosure   = Closure  c
                    , hsExecFID       = fid
                    , hsExecFunction  = f
                    }

       eenv `seq` return (eenv, compiled c vm vs)


  interpLua fid f cloUpvalues =
    do let regNum = funcMaxStackSize f
           varargs
             | funcIsVararg f = drop (funcNumParams f) (SMV.toList vs)
             | otherwise      = []


       stack <- IOVector.new regNum
       SMV.ipadForM_ vs regNum Nil $ \i v ->
          IOVector.unsafeWrite stack i =<< newIORef v

       vasRef   <- newIORef varargs
       vrsRef   <- newIORef NoVarResults

       let eenv = ExecInLua LuaExecEnv
                    { luaExecRegs      = stack
                    , luaExecUpvals    = cloUpvalues
                    , luaExecVarargs   = vasRef
                    , luaExecVarress   = vrsRef
                    , luaExecCode      = funcCode f
                    , luaExecClosure   = Closure  c
                    , luaExecFID       = fid
                    , luaExecFunction  = f
                    }

       eenv `seq` return (eenv, return (Goto 0))

