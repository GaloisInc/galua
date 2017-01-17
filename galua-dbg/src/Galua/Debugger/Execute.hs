{-# LANGUAGE NamedFieldPuns #-}
module Galua.Debugger.Execute where

import           Data.Maybe(fromMaybe)
import qualified Data.Map as Map
import           Data.IORef

import           Control.Monad(when)
import           Control.Concurrent

import Galua.Util.IOURef(readIOURef,newIOURef)
import Galua.Value(valueBool,trimResult1)
import Galua.FunValue
import Galua.Mach
import Galua.Stepper
import Galua.Code(lookupLineNumber)

import Galua.Debugger.Types
import Galua.Debugger.NameHarness
import Galua.Debugger.CommandQueue
import Galua.Debugger.Options


handleAPICall :: ExecState -> VM -> NextStep -> IO CNextStep
handleAPICall dbg vm next =
  do mode <- readIORef (dbgStepMode dbg)
     stepRun dbg vm next (nextMode vm next mode)


newExecState :: Options -> IO ExecState
newExecState opts =
  do dbgPaused       <- newEmptyMVar
     dbgResume       <- newEmptyMVar
     dbgIdleReason   <- newIORef Ready
     dbgStepMode     <- newIORef Run
     dbgBreaks       <- newIORef Map.empty
     dbgBreakOnError <- newIOURef (optBreakOnError opts)
     dbgCommand      <- newCommandQueue
     dbgClients      <- newIORef []
     return $! ExecState { dbgPaused, dbgResume, dbgIdleReason
                         , dbgStepMode
                         , dbgBreaks, dbgBreakOnError
                         , dbgCommand, dbgClients
                         }


stepRun :: ExecState -> VM -> NextStep -> StepMode -> IO CNextStep
stepRun dbg vm0 next0 mode0 =
  do when callingC (writeIORef (dbgStepMode dbg) mode0)
     oneStep' Cont { returnToC, running } vm0 next0
  where
  callingC = nextCallsC next0

  returnToC c =
    do writeIORef (dbgStepMode dbg) mode0 -- or the next one?
       return c

  running vm next =
    do mode <- do oldMode <- if callingC
                               then readIORef (dbgStepMode dbg)
                               else return mode0
                  return $! nextMode vm next oldMode


       checkStopError dbg vm next $
         if not (mayPauseBefore next)
           then stepRun dbg vm next mode
           else checkStop dbg vm next mode $
                checkBreakPoint dbg vm next mode $
                do mb <- peekCmd (dbgCommand dbg)
                   case mb of
                     Nothing      -> stepRun dbg vm next mode
                     Just command ->
                       do mbNewMode <- handleCommand dbg False command
                          let newMode = fromMaybe mode mbNewMode
                          -- external stop directive and we're already
                          -- at a safe point
                          checkStop dbg vm next newMode $
                            stepRun dbg vm next newMode


handleCommand :: ExecState -> Bool -> DebuggerCommand -> IO (Maybe StepMode)
handleCommand dbg isIdle cmd =
  case cmd of
    WhenIdle io        -> Nothing <$ when isIdle io
    TemporaryStop m    -> Nothing <$ m
    AddClient client   -> Nothing <$ modifyIORef (dbgClients dbg) (client :)
    StartExec m client -> Just m  <$ modifyIORef (dbgClients dbg) (client :)




type K = IO CNextStep -> IO CNextStep

checkStop :: ExecState -> VM -> NextStep -> StepMode -> K
checkStop dbg vm next mode k =
  case mode of
    Stop -> doPause dbg vm next Ready
    _    -> k

checkStopError :: ExecState -> VM -> NextStep -> K
checkStopError dbg vm next k =
  case next of
    ThrowError e ->
      do stopOnErr <- readIOURef (dbgBreakOnError dbg)
         if stopOnErr
            then doPause dbg vm next (ThrowingError e)
            else k
    _ -> k


checkBreakPoint :: ExecState -> VM -> NextStep -> StepMode -> K
checkBreakPoint dbg vm nextStep mode k =
  do let eenv   = vmCurExecEnv vm
         curFun = funValueName (execFunction eenv)
     case (nextStep, curFun) of
       (Goto pc, LuaFID fid) ->
         do let loc = (pc,fid)
            breaks <- readIORef (dbgBreaks dbg)
            case Map.lookup loc breaks of
              Just mbCond ->
                case mbCond of
                  Nothing -> atBreak
                  Just c ->
                    do act <- readIORef (brkActive c)
                       case act of
                         -- Already checked if active
                         Just active ->
                           do writeIORef (brkActive c) Nothing
                              if active then atBreak else k

                         -- Start evaluating the breakpoint condition
                         Nothing ->
                           do (nextStep',vm') <-
                                 executeCompiledStatment vm eenv (brkCond c)
                                   $ \vs -> do writeIORef (brkActive c)
                                                 $! Just
                                                 $! valueBool (trimResult1 vs)
                                               return nextStep
                              stepRun dbg vm' nextStep' (StepOut mode)
              _ -> k
       _ -> k

  where
  atBreak = doPause dbg vm nextStep ReachedBreakPoint


doPause :: ExecState -> VM -> NextStep -> IdleReason -> IO CNextStep
doPause dbg vm next reason =
  do writeIORef (dbgIdleReason dbg) reason
     putMVar (dbgPaused dbg) (vm,next)
     (newVM, newNext, newMode) <- takeMVar (dbgResume dbg)
     stepRun dbg newVM newNext newMode




-- | Before executing this "step" we may pause execution.
mayPauseBefore :: NextStep -> Bool
mayPauseBefore nextStep =
  case nextStep of
    Goto{}        -> True
    ApiStart{}    -> True
    ApiEnd{}      -> True
    _             -> False

nextMode :: VM -> NextStep -> StepMode -> StepMode

nextMode _ (Interrupt _) _ = Stop

nextMode vm step mode =
  case mode of

    Stop                -> Stop

    StepIntoOp ->
      case step of
        Goto    {}      -> Stop
        ApiStart{}      -> Stop
        ApiEnd {}       -> Stop
        _               -> mode

    StepOverOp ->
      case step of
        Goto    {}      -> StepOutOp
        ApiStart{}      -> StepOutOp
        ApiEnd {}       -> StepOutOp
        FunCall {}      -> StepOut StepOutOp      -- shouldn't happen
        Resume  {}      -> StepOutYield StepOutOp -- shouldn't happen
        _               -> mode

    StepOutOp ->
      case step of
        Goto    {}      -> Stop
        ApiStart{}      -> Stop
        ApiEnd {}       -> Stop
        FunCall {}      -> StepOut StepOutOp
        Resume  {}      -> StepOutYield StepOutOp
        _               -> mode

    StepOut m ->
      case step of
        FunCall   {}    -> StepOut mode
        ApiStart  {}    -> StepOut mode
        Resume    {}    -> StepOutYield mode
        FunReturn {}    -> m
        ErrorReturn {}  -> m
        ApiEnd    {}    -> m

        -- ThreadExit and ThreadFail can't happen, because we should have
        -- first reached the end of the function that we are trying to exit,
        -- and stopped at that point

        _               -> mode

    StepIntoLine n ->
      case step of
        FunCall {}      -> Stop
        FunTailcall {}  -> Stop
        FunReturn {}    -> Stop
        ErrorReturn {}  -> Stop
        ApiStart {}     -> Stop
        ApiEnd {}       -> Stop
        Goto pc
          | n >= 0 && n /= l -> Stop
          | otherwise        -> StepIntoLine l
          where l = getLineNumberForCurFunPC vm pc

        _ -> mode

    StepOverLine n ->
      case step of
        FunCall {}      -> StepOut mode
        FunTailcall {}  -> StepOut mode
        FunReturn {}    -> Stop
        ErrorReturn {}  -> Stop
        ApiStart {}     -> StepOut mode
        ApiEnd {}       -> Stop
        Resume {}       -> StepOutYield mode
        Goto pc
          | n >= 0 && n /= l -> Stop
          | otherwise        -> StepOverLine l
          where l = getLineNumberForCurFunPC vm pc

        _ -> mode

    StepOutYield m ->
      case step of
        Yield {}        -> m
        ThreadExit {}   -> m
        ThreadFail {}   -> m
        Resume {}       -> StepOutYield mode
        _               -> mode

    Run                 -> Run


-- | Returns 0 if there was no line number associated with this PC getLineNumberForCurFunPC :: VM -> Int -> Int
getLineNumberForCurFunPC :: VM -> Int -> Int
getLineNumberForCurFunPC vm pc =
  fromMaybe 0 $
  do (_,func) <- luaOpCodes (execFunction (vmCurExecEnv vm))
     lookupLineNumber func pc







