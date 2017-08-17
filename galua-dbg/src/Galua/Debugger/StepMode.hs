module Galua.Debugger.StepMode where

import           Data.Maybe (fromMaybe)

import Galua.Code(lookupLineNumber)
import Galua.Mach

data StepMode
  = Run
    -- ^ Evaluate until something causes us to stop.

  | StepIntoOp
    -- ^ Evaluate one op-code.
    -- If the op-code is a function call, stop at the beginning of the
    -- function.

  | StepOverOp
    -- ^ Evaluate one op-code.
    -- If the op-code is a function call, do not stop until the function
    -- returns, or something else causes us to stop.

  | StepOutOp
    -- ^ Finish evaluating the current code, step over function calls

  | Stop
    -- ^ Evaluate until the closest safe place to stop.

  | StepOut StepMode
    -- ^ Evaluate until we return from the current function.
    -- After that, procdeed with the given mode.


  | StepOverLine {-# UNPACK #-} !Int
    -- ^ Evaluate while we are on this line number.
    -- If we encoutner functions, do not stop until they return
    -- or some other condition caused us to stop.

  | StepIntoLine {-# UNPACK #-} !Int
    -- ^ Evaluate while we are on this line number.
    -- ^ If we encoutner a function-call, then we stop at the beginning
    -- of the called function.

  | StepOutYield StepMode
    -- ^ Evaluate until the current thread yeilds (or something else
    -- causes us to stop).
    -- After that, proceed with the given mode.

  deriving (Eq, Show)


-- | Before executing this "step" we may pause execution.
mayPauseBefore :: NextStep -> Bool
mayPauseBefore nextStep =
  case nextStep of
    Goto{}        -> True
    ApiStart{}    -> True
    ApiEnd{}      -> True
    _             -> False



{-# INLINE nextMode #-}
-- | How a 'NextStep' affects the execution mode of the debugger.
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


-- | Returns 0 if there was no line number associated with this PC.
getLineNumberForCurFunPC :: VM -> Int -> Int
getLineNumberForCurFunPC vm pc =
  case vmCurExecEnv vm of
    ExecInLua lenv -> fromMaybe 0 (lookupLineNumber (luaExecFunction lenv) pc)
    ExecInC {}     -> 0


