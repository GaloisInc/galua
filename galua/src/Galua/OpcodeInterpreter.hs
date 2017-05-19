{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
module Galua.OpcodeInterpreter(execute) where

import           Control.Exception hiding (Handler)
import           Control.Monad
import           Data.Foldable
import           Data.IORef
import qualified Data.Vector as Vector

import qualified Galua.Util.SizedVector as SV

import           Galua.Code
import           Galua.Value
import           Galua.FunValue
import           Galua.Overloading
import           Galua.Mach
import           Galua.Number
import           Galua.LuaString(fromByteString)
import qualified Data.Vector.Mutable as IOVector

-- | Attempt to load the instruction stored at the given address
-- in the currently executing function.
{-# INLINE loadInstruction #-}
loadInstruction :: LuaExecEnv -> Int {- ^ instruction counter -} -> OpCode
loadInstruction eenv i =
  luaExecCode eenv `Vector.unsafeIndex` i
  -- program counter offsets are checked when functions
  -- are loaded in Galua.Code

kst :: Literal -> IO Value
kst k =
  case k of
    LNil    -> return Nil
    LBool b -> return (Bool b)
    LNum x  -> return (Number (Double x))
    LInt x  -> return (Number (Int x))
    LStr x  -> String <$> fromByteString x

-- | Compute the result of executing the opcode at the given address
-- within the current function execution environment.
execute :: VM -> Int -> IO NextStep
execute !vm !pc =

  do let eenv = case vmCurExecEnv vm of
                  ExecInLua lenv -> lenv
                  ExecInC {}     -> error "[bug] `execute` whilte `ExecInC`"
         advance      = jump 0
         jump i       = return $! Goto (pc + i + 1)
         tgt =: m     = do a <- m
                           set eenv tgt a
                           advance
         storeIn tgt v  = tgt =: return v
         tabs = machMetatablesRef (vmMachineEnv vm)

         binOp f tgt src1 src2 =
           do x1 <- get eenv src1
              x2 <- get eenv src2
              f tabs (storeIn tgt) x1 x2

         unOp f tgt src1 =
           do x1 <- get eenv src1
              f tabs (storeIn tgt) x1

         relOp f invert op1 op2 =
           do x1  <- get eenv op1
              x2  <- get eenv op2
              -- if ((RK(B) ? RK(C)) ~= A) then pc++
              f tabs (\res -> jump (if res /= invert then 1 else 0)) x1 x2


     case loadInstruction eenv pc of
       OP_MOVE     tgt src -> tgt =: get eenv src
       OP_LOADK    tgt src -> tgt =: kst src
       OP_GETUPVAL tgt src -> tgt =: get eenv src
       OP_SETUPVAL src tgt -> tgt =: get eenv src
                   -- SETUPVAL's argument order is different!

       OP_LOADKX tgt v ->
         do set eenv tgt =<< kst v
            jump 1 -- skip over the extrarg

       OP_LOADBOOL tgt b c ->
         do set eenv tgt (Bool b)
            jump (if c then 1 else 0)

       OP_LOADNIL tgt count ->
         do traverse_ (\r -> set eenv r Nil) (regRange tgt (count+1))
            advance

       OP_GETTABUP tgt src tabKey -> binOp m__index tgt src tabKey
       OP_GETTABLE tgt src tabKey -> binOp m__index tgt src tabKey

       OP_SETTABUP tgt key src ->
         do t <- get eenv tgt
            k <- get eenv key
            v <- get eenv src
            m__newindex tabs advance t k v

       OP_SETTABLE tgt key src ->
         do t <- get eenv tgt
            k <- get eenv key
            v <- get eenv src
            m__newindex tabs advance t k v

       OP_NEWTABLE tgt arraySize hashSize ->
        tgt =: (Table <$> machNewTable vm arraySize hashSize)

       OP_SELF tgt src key ->
         do t <- get eenv src
            k <- get eenv key
            let after v =  do set eenv tgt v
                              set eenv (plusReg tgt 1) t
                              advance
            m__index tabs after t k


       OP_ADD  tgt op1 op2 -> binOp m__add  tgt op1 op2
       OP_SUB  tgt op1 op2 -> binOp m__sub  tgt op1 op2
       OP_MUL  tgt op1 op2 -> binOp m__mul  tgt op1 op2
       OP_MOD  tgt op1 op2 -> binOp m__mod  tgt op1 op2
       OP_POW  tgt op1 op2 -> binOp m__pow  tgt op1 op2
       OP_DIV  tgt op1 op2 -> binOp m__div  tgt op1 op2
       OP_IDIV tgt op1 op2 -> binOp m__idiv tgt op1 op2
       OP_BAND tgt op1 op2 -> binOp m__band tgt op1 op2
       OP_BOR  tgt op1 op2 -> binOp m__bor  tgt op1 op2
       OP_BXOR tgt op1 op2 -> binOp m__bxor tgt op1 op2
       OP_SHL  tgt op1 op2 -> binOp m__shl  tgt op1 op2
       OP_SHR  tgt op1 op2 -> binOp m__shr  tgt op1 op2

       OP_UNM  tgt op1  -> unOp m__unm  tgt op1
       OP_BNOT tgt op1  -> unOp m__bnot tgt op1
       OP_LEN  tgt op1  -> unOp m__len  tgt op1
       OP_NOT  tgt op1  -> tgt =: (Bool . not . valueBool <$> get eenv op1)


       OP_CONCAT tgt start end ->
         do xs  <- traverse (get eenv) (regFromTo start end)
            m__concat tabs (storeIn tgt) xs

       OP_JMP mbCloseReg jmp ->
         do traverse_ (closeStack eenv) mbCloseReg
            jump jmp

       OP_EQ invert op1 op2 -> relOp m__eq invert op1 op2
       OP_LT invert op1 op2 -> relOp m__lt invert op1 op2
       OP_LE invert op1 op2 -> relOp m__le invert op1 op2

       OP_TEST ra c ->
          do a <- get eenv ra
             jump (if valueBool a == c then 0 else 1)

       OP_TESTSET ra rb c ->
          do b <- get eenv rb
             if valueBool b == c
               then do set eenv ra b
                       advance
               else jump 1

       OP_CALL a b c ->
         do u      <- get eenv a
            args   <- getCallArguments eenv (plusReg a 1) b
            let after result = do setCallResults eenv a c result
                                  advance
            m__call tabs after u args

       OP_TAILCALL a b _c ->
         do u       <- get eenv a
            args    <- getCallArguments eenv (plusReg a 1) b
            let after (f,as) = return (FunTailcall f as)
            resolveFunction tabs after u args

       OP_RETURN a c -> FunReturn <$> getCallArguments eenv a c

       OP_FORLOOP a sBx ->
          get eenv a             >>= \v1 ->
          get eenv (plusReg a 1) >>= \v2 ->
          get eenv (plusReg a 2) >>= \v3 ->
          forloopAsNumber "initial" v1 $ \initial ->
          forloopAsNumber "limit"   v2 $ \limit ->
          forloopAsNumber "step"    v3 $ \step ->
            let next = initial + step
                cond | 0 < step = next <= limit
                     | otherwise = limit <= next
            in if cond
                 then do set eenv a (Number next)
                         set eenv (plusReg a 3) (Number next)
                         jump sBx
                 else advance

       OP_FORPREP a jmp ->
          get eenv a             >>= \v1 ->
          get eenv (plusReg a 1) >>= \v2 ->
          get eenv (plusReg a 2) >>= \v3 ->
          forloopAsNumber "initial" v1 $ \initial ->
          forloopAsNumber "limit"   v2 $ \limit ->
          forloopAsNumber "step"    v3 $ \step ->
            do set eenv a (Number (initial - step))
               set eenv (plusReg a 1) (Number limit)
               -- save back the Number form
               set eenv (plusReg a 2) (Number step)
               -- save back the Number form
               jump jmp

       OP_TFORCALL a c ->
         do f  <- get eenv a
            a1 <- get eenv (plusReg a 1)
            a2 <- get eenv (plusReg a 2)
            let after result =
                 do setCallResults eenv (plusReg a 3) (CountInt c) result
                    advance
            m__call tabs after f [a1,a2]

       OP_TFORLOOP a sBx ->
         do v <- get eenv (plusReg a 1)
            if v == Nil
              then advance
              else do set eenv a v
                      jump sBx

       OP_SETLIST a b offset skip ->
         do tab' <- get eenv a
            tab <- case tab' of
                     Table tab -> return tab
                     _ -> interpThrow SetListNeedsTable

            count <-
              case b of
                CountTop ->
                  do sz <- getStackSize eenv
                     let Reg aval = a
                     return (sz - aval - 1)
                CountInt i -> return i

            forM_ [1..count] $ \i ->
              do x <- get eenv (plusReg a i)
                 setTableRaw tab (Number (Int (offset+i))) x

            resetStackSize eenv
            jump skip

       OP_CLOSURE tgt ix cloFunc ->
         do vs <- traverse (getLValue eenv) (funcUpvalues cloFunc)
            closureUpvals <- Vector.thaw vs
            let fid = luaExecFID eenv
                f   = luaFunction (subFun fid ix) cloFunc
            tgt =: (Closure <$> machNewClosure vm f closureUpvals)

       OP_VARARG a b ->
         do let varargs = luaExecVarargs eenv
            setCallResults eenv a b =<< readIORef varargs
            advance

       OP_EXTRAARG{} -> interpThrow UnexpectedExtraArg


-- | Get a list of the `count` values stored from `start`.
getCallArguments ::
  LuaExecEnv -> Reg {- ^ start -} -> Count {- ^ count -} -> IO [Value]
getCallArguments eenv a b =
  do end <- case b of
       CountTop   -> do sz <- getStackSize eenv
                        return $! Reg (sz-1)
       CountInt x -> return (plusReg a (x-1))
     vs <- traverse (get eenv) (regFromTo a end)
     resetStackSize eenv
     return vs


-- | Stores a list of results into a given register range.
-- When expected is 'CountTop', values are stored up to TOP
setCallResults ::
  LuaExecEnv ->
  Reg     {- ^ starting register -} ->
  Count   {- ^ results expected  -} ->
  [Value] {- ^ results           -} ->
  IO ()
setCallResults eenv a b xs =
  do count <- case b of
              CountInt x -> return x
              CountTop   -> n <$ setStackSize eenv sz'
                 where Reg base = a
                       sz' = base + n
                       n   = length xs

     zipWithM_ (set eenv) (regRange a count) (xs ++ repeat Nil)

-- | Allocate fresh references for all registers from the `start` to the
-- `TOP` of the stack
closeStack :: LuaExecEnv -> Reg {- ^ start -} -> IO ()
closeStack eenv (Reg a) =
  do let stack = luaExecStack eenv
     n <- SV.size stack
     for_ [a .. n-1] $ \i ->
       SV.set stack i =<< newIORef Nil

-- | Interpret a value as an input to a for-loop. If the value
-- is not a number an error is raised using the given name.
forloopAsNumber :: String {- ^ argument name -} -> Value ->
                  (Number -> IO NextStep) -> IO NextStep
forloopAsNumber label v cont =
  case valueNumber v of
    Just n  -> cont n
    Nothing -> luaError' ("'for' " ++ label ++ " must be a number")
{-
curLuaFunction :: ExecEnv -> IO (FunId,Function)
curLuaFunction eenv =
  case env of
    ExecInLua lenv -> return (luaExecFID lenv, luaExecFunction lenv)
    ExecInC {}     -> interpThrow NonLuaFunction
-}
------------------------------------------------------------------------
-- Reference accessors
------------------------------------------------------------------------

-- | Class for types that are indexes to references
class LValue a where
  getLValue :: LuaExecEnv -> a -> IO (IORef Value)

instance LValue UpIx where
  getLValue eenv (UpIx i) =
    do let upvals = luaExecUpvals eenv
       IOVector.unsafeRead upvals i
  {-# INLINE getLValue #-}

instance LValue Reg where
  getLValue eenv (Reg i) = SV.unsafeGet (luaExecStack eenv) i
  {-# INLINE getLValue #-}

instance LValue Upvalue where
  getLValue eenv (UpUp  x) = getLValue eenv x
  getLValue eenv (UpReg x) = getLValue eenv x
  {-# INLINE getLValue #-}

{-# INLINE set #-}
set :: LValue a => LuaExecEnv -> a -> Value -> IO ()
set eenv r !x =
  do ref <- getLValue eenv r
     writeIORef ref x

-- | Class for types that are indexes to values.
class RValue a where
  get :: LuaExecEnv -> a -> IO Value

instance RValue Upvalue where
  {-# INLINE get #-}
  get = lvalToRval

instance RValue UpIx where
  {-# INLINE get #-}
  get = lvalToRval

instance RValue Reg where
  {-# INLINE get #-}
  get = lvalToRval

{-# INLINE lvalToRval #-}
lvalToRval :: LValue a => LuaExecEnv -> a -> IO Value
lvalToRval eenv r = readIORef =<< getLValue eenv r

instance RValue RK where
  get eenv (RK_Reg r) = get eenv r
  get _    (RK_Kst k) = kst k
  {-# INLINE get #-}



------------------------------------------------------------------------
-- Interpreter failures
------------------------------------------------------------------------

-- | Raise an exception due to a bug in the implementation of either the
-- interpreter or the bytecode compiler. These exceptions are not accessible
-- to the executing Lua program and should never occur due to a bug in a
-- user program.
interpThrow :: InterpreterFailureType -> IO b
interpThrow e = throwIO (InterpreterFailure {-loc-} e)


-- | Failure type paired with the stack trace at the time of failure
data InterpreterFailure =
      InterpreterFailure {-[LocationInfo]-} InterpreterFailureType

instance Exception InterpreterFailure

-- | Types of fatal interpreter failures
data InterpreterFailureType
  = BadProto Int
  | BadPc Int
  | BadStack Int
  | BadUpval Int
  | BadConstant Int
  | ExpectedExtraArg
  | UnexpectedExtraArg
  | SetListNeedsTable
  | NonLuaFunction
  deriving (Show)

instance Show InterpreterFailure where
  show (InterpreterFailure {-loc-} e) = unlines
    ( "Interpreter failed!"
    : [("Exception: " ++ show e)]
    -- : ""
    -- : map prettyLocationInfo loc
    )


--------------------------------------------------------------------------------



-- | Get the last valid register.
-- This value is meaningful when dealing with
-- variable argument function calls and returns.
{-# INLINE getStackSize #-}
getStackSize :: LuaExecEnv -> IO Int
getStackSize eenv =
  do let stack = luaExecStack eenv
     SV.size stack

{-# INLINE resetStackSize #-}
resetStackSize :: LuaExecEnv -> IO ()
resetStackSize eenv =
  do let fun = luaExecFunction eenv
     setStackSize eenv (funcMaxStackSize fun)

-- | Update TOP and ensure that the stack is large enough
-- to index up to (but excluding) TOP.
setStackSize :: LuaExecEnv -> Int -> IO ()
setStackSize eenv newLen =
  do let stack = luaExecStack eenv
     oldLen <- SV.size stack

     if oldLen <= newLen

       -- Grow to new size
       then replicateM_ (newLen - oldLen) $
               SV.push stack =<< newIORef Nil

       -- Release references
       else SV.shrink stack (oldLen - newLen)

