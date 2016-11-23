{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
module Galua.OpcodeInterpreter where

import           Control.Exception hiding (Handler)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Foldable
import           Data.IORef
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as IOVector

import qualified Galua.Util.SizedVector as SV

import           Language.Lua.Bytecode
import           Language.Lua.Bytecode.FunId
import           Galua.Value
import           Galua.FunValue
import           Galua.Overloading
import           Galua.Mach
import           Galua.Number
import           Galua.LuaString

-- | Compute the 'Value' corresponding to a bytecode file 'Constant'.
constantValue :: Constant -> IO Value
constantValue k =
  case k of
    KNil      -> return Nil
    KNum d    -> return (Number (Double d))
    KInt i    -> return (Number (Int i))
    KBool b   -> return (Bool b)
    KString s -> String <$> fromByteString s
    KLongString s -> String <$> fromByteString s



-- | Attempt to load the instruction stored at the given address
-- in the currently executing function.
{-# INLINE loadInstruction #-}
loadInstruction :: Int {- ^ instruction counter -} -> Mach OpCode
loadInstruction i =
  do code <- getsExecEnv execInstructions
     case code Vector.!? i of
       Nothing -> interpThrow (BadPc i)
       Just x -> return x

-- | Attempt to load the EXTRAARG instruction stored at the given address
-- in the currently executing function.
{-# INLINE loadExtraArg #-}
loadExtraArg :: Int {- ^ program counter -} -> Mach Int
loadExtraArg pc =
  do instr <- loadInstruction pc
     case instr of
       OP_EXTRAARG k -> return k
       _ -> interpThrow ExpectedExtraArg





-- | Compute the result of executing the opcode at the given address
-- within the current function execution environment.
execute :: Int {- ^ program counter -} -> Mach a
execute !pc =

  do instr <- loadInstruction pc
     let advance      = jump 0
         jump i       = machGoto (pc + i + 1)
         getExtraArg  = loadExtraArg (pc+1)
         tgt =: m     = m >>= set tgt >> advance

     case instr of
       OP_MOVE     tgt src -> tgt =: get src
       OP_LOADK    tgt src -> tgt =: get src
       OP_GETUPVAL tgt src -> tgt =: get src
       OP_SETUPVAL src tgt -> tgt =: get src
                   -- SETUPVAL's argument order is different!

       OP_LOADKX tgt ->
         do src <- Kst <$> getExtraArg
            set tgt =<< get src
            jump 1 -- skip over the extrarg

       OP_LOADBOOL tgt b c ->
         do set tgt (Bool b)
            jump (if c then 1 else 0)

       OP_LOADNIL tgt count ->
         do traverse_ (\r -> set r Nil) [tgt .. plusReg tgt count]
            advance

       OP_GETTABUP tgt src tabKey ->
         do t <- get src
            k <- get tabKey
            tgt =: m__index t k

       OP_GETTABLE tgt src tabKey ->
         do t <- get src
            k <- get tabKey
            tgt =: m__index t k

       OP_SETTABUP tgt key src ->
         do t <- get tgt
            k <- get key
            v <- get src
            m__newindex t k v
            advance

       OP_SETTABLE tgt key src ->
         do t <- get tgt
            k <- get key
            v <- get src
            m__newindex t k v
            advance

       OP_NEWTABLE tgt arraySize hashSize ->
        tgt =: (Table <$> machNewTable arraySize hashSize)

       OP_SELF tgt src key ->
         do t <- get src
            k <- get key
            v <- m__index t k
            set tgt v
            set (succ tgt) t
            advance

       OP_ADD  tgt op1 op2 -> tgt =: binOp m__add  op1 op2
       OP_SUB  tgt op1 op2 -> tgt =: binOp m__sub  op1 op2
       OP_MUL  tgt op1 op2 -> tgt =: binOp m__mul  op1 op2
       OP_MOD  tgt op1 op2 -> tgt =: binOp m__mod  op1 op2
       OP_POW  tgt op1 op2 -> tgt =: binOp m__pow  op1 op2
       OP_DIV  tgt op1 op2 -> tgt =: binOp m__div  op1 op2
       OP_IDIV tgt op1 op2 -> tgt =: binOp m__idiv op1 op2
       OP_BAND tgt op1 op2 -> tgt =: binOp m__band op1 op2
       OP_BOR  tgt op1 op2 -> tgt =: binOp m__bor  op1 op2
       OP_BXOR tgt op1 op2 -> tgt =: binOp m__bxor op1 op2
       OP_SHL  tgt op1 op2 -> tgt =: binOp m__shl  op1 op2
       OP_SHR  tgt op1 op2 -> tgt =: binOp m__shr  op1 op2

       OP_UNM  tgt op1  -> tgt =: (get op1 >>= m__unm)
       OP_BNOT tgt op1  -> tgt =: (get op1 >>= m__bnot)
       OP_LEN  tgt op1  -> tgt =: (get op1 >>= m__len)
       OP_NOT  tgt op1  -> tgt =: (get op1 >>= opNot)

       OP_CONCAT tgt start end ->
         do xs  <- traverse get [start .. end]
            tgt =: m__concat xs

       OP_JMP mbCloseReg jmp ->
         do traverse_ closeStack mbCloseReg
            jump jmp

       OP_EQ invert op1 op2 -> jump =<< relOp m__eq invert op1 op2
       OP_LT invert op1 op2 -> jump =<< relOp m__lt invert op1 op2
       OP_LE invert op1 op2 -> jump =<< relOp m__le invert op1 op2

       OP_TEST ra c ->
          do a <- get ra
             jump (if valueBool a == c then 0 else 1)

       OP_TESTSET ra rb c ->
          do b <- get rb
             if valueBool b == c
               then do set ra b
                       advance
               else jump 1

       OP_CALL a b c ->
         do u      <- get a
            args   <- getCallArguments (succ a) b
            result <- m__call u args
            setCallResults a c result
            advance

       OP_TAILCALL a b _c ->
         do u       <- get a
            args    <- getCallArguments (succ a) b
            (f,as)  <- resolveFunction u args
            machTailcall f as

       OP_RETURN a c ->
         do vs <- getCallArguments a c
            machReturn vs

       OP_FORLOOP a sBx ->
         do initial <- forloopAsNumber "initial" =<< get a
            limit   <- forloopAsNumber "limit"   =<< get (plusReg a 1)
            step    <- forloopAsNumber "step"    =<< get (plusReg a 2)
            let next = initial + step
                cond | 0 < step = next <= limit
                     | otherwise = limit <= next
            if cond
              then do set a (Number next)
                      set (plusReg a 3) (Number next)
                      jump sBx
              else advance

       OP_FORPREP a jmp ->
         do initial <- forloopAsNumber "initial" =<< get a
            limit   <- forloopAsNumber "limit"   =<< get (plusReg a 1)
            step    <- forloopAsNumber "step"    =<< get (plusReg a 2)
            set a (Number (initial - step))
            set (plusReg a 1) (Number limit) -- save back the Number form
            set (plusReg a 2) (Number step)  -- save back the Number form
            jump jmp

       OP_TFORCALL a c ->
         do f  <- get a
            a1 <- get (plusReg a 1)
            a2 <- get (plusReg a 2)
            result <- m__call f [a1,a2]
            let resultRegs = regRange (plusReg a 3) c
            zipWithM_ set resultRegs (result ++ repeat Nil)
            advance

       OP_TFORLOOP a sBx ->
         do v <- get (succ a)
            if v == Nil
              then advance
              else do set a v
                      jump sBx

       OP_SETLIST a b c ->
         do tab' <- get a
            tab <- case tab' of
                     Table tab -> return tab
                     _ -> interpThrow SetListNeedsTable

            count <-
              if b == 0
                 then do Reg top <- getTop
                         let Reg aval = a
                         return (top - aval - 1)
                 else return b

            (offset, skip) <-
               if c == 0
                 then do k <- getExtraArg
                         return (50*k,1)
                 else return (50 * (c-1), 0)

            forM_ [1..count] $ \i ->
              setTableRaw tab (Number (Int (offset+i))) =<< get (plusReg a i)

            resetTop

            jump skip

       OP_CLOSURE tgt i ->
         do (fid,closureFunc) <- getProto i
            closureUpvals <- liftIO . Vector.thaw
                         =<< traverse getLValue (funcUpvalues closureFunc)
            tgt =: (Closure <$> machNewClosure (luaFunction fid closureFunc)
                                                                closureUpvals)

       OP_VARARG a b ->
         do varargs <- getsExecEnv execVarargs
            setCallResults a b =<< liftIO (readIORef varargs)
            advance

       OP_EXTRAARG{} -> interpThrow UnexpectedExtraArg


-- | Get a list of the `count` values stored from `start`.
getCallArguments :: Reg {- ^ start -} -> Count {- ^ count -} -> Mach [Value]
getCallArguments a b =
  do end <- case b of
       CountTop   -> getTop
       CountInt x -> return (plusReg a x)
     vs <- traverse get [a .. pred end]
     resetTop
     return vs


-- | Stores a list of results into a given register range.
-- When expected is 'CountTop', values are stored up to TOP
setCallResults ::
  Reg     {- ^ starting register -} ->
  Count   {- ^ results expected  -} ->
  [Value] {- ^ results           -} ->
  Mach ()
setCallResults a b xs =
  do end <- case b of
              CountInt x -> return (plusReg a x)
              CountTop -> top <$ setTop top
                 where top = plusReg a (length xs)
     zipWithM_ set [a .. pred end] (xs ++ repeat Nil)

-- | Allocate fresh references for all registers from the `start` to the
-- `TOP` of the stack
closeStack :: Reg {- ^ start -} -> Mach ()
closeStack (Reg a) =
  do stack <- getsExecEnv execStack
     liftIO $
       do n <- SV.size stack
          for_ [a .. n-1] $ \i ->
             SV.set stack i =<< newIORef Nil

-- | Interpret a value as an input to a for-loop. If the value
-- is not a number an error is raised using the given name.
forloopAsNumber :: String {- ^ argument name -} -> Value -> Mach Number
forloopAsNumber label v =
  case valueNumber v of
    Just n -> return n
    Nothing -> luaError ("'for' " ++ label ++ " must be a number")

asLuaFunction :: FunctionValue -> Mach (FunId, Function)
asLuaFunction run =
  case luaOpCodes run of
    Just (fid,f) -> return (fid,f)
    Nothing      -> interpThrow NonLuaFunction
------------------------------------------------------------------------
-- Reference accessors
------------------------------------------------------------------------

-- | Class for types that are indexes to references
class LValue a where
  getLValue :: a -> Mach (IORef Value)

instance LValue UpIx where
  getLValue (UpIx i) =
    do upvals <- getsExecEnv execUpvals
       if 0 <= i && i < IOVector.length upvals then
         liftIO (IOVector.read upvals i)
       else
         interpThrow (BadUpval i)

instance LValue Reg where
  getLValue (Reg i) =
    do stack <- getsExecEnv execStack
       liftIO (SV.get stack i)

instance LValue Upvalue where
  getLValue (UpUp  x) = getLValue x
  getLValue (UpReg x) = getLValue x

set :: LValue a => a -> Value -> Mach ()
set r !x =
  do ref <- getLValue r
     liftIO (writeIORef ref x)

-- | Class for types that are indexes to values.
class    RValue a       where get :: a -> Mach Value
instance RValue Upvalue where get = lvalToRval
instance RValue UpIx    where get = lvalToRval
instance RValue Reg     where get = lvalToRval

lvalToRval :: LValue a => a -> Mach Value
lvalToRval r = liftIO . readIORef =<< getLValue r

instance RValue RK where
  get (RK_Reg r) = get r
  get (RK_Kst k) = get k

instance RValue Kst where
  get (Kst i) =
    do (_fid,Function{..}) <- asLuaFunction =<< getsExecEnv execFunction
       case funcConstants Vector.!? i of
         Nothing -> interpThrow (BadConstant i)
         Just x -> liftIO (constantValue x)

getProto :: ProtoIx -> Mach (FunId,Function)
getProto (ProtoIx i) =
  do (fid,f) <- asLuaFunction =<< getsExecEnv execFunction
     case funcProtos f Vector.!? i of
       Nothing -> interpThrow (BadProto i)
       Just x  -> return (subFun fid i,x)

------------------------------------------------------------------------
-- Unary operations
------------------------------------------------------------------------

opNot :: Monad m => Value -> m Value
opNot x = return (Bool (not (valueBool x)))



------------------------------------------------------------------------
-- Binary operations
------------------------------------------------------------------------

binOp :: (Value -> Value -> Mach Value) -> RK -> RK -> Mach Value
binOp f src1 src2 =
  do x1 <- get src1
     x2 <- get src2
     f x1 x2




------------------------------------------------------------------------
-- Relational operations
------------------------------------------------------------------------

relOp ::
  (Value -> Value -> Mach Bool) ->
  Bool -> RK -> RK -> Mach Int {- ^ how many instructions to advance -}
relOp (?) invert op1 op2 =
  do x1  <- get op1
     x2  <- get op2
     -- if ((RK(B) ? RK(C)) ~= A) then pc++
     res <- x1 ? x2
     return (if res /= invert then 1 else 0)


------------------------------------------------------------------------
-- Interpreter failures
------------------------------------------------------------------------

-- | Raise an exception due to a bug in the implementation of either the
-- interpreter or the bytecode compiler. These exceptions are not accessible
-- to the executing Lua program and should never occur due to a bug in a
-- user program.
interpThrow :: InterpreterFailureType -> Mach b
interpThrow e = liftIO (throwIO (InterpreterFailure {-loc-} e))


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



-- | Get the register that is one beyond the last valid
-- register. This value is meaningful when dealing with
-- variable argument function calls and returns.
getTop :: Mach Reg
getTop =
  do stack <- getsExecEnv execStack
     n     <- liftIO (SV.size stack)
     return (Reg n)

resetTop :: Mach ()
resetTop =
  do (_,fun) <- asLuaFunction =<< getsExecEnv execFunction
     let n = funcMaxStackSize fun
     setTop (Reg n)

-- | Update TOP and ensure that the stack is large enough
-- to index up to (but excluding) TOP.
setTop :: Reg -> Mach ()
setTop (Reg newLen) =
  do stack <- getsExecEnv execStack

     liftIO $
       do oldLen <- SV.size stack

          if oldLen <= newLen

            -- Grow to new size
            then replicateM_ (newLen - oldLen) $
                    SV.push stack =<< newIORef Nil

            -- Release references
            else SV.shrink stack (oldLen - newLen)
