{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
module Galua.OpcodeInterpreter(execute) where

import           Control.Exception hiding (Handler)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Foldable
import           Data.IORef
import qualified Data.Vector as Vector

import qualified Galua.Util.SizedVector as SV

import           Language.Lua.Bytecode
import           Language.Lua.Bytecode.FunId
import           Galua.Value
import           Galua.FunValue
import           Galua.Overloading
import           Galua.Mach
import           Galua.Number
import           Galua.LuaString
import qualified Galua.Util.IOVector as IOVector

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
loadInstruction :: ExecEnv -> Int {- ^ instruction counter -} -> IO OpCode
loadInstruction eenv i =
  case execInstructions eenv Vector.!? i of
    Nothing -> interpThrow (BadPc i)
    Just x  -> return x

-- | Attempt to load the EXTRAARG instruction stored at the given address
-- in the currently executing function.
{-# INLINE loadExtraArg #-}
loadExtraArg :: ExecEnv -> Int {- ^ program counter -} -> IO Int
loadExtraArg eenv pc =
  do instr <- loadInstruction eenv pc
     case instr of
       OP_EXTRAARG k -> return k
       _             -> interpThrow ExpectedExtraArg



execute :: Int {- ^ program counter -} -> Mach a
execute pc =
  do eenv <- getsExecEnv id
     execute' eenv pc


-- | Compute the result of executing the opcode at the given address
-- within the current function execution environment.
execute' :: ExecEnv -> Int -> Mach a
execute' eenv !pc =

  do instr <- liftIO (loadInstruction eenv pc)
     let advance      = jump 0
         jump i       = machGoto (pc + i + 1)
         getExtraArg  = loadExtraArg eenv (pc+1)
         tgt =: m     = do a <- m
                           liftIO (set eenv tgt a)
                           advance

     case instr of
       OP_MOVE     tgt src -> tgt =: liftIO (get eenv src)
       OP_LOADK    tgt src -> tgt =: liftIO (get eenv src)
       OP_GETUPVAL tgt src -> tgt =: liftIO (get eenv src)
       OP_SETUPVAL src tgt -> tgt =: liftIO (get eenv src)
                   -- SETUPVAL's argument order is different!

       OP_LOADKX tgt ->
         do liftIO (do src <- Kst <$> getExtraArg
                       set eenv tgt =<< get eenv src)
            jump 1 -- skip over the extrarg

       OP_LOADBOOL tgt b c ->
         do liftIO (set eenv tgt (Bool b))
            jump (if c then 1 else 0)

       OP_LOADNIL tgt count ->
         do liftIO (traverse_ (\r -> set eenv r Nil) [tgt .. plusReg tgt count])
            advance

       OP_GETTABUP tgt src tabKey -> tgt =: binOp eenv m__index src tabKey
       OP_GETTABLE tgt src tabKey -> tgt =: binOp eenv m__index src tabKey

       OP_SETTABUP tgt key src ->
         do t <- liftIO (get eenv tgt)
            k <- liftIO (get eenv key)
            v <- liftIO (get eenv src)
            m__newindex t k v
            advance

       OP_SETTABLE tgt key src ->
         do t <- liftIO (get eenv tgt)
            k <- liftIO (get eenv key)
            v <- liftIO (get eenv src)
            m__newindex t k v
            advance

       OP_NEWTABLE tgt arraySize hashSize ->
        tgt =: (Table <$> machNewTable arraySize hashSize)

       OP_SELF tgt src key ->
         do t <- liftIO (get eenv src)
            k <- liftIO (get eenv key)
            v <- m__index t k
            liftIO $ do set eenv tgt v
                        set eenv (succ tgt) t
            advance

       OP_ADD  tgt op1 op2 -> tgt =: binOp eenv m__add  op1 op2
       OP_SUB  tgt op1 op2 -> tgt =: binOp eenv m__sub  op1 op2
       OP_MUL  tgt op1 op2 -> tgt =: binOp eenv m__mul  op1 op2
       OP_MOD  tgt op1 op2 -> tgt =: binOp eenv m__mod  op1 op2
       OP_POW  tgt op1 op2 -> tgt =: binOp eenv m__pow  op1 op2
       OP_DIV  tgt op1 op2 -> tgt =: binOp eenv m__div  op1 op2
       OP_IDIV tgt op1 op2 -> tgt =: binOp eenv m__idiv op1 op2
       OP_BAND tgt op1 op2 -> tgt =: binOp eenv m__band op1 op2
       OP_BOR  tgt op1 op2 -> tgt =: binOp eenv m__bor  op1 op2
       OP_BXOR tgt op1 op2 -> tgt =: binOp eenv m__bxor op1 op2
       OP_SHL  tgt op1 op2 -> tgt =: binOp eenv m__shl  op1 op2
       OP_SHR  tgt op1 op2 -> tgt =: binOp eenv m__shr  op1 op2

       OP_UNM  tgt op1  -> tgt =: (liftIO (get eenv op1) >>= m__unm)
       OP_BNOT tgt op1  -> tgt =: (liftIO (get eenv op1) >>= m__bnot)
       OP_LEN  tgt op1  -> tgt =: (liftIO (get eenv op1) >>= m__len)
       OP_NOT  tgt op1  -> tgt =: liftIO
                                   (Bool . not . valueBool <$> get eenv op1)


       OP_CONCAT tgt start end ->
         do xs  <- liftIO (traverse (get eenv) [start .. end])
            tgt =: m__concat xs

       OP_JMP mbCloseReg jmp ->
         do liftIO (traverse_ (closeStack eenv) mbCloseReg)
            jump jmp

       OP_EQ invert op1 op2 -> jump =<< relOp eenv m__eq invert op1 op2
       OP_LT invert op1 op2 -> jump =<< relOp eenv m__lt invert op1 op2
       OP_LE invert op1 op2 -> jump =<< relOp eenv m__le invert op1 op2

       OP_TEST ra c ->
          do a <- liftIO (get eenv ra)
             jump (if valueBool a == c then 0 else 1)

       OP_TESTSET ra rb c ->
          do b <- liftIO (get eenv rb)
             if valueBool b == c
               then do liftIO (set eenv ra b)
                       advance
               else jump 1

       OP_CALL a b c ->
         do u      <- liftIO (get eenv a)
            args   <- liftIO (getCallArguments eenv (succ a) b)
            result <- m__call u args
            liftIO (setCallResults eenv a c result)
            advance

       OP_TAILCALL a b _c ->
         do u       <- liftIO (get eenv a)
            args    <- liftIO (getCallArguments eenv (succ a) b)
            (f,as)  <- resolveFunction u args
            machTailcall f as

       OP_RETURN a c ->
         do vs <- liftIO (getCallArguments eenv a c)
            machReturn vs

       OP_FORLOOP a sBx ->
         do initial <- forloopAsNumber "initial" =<< liftIO (get eenv a)
            limit   <- forloopAsNumber "limit"   =<< liftIO (get eenv (plusReg a 1))
            step    <- forloopAsNumber "step"    =<< liftIO (get eenv (plusReg a 2))
            let next = initial + step
                cond | 0 < step = next <= limit
                     | otherwise = limit <= next
            if cond
              then do liftIO (set eenv a (Number next))
                      liftIO (set eenv (plusReg a 3) (Number next))
                      jump sBx
              else advance

       OP_FORPREP a jmp ->
         do initial <- forloopAsNumber "initial" =<< liftIO (get eenv a)
            limit   <- forloopAsNumber "limit"   =<< liftIO (get eenv (plusReg a 1))
            step    <- forloopAsNumber "step"    =<< liftIO (get eenv (plusReg a 2))
            liftIO (set eenv a (Number (initial - step)))
            liftIO (set eenv (plusReg a 1) (Number limit)) -- save back the Number form
            liftIO (set eenv (plusReg a 2) (Number step))  -- save back the Number form
            jump jmp

       OP_TFORCALL a c ->
         do f  <- liftIO (get eenv a)
            a1 <- liftIO (get eenv (plusReg a 1))
            a2 <- liftIO (get eenv (plusReg a 2))
            result <- m__call f [a1,a2]
            let resultRegs = regRange (plusReg a 3) c
            liftIO (zipWithM_ (set eenv) resultRegs (result ++ repeat Nil))
            advance

       OP_TFORLOOP a sBx ->
         do v <- liftIO (get eenv (succ a))
            if v == Nil
              then advance
              else do liftIO (set eenv a v)
                      jump sBx

       OP_SETLIST a b c -> jump =<< liftIO work
         where
         work = do tab' <- get eenv a
                   tab <- case tab' of
                            Table tab -> return tab
                            _ -> interpThrow SetListNeedsTable

                   count <-
                     if b == 0
                        then do Reg top <- getTop eenv
                                let Reg aval = a
                                return (top - aval - 1)
                        else return b

                   (offset, skip) <-
                      if c == 0
                        then do k <- getExtraArg
                                return (50*k,1)
                        else return (50 * (c-1), 0)

                   forM_ [1..count] $ \i ->
                     do x <- get eenv (plusReg a i)
                        setTableRaw tab (Number (Int (offset+i))) x

                   resetTop eenv
                   return skip

       OP_CLOSURE tgt i ->
         do (f,us) <- liftIO $
               do (fid,closureFunc) <- getProto eenv i
                  vs <- traverse (getLValue eenv) (funcUpvalues closureFunc)
                  closureUpvals <- Vector.thaw vs
                  return (luaFunction fid closureFunc, closureUpvals)

            tgt =: (Closure <$> machNewClosure f us)

       OP_VARARG a b ->
         do let varargs = execVarargs eenv
            liftIO (setCallResults eenv a b =<< readIORef varargs)
            advance

       OP_EXTRAARG{} -> liftIO $ interpThrow UnexpectedExtraArg


-- | Get a list of the `count` values stored from `start`.
getCallArguments ::
  ExecEnv -> Reg {- ^ start -} -> Count {- ^ count -} -> IO [Value]
getCallArguments eenv a b =
  do end <- case b of
       CountTop   -> getTop eenv
       CountInt x -> return (plusReg a x)
     vs <- traverse (get eenv) [a .. pred end]
     resetTop eenv
     return vs


-- | Stores a list of results into a given register range.
-- When expected is 'CountTop', values are stored up to TOP
setCallResults ::
  ExecEnv ->
  Reg     {- ^ starting register -} ->
  Count   {- ^ results expected  -} ->
  [Value] {- ^ results           -} ->
  IO ()
setCallResults eenv a b xs =
  do end <- case b of
              CountInt x -> return (plusReg a x)
              CountTop   -> top <$ setTop eenv top
                 where top = plusReg a (length xs)
     zipWithM_ (set eenv) [a .. pred end] (xs ++ repeat Nil)

-- | Allocate fresh references for all registers from the `start` to the
-- `TOP` of the stack
closeStack :: ExecEnv -> Reg {- ^ start -} -> IO ()
closeStack eenv (Reg a) =
  do let stack = execStack eenv
     n <- SV.size stack
     for_ [a .. n-1] $ \i ->
       SV.set stack i =<< newIORef Nil

-- | Interpret a value as an input to a for-loop. If the value
-- is not a number an error is raised using the given name.
forloopAsNumber :: String {- ^ argument name -} -> Value -> Mach Number
forloopAsNumber label v =
  case valueNumber v of
    Just n  -> return n
    Nothing -> luaError ("'for' " ++ label ++ " must be a number")

curLuaFunction :: ExecEnv -> IO (FunId, Function)
curLuaFunction eenv =
  case luaOpCodes (execFunction eenv) of
    Just (fid,f) -> return (fid,f)
    Nothing      -> interpThrow NonLuaFunction
------------------------------------------------------------------------
-- Reference accessors
------------------------------------------------------------------------

-- | Class for types that are indexes to references
class LValue a where
  getLValue :: ExecEnv -> a -> IO (IORef Value)

instance LValue UpIx where
  getLValue eenv (UpIx i) =
    do let upvals = execUpvals eenv
       mb <- IOVector.readMaybe upvals i
       case mb of
         Just ref -> return ref
         Nothing  -> interpThrow (BadUpval i)
  {-# INLINE getLValue #-}

instance LValue Reg where
  getLValue eenv (Reg i) = SV.get (execStack eenv) i
  {-# INLINE getLValue #-}

instance LValue Upvalue where
  getLValue eenv (UpUp  x) = getLValue eenv x
  getLValue eenv (UpReg x) = getLValue eenv x
  {-# INLINE getLValue #-}

{-# INLIEN set #-}
set :: LValue a => ExecEnv -> a -> Value -> IO ()
set eenv r !x =
  do ref <- getLValue eenv r
     writeIORef ref x

-- | Class for types that are indexes to values.
class RValue a where
  get :: ExecEnv -> a -> IO Value

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
lvalToRval :: LValue a => ExecEnv -> a -> IO Value
lvalToRval eenv r = readIORef =<< getLValue eenv r

instance RValue RK where
  get eenv (RK_Reg r) = get eenv r
  get eenv (RK_Kst k) = get eenv k
  {-# INLINE get #-}

instance RValue Kst where
  {-# INLINE get #-}
  get eenv (Kst i) =
    do (_fid,Function{..}) <- curLuaFunction eenv
       case funcConstants Vector.!? i of
         Nothing -> interpThrow (BadConstant i)
         Just x  -> constantValue x

getProto :: ExecEnv -> ProtoIx -> IO (FunId,Function)
getProto eenv (ProtoIx i) =
  do (fid,f) <- curLuaFunction eenv
     case funcProtos f Vector.!? i of
       Nothing -> interpThrow (BadProto i)
       Just x  -> return (subFun fid i,x)



------------------------------------------------------------------------
-- Binary operations
------------------------------------------------------------------------

{-# INLINE binOp #-}
binOp ::
  (RValue a, RValue b) =>
    ExecEnv -> (Value -> Value -> Mach c) -> a -> b -> Mach c
binOp eenv f src1 src2 =
  do x1 <- liftIO (get eenv src1)
     x2 <- liftIO (get eenv src2)
     f x1 x2




------------------------------------------------------------------------
-- Relational operations
------------------------------------------------------------------------

{-# INLINE relOp #-}
relOp ::
  ExecEnv ->
  (Value -> Value -> Mach Bool) ->
  Bool -> RK -> RK -> Mach Int {- ^ how many instructions to advance -}
relOp eenv (?) invert op1 op2 =
  do x1  <- liftIO (get eenv op1)
     x2  <- liftIO (get eenv op2)
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



-- | Get the register that is one beyond the last valid
-- register. This value is meaningful when dealing with
-- variable argument function calls and returns.
{-# INLINE getTop #-}
getTop :: ExecEnv -> IO Reg
getTop eenv =
  do let stack = execStack eenv
     n <- SV.size stack
     return (Reg n)

{-# INLINE resetTop #-}
resetTop :: ExecEnv -> IO ()
resetTop eenv =
  do (_,fun) <- curLuaFunction eenv
     let n = funcMaxStackSize fun
     setTop eenv (Reg n)

-- | Update TOP and ensure that the stack is large enough
-- to index up to (but excluding) TOP.
setTop :: ExecEnv -> Reg -> IO ()
setTop eenv (Reg newLen) =
  do let stack = execStack eenv
     oldLen <- SV.size stack

     if oldLen <= newLen

       -- Grow to new size
       then replicateM_ (newLen - oldLen) $
               SV.push stack =<< newIORef Nil

       -- Release references
       else SV.shrink stack (oldLen - newLen)

