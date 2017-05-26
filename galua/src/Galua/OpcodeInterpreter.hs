{-# LANGUAGE BangPatterns #-}
module Galua.OpcodeInterpreter(execute) where

import           Control.Exception hiding (Handler)
import           Control.Monad
import           Data.Foldable
import           Data.IORef
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as IOVector

import           Galua.Code
import           Galua.Value
import           Galua.FunValue
import           Galua.Overloading
import           Galua.Mach
import           Galua.Number
import           Galua.LuaString(fromByteString)
import           Galua.Util.SmallVec(SmallVec)
import qualified Galua.Util.SmallVec as SMV

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
{-# INLINE execute #-}
execute :: VM -> Int -> IO NextStep
execute !vm !pc =

  do eenv <- case vmCurExecEnv vm of
               ExecInLua lenv -> return lenv
               ExecInMLua {}  -> interpThrow ExecuteLuaWhileInMLua
               ExecInC {}     -> interpThrow ExecuteLuaWhileInC

     let advance      = jump 0
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
         do xs  <- getRegsFromTo eenv start end
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
            let after (f,as) = return $! FunTailcall f as
            resolveFunction tabs after u args

       OP_RETURN a c -> do vs <- getCallArguments eenv a c
                           return $! FunReturn vs

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
            m__call tabs after f (SMV.vec2 a1 a2)

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

            let setTab i x = setTableRaw tab (Number (Int (offset+i))) x

            case b of
              CountInt count ->
                forM_ [1..count] $ \i ->
                  setTab i =<< get eenv (plusReg a i)

              CountTop ->
                do vrs <- readIORef (luaExecVarress eenv)
                   case vrs of
                     NoVarResults -> interpThrow MissingVarResults
                     VarResults c vs ->
                       do let count = regDiff a c - 1
                          forM_ [1..count] $ \i ->
                             setTab i =<< get eenv (plusReg a i)
                          SMV.iForM_ vs $ \i v ->
                             setTab (count+1+i) v
                          writeIORef (luaExecVarress eenv) NoVarResults
            jump skip

       OP_CLOSURE tgt ix cloFunc ->
         do let ups   = funcUpvalues cloFunc

            closureUpvals <- IOVector.new (Vector.length ups)
            forM_ (Vector.indexed ups) $ \(i,e) ->
               IOVector.unsafeWrite closureUpvals i =<< getLValue eenv e

            let fid = luaExecFID eenv
                f   = luaFunction (subFun fid ix) cloFunc
            tgt =: (Closure <$> machNewClosure vm f closureUpvals)

       OP_VARARG a b ->
         do let varargs = luaExecVarargs eenv
            setCallResults eenv a b . SMV.fromList =<< readIORef varargs
            advance

       OP_EXTRAARG{} -> interpThrow UnexpectedExtraArg


-- | Get a list of the `count` values stored from `start`.
{-# INLINE getCallArguments #-}
getCallArguments ::
  LuaExecEnv -> Reg {- ^ start -} -> Count {- ^ count -} -> IO (SmallVec Value)
getCallArguments eenv a b =
  case b of
    CountInt x -> getRegsFromLen eenv a x
    CountTop ->
      do vrs <- readIORef (luaExecVarress eenv)
         case vrs of
           NoVarResults -> interpThrow MissingVarResults
           VarResults c vs
             | a < c ->
                  do xs <- getRegsFromTo eenv a (plusReg c (-1))
                     writeIORef (luaExecVarress eenv) NoVarResults
                     return (xs SMV.++ vs)
             | otherwise ->
                  do writeIORef (luaExecVarress eenv) NoVarResults
                     return vs

-- | Stores a list of results into a given register range.
-- When expected is 'CountTop', values are stored up to TOP
{-# INLINE setCallResults #-}
setCallResults ::
  LuaExecEnv ->
  Reg           {- ^ starting register -} ->
  Count         {- ^ results expected  -} ->
  SmallVec Value  {- ^ results           -} ->
  IO ()
setCallResults eenv a b xs =
  case b of
    CountInt count ->
      SMV.ipadForM_ xs count Nil $ \i v ->
        set eenv (plusReg a i) v

    CountTop ->
      do writeIORef (luaExecVarress eenv) $! VarResults a xs
         let end = Reg (IOVector.length (luaExecRegs eenv) - 1)
         for_ (regFromTo a end) $ \(Reg i) ->
           IOVector.unsafeWrite (luaExecRegs eenv) i =<< newIORef Nil



-- | Allocate fresh references for all registers from the `start` to the
-- `TOP` of the stack
{-# INLINE closeStack #-}
closeStack :: LuaExecEnv -> Reg {- ^ start -} -> IO ()
closeStack eenv start =
  do let regs = luaExecRegs eenv
     vrs <- readIORef (luaExecVarress eenv)
     let end = case vrs of
                 NoVarResults   -> Reg (IOVector.length regs - 1)
                 VarResults b _ -> plusReg b (-1)
     for_ (regFromTo start end) $ \(Reg i) ->
       IOVector.unsafeWrite regs i =<< newIORef Nil

-- | Interpret a value as an input to a for-loop. If the value
-- is not a number an error is raised using the given name.
forloopAsNumber :: String {- ^ argument name -} -> Value ->
                  (Number -> IO NextStep) -> IO NextStep
forloopAsNumber label v cont =
  case valueNumber v of
    Just n  -> cont n
    Nothing -> luaError' ("'for' " ++ label ++ " must be a number")
{-# INLINE forloopAsNumber #-}


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
  getLValue eenv (Reg i) = IOVector.unsafeRead (luaExecRegs eenv) i
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


{-# INLINE getRegsFromLen #-}
getRegsFromLen :: LuaExecEnv -> Reg -> Int -> IO (SmallVec Value)
getRegsFromLen eenv (Reg r) len =
  do let regs = luaExecRegs eenv
         area = IOVector.unsafeSlice r len regs
     SMV.generateM len (\i -> readIORef =<< IOVector.unsafeRead area i)
     -- maybe unsafeFreeze?

{-# INLINE getRegsFromTo #-}
getRegsFromTo :: LuaExecEnv -> Reg -> Reg -> IO (SmallVec Value)
getRegsFromTo eenv (Reg r1) (Reg r2) =
  getRegsFromLen eenv (Reg r1) (r2 - r1 + 1)


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
  = UnexpectedExtraArg
  | SetListNeedsTable
  | ExecuteLuaWhileInC
  | ExecuteLuaWhileInMLua
  | MissingVarResults
  deriving (Show)

instance Show InterpreterFailure where
  show (InterpreterFailure {-loc-} e) = unlines
    ( "Interpreter failed!"
    : [("Exception: " ++ show e)]
    -- : ""
    -- : map prettyLocationInfo loc
    )




