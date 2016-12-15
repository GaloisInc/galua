module Galua.Code
  ( parseLuaBytecode
  , dumpLuaBytecode
  , Chunk(..)
  , Function(..)
  , Upvalue(..)
  , UpIx(..)
  , Reg(..)
  , plusReg
  , Kst
  , RK(..)
  , Count(..)
  , OpCode(..)
  , plusReg
  , regRange
  , regFromTo
  , module FunId

  -- * Pretty print
  , BC.blankPPInfo
  , BC.PP(..)
  , ppOpCode

    -- * Debug
  , DebugInfo(..)
  , VarInfo(..)
  , lookupLineNumber
  , lookupLocalName
  , deepLineNumberMap
  , inferSubFunctionNames
  , inferFunctionName
  ) where

import           Data.Map(Map)
import           Data.Vector(Vector)
import qualified Data.Vector as Vector
import           Data.ByteString(ByteString)
import qualified Data.ByteString.Lazy as L
import           Text.PrettyPrint

import           Language.Lua.Bytecode
                    (UpIx(..),ProtoIx(..),Count(..)
                    ,DebugInfo(..),VarInfo(..))
import           Language.Lua.Bytecode.FunId as FunId
import qualified Language.Lua.Bytecode as BC
import qualified Language.Lua.Bytecode.Pretty as BC
import qualified Language.Lua.Bytecode.Parser as BC
import qualified Language.Lua.Bytecode.Debug as BC

import           Galua.Value
import           Galua.Number
import           Galua.LuaString

import           Data.Coerce

parseLuaBytecode :: Maybe String -> L.ByteString -> IO (Either String Chunk)
parseLuaBytecode name bytesL =
  case BC.parseLuaBytecode name bytesL of
    Right (BC.Chunk x f) ->
      do f' <- cvtFunction (BC.propagateSources f)
         return $! Right $! Chunk x f'
    Left err -> return (Left err)

dumpLuaBytecode :: Function -> L.ByteString
dumpLuaBytecode fun =
  BC.dumpLuaBytecode BC.luaBytecodeMode53
    (BC.Chunk (length (funcUpvalues fun)) (funcOrig fun))

lookupLineNumber :: Function -> Int -> Maybe Int
lookupLineNumber f pc = BC.lookupLineNumber (funcOrig f) pc

lookupLocalName :: Function -> Int -> Reg -> Maybe ByteString
lookupLocalName f pc (Reg r) = BC.lookupLocalName (funcOrig f) pc (BC.Reg r)

deepLineNumberMap :: Function -> Map Int [(FunId,[Int])]
deepLineNumberMap x = BC.deepLineNumberMap (funcOrig x)

inferFunctionName :: Function -> Int -> Maybe ByteString
inferFunctionName f = BC.inferFunctionName (funcOrig f)

inferSubFunctionNames :: Function -> [(Int,ByteString)]
inferSubFunctionNames f = BC.inferSubFunctionNames (funcOrig f)



data Chunk = Chunk !Int !Function

data Function = Function
  { funcSource          :: !(Maybe ByteString)
  , funcLineDefined     :: !Int
  , funcLastLineDefined :: !Int
  , funcNumParams       :: !Int
  , funcIsVararg        :: !Bool
  , funcMaxStackSize    :: !Int
  , funcCode            :: !(Vector OpCode)
  , funcUpvalues        :: !(Vector Upvalue)
  , funcNested          :: !(Vector Function)
  , funcDebug           :: !DebugInfo
  , funcOrig            :: !BC.Function
  }

type Kst = Value

data RK = RK_Reg !Reg | RK_Kst !Kst

data Upvalue = UpReg !Reg | UpUp !UpIx

newtype Reg = Reg Int
  deriving (Eq, Ord, Show)

plusReg :: Reg -> Int -> Reg
plusReg (Reg r) i = Reg (r+i)

regRange :: Reg -> Int -> [Reg]
regRange = coerce BC.regRange

regFromTo :: Reg -> Reg -> [Reg]
regFromTo (Reg reg1) (Reg regN) = coerce [reg1 .. regN]

data OpCode
  = OP_MOVE     !Reg !Reg       {- ^    A B     R(A) := R(B)                                    -}
  | OP_LOADK    !Reg !Kst       {- ^    A Bx    R(A) := Kst(Bx)                                 -}
  | OP_LOADKX   !Reg !Kst       {- ^    A       R(A) := Kst   (this is here so that we know to skip the---now unused---extra arg -}
  | OP_LOADBOOL !Reg !Bool !Bool{- ^    A B C   R(A) := (Bool)B; if (C) pc++                    -}
  | OP_LOADNIL  !Reg !Int       {- ^    A B     R(A), R(A+1), ..., R(A+B) := nil                -}
  | OP_GETUPVAL !Reg !UpIx      {- ^    A B     R(A) := UpValue[B]                              -}

  | OP_GETTABUP !Reg !UpIx !RK  {- ^    A B C   R(A) := UpValue[B][RK(C)]                       -}
  | OP_GETTABLE !Reg !Reg !RK   {- ^    A B C   R(A) := R(B)[RK(C)]                             -}

  | OP_SETTABUP !UpIx !RK !RK   {- ^     A B C   UpValue[A][RK(B)] := RK(C)                     -}
  | OP_SETUPVAL !Reg !UpIx {- ^     A B     UpValue[B] := R(A)                             -}
  | OP_SETTABLE !Reg !RK !RK    {- ^    A B C   R(A)[RK(B)] := RK(C)                            -}

  | OP_NEWTABLE !Reg !Int !Int  {- ^    A B C   R(A) := {} (size = B,C)                         -}

  | OP_SELF     !Reg !Reg !RK   {- ^    A B C   R(A+1) := R(B); R(A) := R(B)[RK(C)]             -}

  | OP_ADD      !Reg !RK !RK    {- ^    A B C   R(A) := RK(B) + RK(C)                           -}
  | OP_SUB      !Reg !RK !RK    {- ^    A B C   R(A) := RK(B) - RK(C)                           -}
  | OP_MUL      !Reg !RK !RK    {- ^    A B C   R(A) := RK(B) * RK(C)                           -}
  | OP_MOD      !Reg !RK !RK    {- ^    A B C   R(A) := RK(B) % RK(C)                           -}
  | OP_POW      !Reg !RK !RK    {- ^    A B C   R(A) := RK(B) ^ RK(C)                           -}
  | OP_DIV      !Reg !RK !RK    {- ^    A B C   R(A) := RK(B) / RK(C)                           -}
  | OP_IDIV     !Reg !RK !RK    {- ^    A B C   R(A) := RK(B) // RK(C)                          -}
  | OP_BAND     !Reg !RK !RK    {- ^    A B C   R(A) := RK(B) & RK(C)                           -}
  | OP_BOR      !Reg !RK !RK    {- ^    A B C   R(A) := RK(B) | RK(C)                           -}
  | OP_BXOR     !Reg !RK !RK    {- ^    A B C   R(A) := RK(B) ~ RK(C)                           -}
  | OP_SHL      !Reg !RK !RK    {- ^    A B C   R(A) := RK(B) << RK(C)                          -}
  | OP_SHR      !Reg !RK !RK    {- ^    A B C   R(A) := RK(B) >> RK(C)                          -}
  | OP_UNM      !Reg !Reg       {- ^    A B     R(A) := -R(B)                                   -}
  | OP_BNOT     !Reg !Reg       {- ^    A B     R(A) := ~R(B)                                   -}
  | OP_NOT      !Reg !Reg       {- ^    A B     R(A) := not R(B)                                -}
  | OP_LEN      !Reg !Reg       {- ^    A B     R(A) := length of R(B)                          -}

  | OP_CONCAT   !Reg !Reg !Reg  {- ^    A B C   R(A) := R(B).. ... ..R(C)                       -}

  | OP_JMP      !(Maybe Reg) !Int {- ^    A sBx   pc+=sBx; if (A) close all upvalues >= R(A - 1)  -}
  | OP_EQ       !Bool !RK !RK     {- ^    A B C   if ((RK(B) == RK(C)) ~= A) then pc++            -}
  | OP_LT       !Bool !RK !RK     {- ^    A B C   if ((RK(B) <  RK(C)) ~= A) then pc++            -}
  | OP_LE       !Bool !RK !RK     {- ^    A B C   if ((RK(B) <= RK(C)) ~= A) then pc++            -}

  | OP_TEST     !Reg !Bool      {- ^    A C     if not (R(A) <=> C) then pc++                   -}
  | OP_TESTSET  !Reg !Reg !Bool {- ^    A B C   if (R(B) <=> C) then R(A) := R(B) else pc++     -}

  | OP_CALL     !Reg !Count !Count {- ^    A B C   R(A), ... ,R(A+C-2) := R(A)(R(A+1), ... ,R(A+B-1)) -}
  | OP_TAILCALL !Reg !Count !Count {- ^    A B C   return R(A)(R(A+1), ... ,R(A+B-1))              -}
  | OP_RETURN   !Reg !Count       {- ^    A B     return R(A), ... ,R(A+B-2)      (see note)      -}

  | OP_FORLOOP  !Reg !Int         {- ^    A sBx   R(A)+=R(A+2); if R(A) <?= R(A+1) then { pc+=sBx; R(A+3)=R(A) }-}
  | OP_FORPREP  !Reg !Int         {- ^    A sBx   R(A)-=R(A+2); pc+=sBx                           -}

  | OP_TFORCALL !Reg !Int         {- ^    A C     R(A+3), ... ,R(A+2+C) := R(A)(R(A+1), R(A+2));  -}
  | OP_TFORLOOP !Reg !Int         {- ^    A sBx   if R(A+1) ~= nil then { R(A)=R(A+1); pc += sBx }-}

  | OP_SETLIST  !Reg !Int !Int   {- ^    A B C   R(A)[(C-1)*FPF+i] := R(A+i), 1 <= i <= B        -}

  | OP_CLOSURE  !Reg !Int !Function   {- ^    A Bx    R(A) := closure(KPROTO[Bx])                     -}

  | OP_VARARG   !Reg !Count {- ^    A B     R(A), R(A+1), ..., R(A+B-2) = vararg            -}
  | OP_EXTRAARG !Int        {- ^    Ax      extra (larger) argument for previous opcode     -}

--------------------------------------------------------------------------------

cvtFunction :: BC.Function -> IO Function
cvtFunction fun =
  do nest <- mapM cvtFunction (BC.funcProtos fun)
     code <- mapM (cvtOpCode nest fun) (Vector.indexed (BC.funcCode fun))
     ups' <- mapM (cvtUpvalue fun) (BC.funcUpvalues fun)
     return Function
      { funcSource          = BC.funcSource fun
      , funcLineDefined     = BC.funcLineDefined fun
      , funcLastLineDefined = BC.funcLastLineDefined fun
      , funcNumParams       = BC.funcNumParams fun
      , funcIsVararg        = BC.funcIsVararg fun
      , funcMaxStackSize    = BC.funcMaxStackSize fun
      , funcCode            = code
      , funcUpvalues        = ups'
      , funcDebug           = BC.funcDebug fun
      , funcNested          = nest
      , funcOrig            = fun
      }

getOpCode :: BC.Function -> Int -> IO BC.OpCode
getOpCode fun pc =
  case BC.funcCode fun Vector.!? pc of
    Nothing -> fail "Malformed op-codes"
    Just op -> return op

cvtReg :: BC.Function -> BC.Reg -> IO Reg
cvtReg fun (BC.Reg r)
  | 0 <= r, r < BC.funcMaxStackSize fun = return (Reg r)
  | otherwise = fail "cvtReg: Register out of range"

cvtUpvalue :: BC.Function -> BC.Upvalue -> IO Upvalue
cvtUpvalue _   (BC.UpUp u) = pure (UpUp u)
cvtUpvalue fun (BC.UpReg r) = UpReg <$> cvtReg fun r

cvtOpCode :: Vector Function -> BC.Function -> (Int,BC.OpCode) -> IO OpCode
cvtOpCode nest fun (pc,op) =
  case op of
    BC.OP_MOVE     r1 r2      -> OP_MOVE <$> cvtReg fun r1 <*> cvtReg fun r2
    BC.OP_LOADK    r1 k       -> OP_LOADK <$> cvtReg fun r1 <*> cvtKst fun k
    BC.OP_LOADKX   r1  ->
      do BC.OP_EXTRAARG k <- getOpCode fun (pc + 1)
         OP_LOADKX <$> cvtReg fun r1 <*> cvtKst fun (BC.Kst k)
    BC.OP_LOADBOOL r1 b1 b2   -> OP_LOADBOOL <$> cvtReg fun r1 <*> pure b1 <*> pure b2
    BC.OP_LOADNIL  r1 x       -> OP_LOADNIL  <$> cvtReg fun r1 <*> pure x
    BC.OP_GETUPVAL r1 u1      -> OP_GETUPVAL <$> cvtReg fun r1 <*> pure u1

    BC.OP_GETTABUP r1 u1 rk1  -> OP_GETTABUP <$> cvtReg fun r1 <*> pure u1 <*> cvtRK fun rk1
    BC.OP_GETTABLE r1 r2 rk1  -> OP_GETTABLE <$> cvtReg fun r1 <*> cvtReg fun r2 <*> cvtRK fun rk1

    BC.OP_SETTABUP u1 rk1 rk2 -> OP_SETTABUP u1 <$> cvtRK fun rk1 <*> cvtRK fun rk2
    BC.OP_SETUPVAL r1 u1      -> OP_SETUPVAL <$> cvtReg fun r1 <*> pure u1
    BC.OP_SETTABLE r1 rk1 rk2 -> OP_SETTABLE <$> cvtReg fun r1 <*> cvtRK fun rk1 <*> cvtRK fun rk2

    BC.OP_NEWTABLE r1 x y     -> OP_NEWTABLE <$> cvtReg fun r1 <*> pure x <*> pure y

    BC.OP_SELF     r1 r2 rk   -> OP_SELF <$> cvtReg fun r1 <*> cvtReg fun r2 <*> cvtRK fun rk

    BC.OP_ADD      r1 rk1 rk2 -> OP_ADD  <$> cvtReg fun r1 <*> cvtRK fun rk1 <*> cvtRK fun rk2
    BC.OP_SUB      r1 rk1 rk2 -> OP_SUB  <$> cvtReg fun r1 <*> cvtRK fun rk1 <*> cvtRK fun rk2
    BC.OP_MUL      r1 rk1 rk2 -> OP_MUL  <$> cvtReg fun r1 <*> cvtRK fun rk1 <*> cvtRK fun rk2
    BC.OP_MOD      r1 rk1 rk2 -> OP_MOD  <$> cvtReg fun r1 <*> cvtRK fun rk1 <*> cvtRK fun rk2
    BC.OP_POW      r1 rk1 rk2 -> OP_POW  <$> cvtReg fun r1 <*> cvtRK fun rk1 <*> cvtRK fun rk2
    BC.OP_DIV      r1 rk1 rk2 -> OP_DIV  <$> cvtReg fun r1 <*> cvtRK fun rk1 <*> cvtRK fun rk2
    BC.OP_IDIV     r1 rk1 rk2 -> OP_IDIV <$> cvtReg fun r1 <*> cvtRK fun rk1 <*> cvtRK fun rk2
    BC.OP_BAND     r1 rk1 rk2 -> OP_BAND <$> cvtReg fun r1 <*> cvtRK fun rk1 <*> cvtRK fun rk2
    BC.OP_BOR      r1 rk1 rk2 -> OP_BOR  <$> cvtReg fun r1 <*> cvtRK fun rk1 <*> cvtRK fun rk2
    BC.OP_BXOR     r1 rk1 rk2 -> OP_BXOR <$> cvtReg fun r1 <*> cvtRK fun rk1 <*> cvtRK fun rk2
    BC.OP_SHL      r1 rk1 rk2 -> OP_SHL  <$> cvtReg fun r1 <*> cvtRK fun rk1 <*> cvtRK fun rk2
    BC.OP_SHR      r1 rk1 rk2 -> OP_SHR  <$> cvtReg fun r1 <*> cvtRK fun rk1 <*> cvtRK fun rk2
    BC.OP_UNM      r1 r2      -> OP_UNM  <$> cvtReg fun r1 <*> cvtReg fun r2
    BC.OP_BNOT     r1 r2      -> OP_BNOT <$> cvtReg fun r1 <*> cvtReg fun r2
    BC.OP_NOT      r1 r2      -> OP_NOT  <$> cvtReg fun r1 <*> cvtReg fun r2
    BC.OP_LEN      r1 r2      -> OP_LEN  <$> cvtReg fun r1 <*> cvtReg fun r2

    BC.OP_CONCAT   r1 r2 r3   -> OP_CONCAT <$> cvtReg fun r1 <*> cvtReg fun r2 <*> cvtReg fun r3

    BC.OP_JMP      mbR x      -> OP_JMP <$> traverse (cvtReg fun) mbR <*> pure x
    BC.OP_EQ       b rk1 rk2  -> OP_EQ  b <$> cvtRK fun rk1 <*> cvtRK fun rk2
    BC.OP_LT       b rk1 rk2  -> OP_LT  b <$> cvtRK fun rk1 <*> cvtRK fun rk2
    BC.OP_LE       b rk1 rk2  -> OP_LE  b <$> cvtRK fun rk1 <*> cvtRK fun rk2

    BC.OP_TEST     r1 b       -> OP_TEST    <$> cvtReg fun r1 <*> pure b
    BC.OP_TESTSET  r1 r2 b    -> OP_TESTSET <$> cvtReg fun r1 <*> cvtReg fun r2 <*> pure b

    BC.OP_CALL     r1 c1 c2   -> OP_CALL     <$> cvtReg fun r1 <*> pure c1 <*> pure c2
    BC.OP_TAILCALL r1 c1 c2   -> OP_TAILCALL <$> cvtReg fun r1 <*> pure c1 <*> pure c2
    BC.OP_RETURN   r1 c1      -> OP_RETURN   <$> cvtReg fun r1 <*> pure c1

    BC.OP_FORLOOP  r1 x       -> OP_FORLOOP <$> cvtReg fun r1 <*> pure x
    BC.OP_FORPREP  r1 x       -> OP_FORPREP <$> cvtReg fun r1 <*> pure x

    BC.OP_TFORCALL r1 x       -> OP_TFORCALL <$> cvtReg fun r1 <*> pure x
    BC.OP_TFORLOOP r1 x       -> OP_TFORLOOP <$> cvtReg fun r1 <*> pure x

    BC.OP_SETLIST  r1 x y     -> OP_SETLIST <$> cvtReg fun r1 <*> pure x <*> pure y

    BC.OP_CLOSURE  r1 (ProtoIx f) ->
      case nest Vector.!? f of
        Nothing -> fail "Malformed op-code"
        Just fu -> OP_CLOSURE <$> cvtReg fun r1 <*> pure f <*> pure fu

    BC.OP_VARARG   r1 c1      -> OP_VARARG <$> cvtReg fun r1 <*> pure c1

    BC.OP_EXTRAARG x          -> return $! OP_EXTRAARG x


cvtKst :: BC.Function -> BC.Kst -> IO Kst
cvtKst fun (BC.Kst n) =
  case BC.funcConstants fun Vector.!? n of
    Nothing -> fail "Bad constant in op-codes"
    Just k ->
      case k of
        BC.KNil          -> return Nil
        BC.KNum d        -> return (Number (Double d))
        BC.KInt i        -> return (Number (Int i))
        BC.KBool b       -> return (Bool b)
        BC.KString s     -> String <$> fromByteString s
        BC.KLongString s -> String <$> fromByteString s

cvtRK :: BC.Function -> BC.RK -> IO RK
cvtRK fun rk =
  case rk of
    BC.RK_Reg r -> RK_Reg <$> cvtReg fun r
    BC.RK_Kst k -> RK_Kst <$> cvtKst fun k

--------------------------------------------------------------------------------

ppOpCode :: Function -> Int -> Doc
ppOpCode f pc = BC.ppOpCode (funcOrig f) pc

