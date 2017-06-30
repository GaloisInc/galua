{-# Language OverloadedStrings #-}
module Galua.Code
  ( module Galua.Code
  , module Galua.FunId
  , Count(..)
  , Literal(..)
  , DebugInfo(..)
  , VarInfo(..)
  ) where

import Data.ByteString(ByteString)
import qualified Data.ByteString.Lazy as L
import Data.Vector(Vector)
import Data.Map(Map)
import Data.Coerce(coerce)

import           Language.Lua.Bytecode (Count(..),DebugInfo(..),VarInfo(..))
import qualified Language.Lua.Bytecode as BC
import qualified Language.Lua.Bytecode.Debug as BC
import qualified Language.Lua.Bytecode.Parser as BC
import qualified Language.Lua.Bytecode.Pretty as BC
import qualified Language.Lua.Bytecode.FunId as BC

import Galua.Util.String(unpackUtf8)
import Galua.Pretty
import Galua.FunId
import Galua.ValueType(ValueType(..))

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
  } deriving Show

type Kst = Literal

data Literal = LNil
             | LBool !Bool
             | LInt {-# UNPACK #-} !Int
             | LNum {-# UNPACK #-} !Double
             | LStr {-# UNPACK #-} !ByteString
               deriving Show

literalType :: Literal -> ValueType
literalType l = case l of
                  LNil      -> NilType
                  LInt {}   -> NumberType
                  LNum {}   -> NumberType
                  LStr {}   -> StringType
                  LBool {}  -> BoolType

data RK = RK_Reg !Reg | RK_Kst !Kst
  deriving Show

data Upvalue = UpReg !Reg | UpUp !UpIx
  deriving Show

newtype Reg = Reg Int
  deriving (Eq, Ord, Show)

plusReg :: Reg -> Int -> Reg
plusReg (Reg r) i = Reg (r+i)
{-# INLINE plusReg #-}

-- | @plusReg a (regDiff a b) == b@
regDiff :: Reg -> Reg -> Int
regDiff (Reg a) (Reg b) = b - a
{-# INLINE regDiff #-}

regRange :: Reg -> Int -> [Reg]
regRange r n = [ plusReg r i | i <- take n [ 0 .. ] ]
{-# INLINE regRange #-}

regFromTo :: Reg -> Reg -> [Reg]
regFromTo (Reg reg1) (Reg regN) = [ Reg r | r <- [ reg1 .. regN ] ]
{-# INLINE regFromTo #-}

newtype UpIx = UpIx Int
  deriving (Eq, Ord, Show)

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

  | OP_SETLIST  !Reg !Count !Int !Int {- ^    A B C D  R(A)[C+i] := R(A+i), 1 <= i <= B; pc += D -}

  | OP_CLOSURE  !Reg !Int !Function   {- ^    A Bx    R(A) := closure(KPROTO[Bx])                     -}

  | OP_VARARG   !Reg !Count {- ^    A B     R(A), R(A+1), ..., R(A+B-2) = vararg            -}
  | OP_EXTRAARG !Int        {- ^    Ax      extra (larger) argument for previous opcode     -}
  deriving Show


--------------------------------------------------------------------------------

ppOpCode :: Function -> Int -> Doc
ppOpCode f pc = BC.ppOpCode (funcOrig f) pc

--------------------------------------------------------------------------------

instance Pretty UpIx where
  pp (UpIx u) = "U" <> int u

instance Pretty Reg where
  pp (Reg u) = "R" <> int u


instance Pretty Literal where
  pp l =
    case l of
      LNil -> "nil"
      LBool b -> if b then "true" else "false"
      LInt n  -> int n
      LNum n  -> double n
      LStr x  -> text (show (unpackUtf8 x))

--------------------------------------------------------------------------------

dumpLuaBytecode :: Function -> L.ByteString
dumpLuaBytecode fun =
  BC.dumpLuaBytecode BC.luaBytecodeMode53
    (BC.Chunk (length (funcUpvalues fun)) (funcOrig fun))

lookupLineNumber :: Function -> Int -> Maybe Int
lookupLineNumber f pc = BC.lookupLineNumber (funcOrig f) pc

lookupLocalName :: Function -> Int -> Reg -> Maybe ByteString
lookupLocalName f pc (Reg r) = BC.lookupLocalName (funcOrig f) pc (BC.Reg r)

deepLineNumberMap :: Function -> Map Int [(FunId,[Int])]
deepLineNumberMap x = coerce (BC.deepLineNumberMap (funcOrig x))

inferFunctionName :: Function -> Int -> Maybe ByteString
inferFunctionName f = BC.inferFunctionName (funcOrig f)

inferSubFunctionNames :: Function -> [(Int,ByteString)]
inferSubFunctionNames f = BC.inferSubFunctionNames (funcOrig f)


