{-# Language OverloadedStrings #-}
module Galua.Code
  ( parseLuaBytecode
  , dumpLuaBytecode
  , Chunk(..)
  , Function(..)
  , Upvalue(..)
  , UpIx(..)
  , Reg(..)
  , plusReg
  , regDiff
  , Kst
  , RK(..)
  , Count(..)
  , OpCode(..)
  , Literal(..)
  , regRange
  , regFromTo
  , module FunId

  -- * Pretty print
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

import           Control.Monad (unless)
import           Data.Map(Map)
import           Data.Vector(Vector)
import qualified Data.Vector as Vector
import           Data.ByteString(ByteString)
import qualified Data.ByteString.Lazy as L
import           Text.PrettyPrint hiding (nest)

import           Language.Lua.Bytecode
                    (ProtoIx(..),Count(..)
                    ,DebugInfo(..),VarInfo(..))
import           Language.Lua.Bytecode.FunId as FunId
import qualified Language.Lua.Bytecode as BC
import qualified Language.Lua.Bytecode.Pretty as BC
import qualified Language.Lua.Bytecode.Parser as BC
import qualified Language.Lua.Bytecode.Debug as BC

import           Data.Coerce

import Galua.Pretty
import Galua.Util.String(unpackUtf8)
import {-# SOURCE #-} qualified Galua.Micro.Translate as Micro

parseLuaBytecode :: Maybe String -> L.ByteString -> IO (Either String Chunk)
parseLuaBytecode name bytesL =
  case BC.parseLuaBytecode name bytesL of
    Right (BC.Chunk x f) ->
      do f' <- cvtFunction Nothing (BC.propagateSources f)
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
  , funcMicroCode       :: !Micro.MicroFunction
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

data RK = RK_Reg !Reg | RK_Kst !Kst
  deriving Show

data Upvalue = UpReg !Reg | UpUp !UpIx
  deriving Show

newtype Reg = Reg Int
  deriving (Eq, Ord, Show)

plusReg :: Reg -> Int -> Reg
plusReg (Reg r) i = Reg (r+i)

-- | @plusReg a (regDiff a b) == b@
regDiff :: Reg -> Reg -> Int
regDiff (Reg a) (Reg b) = b - a

regRange :: Reg -> Int -> [Reg]
regRange = coerce BC.regRange

regFromTo :: Reg -> Reg -> [Reg]
regFromTo (Reg reg1) (Reg regN) = coerce [reg1 .. regN]

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

cvtFunction :: Maybe BC.Function -> BC.Function -> IO Function
cvtFunction parent fun =
  do subs <- mapM (cvtFunction (Just fun)) (BC.funcProtos fun)
     code <- mapM (cvtOpCode subs fun) (Vector.indexed (BC.funcCode fun))
     ups' <- mapM (cvtUpvalue parent) (BC.funcUpvalues fun)
     let fun1 = Function
                  { funcSource          = BC.funcSource fun
                  , funcLineDefined     = BC.funcLineDefined fun
                  , funcLastLineDefined = BC.funcLastLineDefined fun
                  , funcNumParams       = BC.funcNumParams fun
                  , funcIsVararg        = BC.funcIsVararg fun
                  , funcMaxStackSize    = BC.funcMaxStackSize fun
                  , funcCode            = code
                  , funcMicroCode       = Micro.blankMicroFunction
                  , funcUpvalues        = ups'
                  , funcDebug           = BC.funcDebug fun
                  , funcNested          = subs
                  , funcOrig            = fun
                  }
     return fun1 { funcMicroCode = Micro.translate fun1 }

getExtraOp :: BC.Function -> Int -> IO Int
getExtraOp fun pc =
  case BC.funcCode fun Vector.!? pc of
    Just (BC.OP_EXTRAARG n) -> return n
    _ -> fail "Malformed op-codes"

cvtReg :: BC.Function -> BC.Reg -> IO Reg
cvtReg fun (BC.Reg r)
  | 0 <= r, r < BC.funcMaxStackSize fun = return (Reg r)
  | otherwise = fail "cvtReg: Register out of range"

cvtUpReg :: Maybe BC.Function -> BC.Reg -> IO Reg
cvtUpReg Nothing r
  | r == BC.Reg 0 = return (Reg 0)
  | otherwise     = fail "cvtUpReg: UpReg out of range."
cvtUpReg (Just f) r = cvtReg f r

cvtUpIx :: BC.Function -> BC.UpIx -> IO UpIx
cvtUpIx fun (BC.UpIx u)
  | 0 <= u, u < length (BC.funcUpvalues fun) = return (UpIx u)
  | otherwise = fail ("cvtUpIx: Upvalue index out of range " ++
                          show (u, length (BC.funcUpvalues fun)))

cvtUpUpIx :: Maybe BC.Function -> BC.UpIx -> IO UpIx
cvtUpUpIx Nothing _ = fail "cvtUpUpIx: UpUpIx at the top level."
cvtUpUpIx (Just f) u = cvtUpIx f u

cvtUpvalue :: Maybe BC.Function -> BC.Upvalue -> IO Upvalue
cvtUpvalue parent (BC.UpUp  u) = UpUp  <$> cvtUpUpIx  parent u
cvtUpvalue parent (BC.UpReg r) = UpReg <$> cvtUpReg   parent r

extraRegChecks :: BC.Function -> OpCode -> IO ()
extraRegChecks fun op =
  case op of
    OP_FORLOOP r _     -> simple r 3
    OP_FORPREP r _     -> simple r 2
    OP_TFORLOOP r _    -> simple r 1
    OP_CALL r c1 c2    -> counted (plusReg r 1) c1 >> counted r c2
    OP_TAILCALL r c1 _ -> counted (plusReg r 1) c1
    OP_TFORCALL r c    -> counted (plusReg r 3) (CountInt c) >> simple r 2
    OP_SELF r _ _      -> simple r 1
    OP_SETLIST _ CountTop _ _ -> return ()
    OP_SETLIST r c _ _        -> counted r c
    OP_LOADNIL r c     -> counted r (CountInt (c+1))
    _ -> return ()
  where
    simple (Reg r) i =
      do let r' = r + i
         unless (0 <=r' && r' < BC.funcMaxStackSize fun)
           (fail "bad reg")

    counted (Reg _) CountTop = return ()
    counted (Reg r) (CountInt i) =
      do let r' = r + i
         unless (0 <= r && r <= r' && r' <= BC.funcMaxStackSize fun)
           (fail "bad counted reg")

extraPcChecks :: BC.Function -> Int -> OpCode -> IO ()
extraPcChecks fun pc op =
  case op of
    -- possible jumps by 1
    OP_EQ{}       -> check 1
    OP_LT{}       -> check 1
    OP_LE{}       -> check 1
    OP_LOADKX{}   -> check 1
    OP_LOADBOOL{} -> check 1
    OP_TEST{}     -> check 1
    OP_TESTSET{}  -> check 1

    -- might jump forward or backward
    OP_FORLOOP  _ x -> check 0 >> check x
    OP_TFORLOOP _ x -> check 0 >> check x

    -- always jumps by parameter
    OP_FORPREP _     x -> check x
    OP_SETLIST _ _ _ x -> check x
    OP_JMP _         x -> check x

    OP_RETURN{} -> return ()

    _ -> check 0
  where
    n = length (BC.funcCode fun)
    check rel =
      let pc' = pc + 1 + rel
      in unless (0 <= pc' && pc' < n)
           (fail "Invalid target program counter")

cvtOpCode :: Vector Function -> BC.Function -> (Int,BC.OpCode) -> IO OpCode
cvtOpCode subs fun (pc,op) =
  do op' <- cvtOpCode' subs fun pc op
     extraRegChecks fun op'
     extraPcChecks fun pc op'
     return op'

cvtOpCode' :: Vector Function -> BC.Function -> Int -> BC.OpCode -> IO OpCode
cvtOpCode' subs fun pc op =
  case op of
    BC.OP_MOVE     r1 r2      -> OP_MOVE <$> cvtReg fun r1 <*> cvtReg fun r2
    BC.OP_LOADK    r1 k       -> OP_LOADK <$> cvtReg fun r1 <*> cvtKst fun k
    BC.OP_LOADKX   r1  ->
      do k <- getExtraOp fun (pc + 1)
         OP_LOADKX <$> cvtReg fun r1 <*> cvtKst fun (BC.Kst k)
    BC.OP_LOADBOOL r1 b1 b2   -> OP_LOADBOOL <$> cvtReg fun r1 <*> pure b1 <*> pure b2
    BC.OP_LOADNIL  r1 x       -> OP_LOADNIL  <$> cvtReg fun r1 <*> pure x
    BC.OP_GETUPVAL r1 u1      -> OP_GETUPVAL <$> cvtReg fun r1 <*> cvtUpIx fun u1

    BC.OP_GETTABUP r1 u1 rk1  -> OP_GETTABUP <$> cvtReg fun r1 <*> cvtUpIx fun u1 <*> cvtRK fun rk1
    BC.OP_GETTABLE r1 r2 rk1  -> OP_GETTABLE <$> cvtReg fun r1 <*> cvtReg fun r2 <*> cvtRK fun rk1

    BC.OP_SETTABUP u1 rk1 rk2 -> OP_SETTABUP <$> cvtUpIx fun u1 <*> cvtRK fun rk1 <*> cvtRK fun rk2
    BC.OP_SETUPVAL r1 u1      -> OP_SETUPVAL <$> cvtReg fun r1 <*> cvtUpIx fun u1
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

    BC.OP_SETLIST  r1 x y ->
      do r' <- cvtReg fun r1
         let fpf = 50
             sz  = if x == 0 then CountTop else CountInt x
         if y == 0
            then do k <- getExtraOp fun (pc + 1)
                    return $! OP_SETLIST r' sz (fpf * k) 1
            else    return $! OP_SETLIST r' sz (fpf * (y - 1)) 0

    BC.OP_CLOSURE  r1 (ProtoIx f) ->
      case subs Vector.!? f of
        Nothing -> fail "Malformed op-code"
        Just fu -> OP_CLOSURE <$> cvtReg fun r1 <*> pure f <*> pure fu

    BC.OP_VARARG   r1 c1      -> OP_VARARG <$> cvtReg fun r1 <*> pure c1

    BC.OP_EXTRAARG x          -> return $! OP_EXTRAARG x


cvtKst :: BC.Function -> BC.Kst -> IO Literal
cvtKst fun (BC.Kst n) =
  case BC.funcConstants fun Vector.!? n of
    Nothing -> fail "Bad constant in op-codes"
    Just k ->
      return $!
      case k of
        BC.KNil          -> LNil
        BC.KNum d        -> LNum d
        BC.KInt i        -> LInt i
        BC.KBool b       -> LBool b
        BC.KString s     -> LStr s
        BC.KLongString s -> LStr s

cvtRK :: BC.Function -> BC.RK -> IO RK
cvtRK fun rk =
  case rk of
    BC.RK_Reg r -> RK_Reg <$> cvtReg fun r
    BC.RK_Kst k -> RK_Kst <$> cvtKst fun k

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














