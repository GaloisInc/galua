module Galua.Code
  ( parseLuaBytecode
  , dumpLuaBytecode
  , Chunk(..)
  , Function(..)
  , Upvalue(..)
  , UpIx(..)
  , Reg(..)
  , Kst
  , RK(..)
  , Count(..)
  , OpCode(..)
  , plusReg
  , regRange
  , module FunId

  -- * Pretty print
  , BC.blankPPInfo
  , BC.PP(..)
  , ppOpCode

    -- * Devug
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
                    (UpIx(..),Reg(..),ProtoIx(..),Count(..),Upvalue(..)
                    ,DebugInfo(..),VarInfo(..),plusReg,regRange)
import           Language.Lua.Bytecode.FunId as FunId
import qualified Language.Lua.Bytecode as BC
import qualified Language.Lua.Bytecode.Pretty as BC
import qualified Language.Lua.Bytecode.Parser as BC
import qualified Language.Lua.Bytecode.Debug as BC

import           Galua.Value
import           Galua.Number
import           Galua.LuaString

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
lookupLocalName f pc = BC.lookupLocalName (funcOrig f) pc

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
     return Function
      { funcSource          = BC.funcSource fun
      , funcLineDefined     = BC.funcLineDefined fun
      , funcLastLineDefined = BC.funcLastLineDefined fun
      , funcNumParams       = BC.funcNumParams fun
      , funcIsVararg        = BC.funcIsVararg fun
      , funcMaxStackSize    = BC.funcMaxStackSize fun
      , funcCode            = code
      , funcUpvalues        = BC.funcUpvalues fun
      , funcDebug           = BC.funcDebug fun
      , funcNested          = nest
      , funcOrig            = fun
      }

getOpCode :: BC.Function -> Int -> IO BC.OpCode
getOpCode fun pc =
  case BC.funcCode fun Vector.!? pc of
    Nothing -> fail "Malformed op-codes"
    Just op -> return op

cvtOpCode :: Vector Function -> BC.Function -> (Int,BC.OpCode) -> IO OpCode
cvtOpCode nest fun (pc,op) =
  case op of
    BC.OP_MOVE     r1 r2      -> return $! OP_MOVE r1 r2
    BC.OP_LOADK    r1 k       -> OP_LOADK r1 <$> cvtKst fun k
    BC.OP_LOADKX   r1  ->
      do BC.OP_EXTRAARG k <- getOpCode fun (pc + 1)
         OP_LOADKX r1 <$> cvtKst fun (BC.Kst k)
    BC.OP_LOADBOOL r1 b1 b2   -> return $! OP_LOADBOOL r1 b1 b2
    BC.OP_LOADNIL  r1 x       -> return $! OP_LOADNIL  r1 x
    BC.OP_GETUPVAL r1 u1      -> return $! OP_GETUPVAL r1 u1

    BC.OP_GETTABUP r1 u1 rk1  -> OP_GETTABUP r1 u1 <$> cvtRK fun rk1
    BC.OP_GETTABLE r1 r2 rk1  -> OP_GETTABLE r1 r2 <$> cvtRK fun rk1

    BC.OP_SETTABUP u1 rk1 rk2 -> OP_SETTABUP u1 <$> cvtRK fun rk1 <*> cvtRK fun rk2
    BC.OP_SETUPVAL r1 u1      -> return $! OP_SETUPVAL r1 u1
    BC.OP_SETTABLE r1 rk1 rk2 -> OP_SETTABLE r1 <$> cvtRK fun rk1 <*> cvtRK fun rk2

    BC.OP_NEWTABLE r1 x y     -> return $! OP_NEWTABLE r1 x y

    BC.OP_SELF     r1 r2 rk   -> OP_SELF r1 r2 <$> cvtRK fun rk

    BC.OP_ADD      r1 rk1 rk2 -> OP_ADD  r1 <$> cvtRK fun rk1 <*> cvtRK fun rk2
    BC.OP_SUB      r1 rk1 rk2 -> OP_SUB  r1 <$> cvtRK fun rk1 <*> cvtRK fun rk2
    BC.OP_MUL      r1 rk1 rk2 -> OP_MUL  r1 <$> cvtRK fun rk1 <*> cvtRK fun rk2
    BC.OP_MOD      r1 rk1 rk2 -> OP_MOD  r1 <$> cvtRK fun rk1 <*> cvtRK fun rk2
    BC.OP_POW      r1 rk1 rk2 -> OP_POW  r1 <$> cvtRK fun rk1 <*> cvtRK fun rk2
    BC.OP_DIV      r1 rk1 rk2 -> OP_DIV  r1 <$> cvtRK fun rk1 <*> cvtRK fun rk2
    BC.OP_IDIV     r1 rk1 rk2 -> OP_IDIV r1 <$> cvtRK fun rk1 <*> cvtRK fun rk2
    BC.OP_BAND     r1 rk1 rk2 -> OP_BAND r1 <$> cvtRK fun rk1 <*> cvtRK fun rk2
    BC.OP_BOR      r1 rk1 rk2 -> OP_BOR  r1 <$> cvtRK fun rk1 <*> cvtRK fun rk2
    BC.OP_BXOR     r1 rk1 rk2 -> OP_BXOR r1 <$> cvtRK fun rk1 <*> cvtRK fun rk2
    BC.OP_SHL      r1 rk1 rk2 -> OP_SHL  r1 <$> cvtRK fun rk1 <*> cvtRK fun rk2
    BC.OP_SHR      r1 rk1 rk2 -> OP_SHR  r1 <$> cvtRK fun rk1 <*> cvtRK fun rk2
    BC.OP_UNM      r1 r2      -> return $! OP_UNM  r1 r2
    BC.OP_BNOT     r1 r2      -> return $! OP_BNOT r1 r2
    BC.OP_NOT      r1 r2      -> return $! OP_NOT  r1 r2
    BC.OP_LEN      r1 r2      -> return $! OP_LEN  r1 r2

    BC.OP_CONCAT   r1 r2 r3   -> return $! OP_CONCAT   r1 r2 r3

    BC.OP_JMP      mbR x      -> return $! OP_JMP mbR x
    BC.OP_EQ       b rk1 rk2  -> OP_EQ  b <$> cvtRK fun rk1 <*> cvtRK fun rk2
    BC.OP_LT       b rk1 rk2  -> OP_LT  b <$> cvtRK fun rk1 <*> cvtRK fun rk2
    BC.OP_LE       b rk1 rk2  -> OP_LE  b <$> cvtRK fun rk1 <*> cvtRK fun rk2

    BC.OP_TEST     r1 b       -> return $! OP_TEST     r1 b
    BC.OP_TESTSET  r1 r2 b    -> return $! OP_TESTSET  r1 r2 b

    BC.OP_CALL     r1 c1 c2   -> return $! OP_CALL     r1 c1 c2
    BC.OP_TAILCALL r1 c1 c2   -> return $! OP_TAILCALL r1 c1 c2
    BC.OP_RETURN   r1 c1      -> return $! OP_RETURN   r1 c1

    BC.OP_FORLOOP  r1 x       -> return $! OP_FORLOOP  r1 x
    BC.OP_FORPREP  r1 x       -> return $! OP_FORPREP  r1 x

    BC.OP_TFORCALL r1 x       -> return $! OP_TFORCALL r1 x
    BC.OP_TFORLOOP r1 x       -> return $! OP_TFORLOOP r1 x

    BC.OP_SETLIST  r1 x y     -> return $! OP_SETLIST  r1 x y

    BC.OP_CLOSURE  r1 (ProtoIx f) ->
      case nest Vector.!? f of
        Nothing -> fail "Malformed op-code"
        Just fu -> return $! OP_CLOSURE r1 f fu

    BC.OP_VARARG   r1 c1      -> return $! OP_VARARG r1 c1

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
    BC.RK_Reg r -> return (RK_Reg r)
    BC.RK_Kst k -> RK_Kst <$> cvtKst fun k

--------------------------------------------------------------------------------

ppOpCode :: Function -> Int -> Doc
ppOpCode f pc = BC.ppOpCode (funcOrig f) pc

