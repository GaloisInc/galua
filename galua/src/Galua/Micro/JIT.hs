{-# Language OverloadedStrings #-}
module Galua.Micro.JIT (jit) where

import Text.PrettyPrint
import qualified Data.Vector as Vector
import qualified Data.Map as Map
import           Data.ByteString(ByteString)


import           Galua.Mach(CompiledFunction)
import           Galua.Pretty(pp)

import           Galua.Micro.AST
import           Galua.Micro.Translate(translate)
import           Galua.Code hiding (Reg(..))
import qualified Galua.Code as Code (Reg(..),UpIx(..),Function(..))
import qualified Galua.Micro.Compile as GHC (compile)



jit :: FunId -> Function -> IO CompiledFunction
jit fid f = GHC.compile modName code
  where
  modName    = "Lua_" ++ funIdString fid
  code = compile modName f


{-

Read only values:

  * vm       :: VM

  * errRef   :: IORef Value

  * upvalues :: IOVector (IORef Value)
  * fid      :: Value
  * fun      :: Function
  * args0    :: SmallVec Value

-}


compile :: String -> Function -> HsModule
compile modName func = vcat $
  [ "{-# Language BangPatterns, OverloadedStrings #-}"
  , "module" <+> text modName <+> "(main) where"
  , "import Data.Maybe"
  , "import Data.IORef"
  , "import qualified Data.Map as Map"
  , "import           Data.Vector.Mutable(IOVector)"
  , "import qualified Data.Vector.Mutable as IOVector"
  , "import qualified Data.Vector as Vector"
  , "import qualified Data.ByteString as BS"
  , ""
  , "import Galua.Mach"
  , "import Galua.Value"
  , "import Galua.FunValue"
  , "import Galua.Number"
  , "import Galua.LuaString"
  , "import Galua.Code(Function(funcNested))"
  , "import qualified Galua.Code as Code"
  , "import           Galua.Util.SmallVec (SmallVec)"
  , "import qualified Galua.Util.SmallVec as SMV"
  , ""
  , declare
  , ""
  ] ++
  [ "main :: Reference Closure -> VM -> SmallVec Value -> IO NextStep"
  , "main cloRef vm args0 ="
  , nest 2 $ doBlock $
      [ "let MkClosure { cloFun = LuaFunction fid fun"
      , "              , cloUpvalues = upvalues }  = referenceVal cloRef"

      , "errRef <- newIORef (error \"[bug]: used `errRef`\")"
      , "let" <+> initState
      , "state <-" <+> setRegList ArgReg "args0"
      ] ++
     [ enterBlock EntryBlock ]
  ] ++
  [ blockDecl k v $$ " "
          | (k,v) <- Map.toList (functionCode mf)
  ]
  where
  (declare,initState) = stateDecl (functionRegsTMP mf) func
  mf = translate func


{-

The state:

@
data State = State
  { args  :: !(SmallVec Value)
  , list  :: !(SmallVec Value)
  , reg_0 :: !Value
  , ...
  , ref_0 :: {-# UNPACK #-} !(IORef Value)
  , ...
  , tmp_0 :: !Value
  , ...
  }
@

-}

stateDecl :: Int -> Function -> (HsDecl, HsDecl)
stateDecl tmpNum fun =
  ( "data State = State" $$ nest 2 (block fieldTys $$ "}")
  , "state = State" $$ nest 2 (block fieldVals $$ "}")
  )
  where
  regNum  = funcMaxStackSize fun

  rVals = [ regValName (Reg r) | r <- regRange (Code.Reg 0) regNum ]
  rRefs = [ regRefName (Reg r) | r <- regRange (Code.Reg 0) regNum ]
  rTMPs = [ regValName (TMP 3 i) | i <- take tmpNum [ 0 .. ] ]
  rLsts = [ regListName r | r <- [ ArgReg, ListReg ] ]


  fieldTys  = [ r <+> ":: !Value"                        | r <- rVals ] ++
              [ r <+> ":: {-# UNPACK #-} !(IORef Value)" | r <- rRefs ] ++
              [ r <+> ":: !Value"                        | r <- rTMPs ] ++
              [ r <+> ":: !(SmallVec Value)"             | r <- rLsts ]

  fieldVals = [ r <+> "= Nil"       | r <- rVals ] ++
              [ r <+> "= errRef"    | r <- rRefs ] ++
              [ r <+> "= Nil"       | r <- rTMPs ] ++
              [ r <+> "= SMV.empty" | r <- rLsts ]

  block fs = case fs of
               [] -> "{"
               x : xs -> ("{" <+> x) $$ vcat (map ("," <+>) xs)





--------------------------------------------------------------------------------
-- Names
--------------------------------------------------------------------------------

-- | The name of a register holding a value.
regValName :: Reg -> Name
regValName reg =
  case reg of
    Reg (Code.Reg r)  -> "reg_val_" <> int r
    TMP _flat_phase n -> "reg_tmp_" <> int n

-- | The name of a register holding a reference.
regRefName :: Reg -> Name
regRefName reg =
  case reg of
    Reg (Code.Reg r)  -> "reg_ref_" <> int r
    TMP {}            -> error "refRefName: TMP"

-- | The name of a "list" register.  These are the special registers
-- holding the inputs and ouputs to functional calls.
regListName :: ListReg -> Name
regListName r =
  case r of
    ArgReg  -> text "reg_args"
    ListReg -> text "reg_list"

-- | The name of a block.
blockNameName :: BlockName -> Name
blockNameName bn =
  case bn of
    EntryBlock    -> "block_entry"
    PCBlock pc    -> "block_pc_" <> int pc
    NewBlock pc i -> "block_pc_" <> int pc <> "_" <> int i




{- | Compilation of a block:

High-level paln:

@
bloackName :: State -> IO NextStep
bloackName state = do state <- $(compile stmt1)
                      state <- $(compile stmt2)
                      ...
                      $(compile endOfBlock)
@

There is a special case for the handling of function calls:
when a function is called, we want to stop and go back to the environment,
so that we give it an option to execute the function in some other way
(e.g., slowly, step-by-step).   To do this we return a `FunCall` next step,
and its continuation we instert the remaining statements of the block,
so that whenever the caller returns back to us, the rest of the block
will be executed.
-}

blockDecl :: BlockName -> Block -> HsDecl
blockDecl bn stmts =
  name <+> ":: VM -> IORef Value -> IOVector (IORef Value) -> FunId -> Function -> State -> IO NextStep" $$
  name <+> "!vm !errRef !upvalues !fid !fun !state = do" <+> vcat (goBlocks stmtList)
  where
  name     = blockNameName bn
  stmtList = map stmtCode (Vector.toList (blockBody stmts))

  goBlocks [] = [ stmtEndStmt (stmtCode (blockEnd stmts)) ]
  goBlocks (s : ss) =
    case s of
      Call f -> [ performCall f (goBlocks ss) ]
      _      -> stmtStmt s : goBlocks ss


-- | Compilation of an end statement.
stmtEndStmt :: EndStmt -> HsStmt
stmtEndStmt stmt =
  case stmt of
    -- Local control flow
    Case e as d             -> performCase e as d
    If p tr fa              -> performIf p tr fa
    Goto b                  -> performGoto b

    TailCall clo            -> performTailCall clo
    Return                  -> performReturn
    Raise e                 -> performRaise e




-- | Compilation of a statement.
stmtStmt :: Stmt -> HsStmt
stmtStmt stmt =
  case stmt of
    Comment s               -> "{-" <+> text s <+> "-}" -- XXX: escape `s`?
    Assign r e              -> performAssign r e

    -- Tables
    NewTable r              -> performNewTable r
    LookupTable res tab ix  -> performLookupTable res tab ix

    SetTable tab ix val     -> performSetTable tab ix val
    SetTableList tab ix     -> performSetTableList tab ix
    GetMeta r e             -> performGetMeta r e

    -- Functions
    NewClosure res ix fun   -> performNewClosure res ix fun

    -- Lists
    Drop r n                -> performDrop r n
    SetList res es          -> performSetList res es
    Append res es           -> performAppend res es
    IndexList res lst ix    -> performIndexList res lst ix

    -- References
    NewRef res val          -> performNewRef res val
    ReadRef res ref         -> performReadRef res ref
    WriteRef ref val        -> performWriteRef ref val

    -- Arithmetic
    Arith1 res op e         -> performArith1 res op e
    Arith2 res op e1 e2     -> performArith2 res op e1 e2

    -- Misc

    -- Errors
    Call _                  -> error "Call statement is special."
    CloseStack _            -> error "CloseStack in phase 2"
    SetUpVal _ _            -> error "SetUpVal in phase 2"



-- | Get the value of a register.
-- Free vars: 'state :: State'
-- Type: 'Value'.
getReg :: Reg -> HsExpr
getReg r = regValName r <+> "state"

-- | Set a register to a given value.
-- Free vars: 'state :: State, errRef :: IORef Value'
-- Type: 'IO State'
setReg :: Reg -> HsExpr -> HsExpr
setReg r e =
  case r of
    TMP {} -> "return $! state {" <+> regValName r <+> "=" <+> e <+> "}"
    Reg {} -> "return $! state {" <+> regValName r <+> "=" <+> e <+>
                              "," <+> regRefName r <+> "= errRef }"

-- | Get the reference stored in a register.
-- Type: 'IORef Value'
getRegRef :: Reg -> HsExpr
getRegRef r = regRefName r <+> "state"

-- | Set a registesr to a given reference.
-- Free vars: 'state :: State'
-- Type: 'IO State'
setRegRef :: Reg -> HsExpr -> HsExpr
setRegRef r e =
  "return $! state {" <+> regValName r <+> "= Nil" <+>
                  "," <+> regRefName r <+> "=" <+> e <+> "}"


-- | Get the value from one of the list register.
--   Type: 'SmallVec Value'
getRegList :: ListReg -> HsExpr
getRegList r = regListName r <+> "state"

-- | Get the value from the result register.
--   Type: 'SmallVec Value'
getListReg :: HsExpr
getListReg = getRegList ListReg


setRegList :: ListReg -> HsExpr -> HsExpr
setRegList x e = "return $! state {" <+> regListName x <+> "=" <+> e <+> "}"

-- | Get the value of an expression.
-- Type: 'IO Value'
-- The IO is used so that we can allocate string litterals.
getExpr :: Expr -> HsExpr
getExpr expr =
  case expr of
    EReg r -> "return" <+> parens (getReg r)
    EUp {} -> error "getExpr: upvalues are references"
    ELit l ->
      case l of
        LNil           -> "return Nil"
        LBool b        -> "return" <+> val "Bool" b
        LNum d         -> "return" <+> val "Number" (val "Double" d)
        LInt n         -> "return" <+> val "Number" (val "Int" n)
        LStr bs        -> "String <$> fromByteString" <+> text (show bs)
                 -- hopefully all this inlines to something reasonable?
  where
  val c x = parens (c <+> text (show x))


-- | Get the value of a reference expression.
-- Type: 'IO Value'
getRefExpr :: Expr -> HsExpr
getRefExpr expr =
  case expr of
    EReg r             -> "return" <+> parens (getRegRef r)
    EUp (Code.UpIx x)  -> "IOVector.unsafeRead upvalues" <+> int x
    ELit {}            -> error "Bug: there should be not reference literals."



-- | Emit code to start executing the given block.
-- Type: IO NextStep
enterBlock :: BlockName -> HsStmt
enterBlock b = blockNameName b <+> "vm errRef upvalues fid fun state"

-- | Compile a decision.
-- Type: IO Bool
getProp :: Prop -> HsExpr
getProp (Prop pre ~(v1 : ~(v2 : _))) =
  case pre of
    IsNone ->
      doBlock
        [ "v <-" <+> getExpr v1
        , "return (case v of"
        , "          Bool b -> b"
        , "          Nil    -> False"
        , "          _      -> True)"
        ]

    IsInteger ->
      doBlock
        [ "Number n <-" <+> getExpr v1
        , "return (case n of"
        , "          Int {}    -> True"
        , "          Double {} -> False)"
        ]

    IsNaN ->
      doBlock
        [ "Number n <-" <+> getExpr v1
        , "return (case n of"
        , "          Int {}   -> False"
        , "          Double d -> isNaN d)"
        ]

    Equals ->
      doBlock
        [ "v1 <-" <+> getExpr v1
        , "v2 <-" <+> getExpr v2
        , "return (v1 == v2)"
        ]

    NumberLT ->
      doBlock
        [ "Number n1 <-" <+> getExpr v1
        , "Number n2 <-" <+> getExpr v2
        , "return (n1 < n2)"
        ]

    NumberLEQ ->
      doBlock
        [ "Number n1 <-" <+> getExpr v1
        , "Number n2 <-" <+> getExpr v2
        , "return (n1 <= n2)"
        ]

    StringLT -> -- XXX: avoid allocating string literals
      doBlock
        [ "String s1 <-" <+> getExpr v1
        , "String s2 <-" <+> getExpr v2
        , "return (s1 < s2)"
        ]

    StringLEQ ->  -- XXX: avoid allocating string literals
      doBlock
        [ "String s1 <-" <+> getExpr v1
        , "String s2 <-" <+> getExpr v2
        , "return (s1 <= s2)"
        ]






innerBlockStmt :: [Doc] -> Doc
innerBlockStmt code = "state <-" <+> doBlock code

finalBlockStmt :: [Doc] -> Doc
finalBlockStmt code = vcat code


performAssign :: Reg -> Expr -> HsStmt
performAssign r e =
  innerBlockStmt
    [ "v <-" <+> getExpr e
    , setReg r "v"
    ]

performNewTable :: Reg -> HsStmt
performNewTable r =
  innerBlockStmt
    [ "v <- machNewTable vm 0 0"
    , setReg r "Table v"
    ]

strLab :: ByteString -> HsExpr
strLab x = "String" <+> parens ("unsafeFromByteString" <+> text (show x))

performLookupTable :: Reg -> Reg -> Expr -> HsStmt
performLookupTable res tab (ELit (LStr x)) =
  innerBlockStmt
    [ "let Table t =" <+> getReg tab
    , "v          <- getTableRaw t" <+> parens (strLab x)
    , setReg res "v"
    ]

performLookupTable res tab ix =
  innerBlockStmt
    [ "let Table t =" <+> getReg tab
    , "ix         <-" <+> getExpr ix
    , "v          <- getTableRaw t ix"
    , setReg res "v"
    ]

performSetTable :: Reg -> Expr -> Expr -> HsStmt
performSetTable tab (ELit (LStr x)) val =
  innerBlockStmt
    [ "let Table t =" <+> getReg tab
    , "val        <-" <+> getExpr val
    , "setTableRaw t" <+> parens (strLab x) <+> "val"
    , "return state"
    ]

performSetTable tab ix val =
  innerBlockStmt
    [ "let Table t =" <+> getReg tab
    , "ix         <-" <+> getExpr ix
    , "val        <-" <+> getExpr val
    , "setTableRaw t ix val"
    , "return state"
    ]

performSetTableList :: Reg -> Int -> HsStmt
performSetTableList tab ix =
  innerBlockStmt
    [ "let Table t =" <+> getReg tab
    , "    xs      =" <+> getListReg
    , "iForM_ xs $ \\i a -> setTableRaw t (Number (Int (i" <+> int ix <+>"))) a"
    , "return state"
    ]

performGetMeta :: Reg -> Expr -> HsStmt
performGetMeta r e =
  innerBlockStmt
    [ "v <-" <+> getExpr e
    , "let setMb mb =" <+> setReg r "maybe Nil Table mb"
    , "case v of"
    , "  Table    tr -> setMb =<< getTableMeta tr"
    , "  UserData ur -> setMb =<< readIORef (userDataMeta (referenceVal ur))"
    , "  _           -> do let ty = valueType v"
    , "                    tabs <- readIORef (machMetatablesRef (vmMachineEnv vm))"
    , "                    setMb (Map.lookup ty tabs)"
    ]

performRaise :: Expr -> HsStmt
performRaise e =
  finalBlockStmt
    [ "v <-" <+> getExpr e
    , "return $! ThrowError v"
    ]


performCase :: Expr -> [(ValueType,BlockName)] -> Maybe BlockName -> HsStmt
performCase e as d =
  finalBlockStmt
    [ "v <-" <+> getExpr e
    , "case valueType v of" $$ nest 2 (vcat branches)
    ]

  where
  pats      = [ tyPat t <+> "->" <+> enterBlock b | (t,b) <- as ]
  branches  = case d of
                Nothing -> pats
                Just b  -> pats ++ ["_ ->" <+> enterBlock b]

  tyPat t   = text (show t)


performIf :: Prop -> BlockName -> BlockName -> HsStmt
performIf p tr fa =
  finalBlockStmt
    [ "b <-" <+> getProp p
    , "if b then" <+> enterBlock tr <+> "else" <+> enterBlock fa
    ]

performGoto :: BlockName -> HsStmt
performGoto b =
  finalBlockStmt
    [ enterBlock b
    ]


performCall :: Reg -> [HsStmt] -> HsStmt
performCall clo after =
  finalBlockStmt -- kind of, the rest of the block is captured.
    [ "let Closure newFun =" <+> getReg clo
    , "    args        =" <+> getListReg
    , "return $! FunCall newFun args Nothing $ \\result ->"
    , nest 2 $ doBlock (setRes : after)
    ]
  where
  setRes = "state <-" <+> setRegList ListReg "result"


performTailCall :: Reg -> HsStmt
performTailCall clo =
  finalBlockStmt
    [ "let Closure newFun =" <+> getReg clo
    , "let vs =" <+> getListReg
    , "return $! FunTailcall newFun vs"
    ]


performReturn :: HsStmt
performReturn =
  finalBlockStmt
    [ "let vs =" <+> getListReg
    , "return $! FunReturn vs"
    ]


performNewClosure :: Reg -> Int -> Code.Function -> HsStmt
performNewClosure res ix fun =
  innerBlockStmt $
    [ u <+> "<-" <+> getRefExpr ue | (u,ue) <- zip unames upexprs ] ++
    [ "ups <- Vector.thaw (Vector.fromListN" <+> int upNum <+>
                                                      listLit unames <+> ")"
    , "let fu = luaFunction" <+> parens ("Code.subFun fid" <+> int ix)
           <+> parens ("Vector.unsafeIndex (funcNested fun)" <+> int ix)
    , "c <- machNewClosure vm fu ups"
    , setReg res "Closure c"
    ]
  where
  upexprs = funcUpvalExprs fun
  upNum   = length upexprs
  unames  = [ "u" <> int i | i <- take upNum [ 0 .. ] ]



performDrop :: ListReg -> Int -> HsStmt
performDrop res n =
  innerBlockStmt
    [ "let xs =" <+> getRegList res
    , setRegList res ("SMV.drop" <+> int n <+> "xs")
    ]


performSetList :: ListReg -> [Expr] -> HsStmt
performSetList res es =
  innerBlockStmt $
    [ v <+> "<-" <+> getExpr e | (v,e) <- zip vs es ] ++
    [ setRegList res (vecLit vs) ]
  where
  vs  = [ "v" <> int i | i <- take (length es) [ 0 .. ] ]


performAppend :: ListReg -> [Expr] -> HsStmt
performAppend res es =
  innerBlockStmt $
    [ v <+> "<-" <+> getExpr e | (v,e) <- zip vs es ] ++
    [ "let xs =" <+> getRegList res
    , setRegList res (vecLit vs <+> "SMV.++ xs")
    ]
  where
  vs  = [ "v" <> int i | i <- take (length es) [ 0 .. ] ]


performIndexList :: Reg -> ListReg -> Int -> HsStmt
performIndexList res lst ix =
  innerBlockStmt
    [ "let vs =" <+> getRegList lst
    , setReg res ("SMV.indexWithDefault vs Nil" <+> int ix)
    ]

performNewRef :: Reg -> Expr -> HsStmt
performNewRef res val =
  innerBlockStmt
    [ "v   <-" <+> getExpr val
    , "ref <- newIORef v"
    , setRegRef res "ref"
    ]


performReadRef :: Reg -> Expr -> HsStmt
performReadRef res ref =
  innerBlockStmt
    [ "ref <-" <+> getRefExpr ref
    , "v   <- readIORef ref"
    , setReg res "v"
    ]


performWriteRef :: Expr -> Expr -> HsStmt
performWriteRef ref val =
  innerBlockStmt
    [ "ref <-" <+> getRefExpr ref
    , "v   <-" <+> getExpr val
    , "writeIORef ref v"
    , "return state"
    ]


performArith1 :: Reg -> Op1 -> Expr -> HsStmt
performArith1 res op e =
  innerBlockStmt ([ "v <-" <+> getExpr e ] ++ doOp)
  where
  doOp =
    case op of
      ToNumber ->
        [ "let mb = valueNumber v"
        , setReg res "maybe Nil Number mb"
        ]

      ToInt ->
        [ "let mb = valueInt v"
        , setReg res "fromMaybe Nil (Number . Int) mb"
        ]

      IntToDouble ->
        [ "let Number (Int n) = v"
        , setReg res "Number (Double (fromIntegral n))"
        ]

      ToString ->
        [ "let mb = valueString v"
        , "case mb of"
        , "  Just s -> do v1 <- fromByteString s"
        , "               " <> setReg res "String v1"
        , "  Nothing ->" <+> setReg res "Nil"
        ]


      ToBoolean ->
        [ setReg res "Bool (valueBool v)" ]

      StringLen ->
        [ "let String s = v"
        , setReg res "Number (Int (luaStringLen s))"
        ]

      TableLen ->
        [ "let Table t = v"
        , "n <- tableLen t"
        , setReg res "Number (Int n)"
        ]

      NumberUnaryMinus ->
        [ "let Number n = v"
        , setReg res "Number (neagate n)"
        ]

      Complement  ->
        [ "let Number (Int n) = v"
        , setReg res "Number (Int (complement n))"
        ]

      BoolNot ->
        [ "let Bool b = v"
        , setReg res "Bool (not b)"
        ]


performArith2 :: Reg -> Op2 -> Expr -> Expr -> HsExpr
performArith2 res op e1 e2 =
  innerBlockStmt $
    [ "v1 <-" <+> getExpr e1
    , "v2 <-" <+> getExpr e2
    ] ++ doOp
  where
  doOp =
    case op of

      NumberAdd ->
        [ "let Number n1 = v1"
        , "    Number n2 = v2"
        , setReg res "Number (n1 + n2)"
        ]

      NumberSub ->
        [ "let Number n1 = v1"
        , "    Number n2 = v2"
        , setReg res "Number (n1 - n2)"
        ]

      NumberMul ->
        [ "let Number n1 = v1"
        , "    Number n2 = v2"
        , setReg res "Number (n1 * n2)"
        ]

      NumberPow ->
        [ "let Number n1 = v1"
        , "    Number n2 = v2"
        , setReg res "Number (numberPow n1 n2)"
        ]

      IMod ->
        [ "let Number (Int n1) = v1"
        , "    Number (Int n2) = v2"
        , setReg res "Number (Int (mod n1 n2))"
        ]

      FMod ->
        [ "let Number (Double n1) = v1"
        , "    Number (Double n2) = v2"
        , setReg res "Number (Double (nummod n1 n2))"
        ]

      IDiv ->
        [ "let Number (Int n1) = v1"
        , "    Number (Int n2) = v2"
        , setReg res "Number (Int (div n1 n2))"
        ]

      NumberDiv ->
        [ "let Number n1 = v1"
        , "    Number n2 = v2"
        , setReg res "Number (numberDiv n1 n2)"
        ]

      And ->
        [ "let Number (Int n1) = v1"
        , "    Number (Int n2) = v2"
        , setReg res "Number (Int (n1 .&. n2))"
        ]

      Or ->
        [ "let Number (Int n1) = v1"
        , "    Number (Int n2) = v2"
        , setReg res "Number (Int (n1 .|. n2))"
        ]

      Xor ->
        [ "let Number (Int n1) = v1"
        , "    Number (Int n2) = v2"
        , setReg res "Number (Int (xor n1 n2))"
        ]

      Shl ->
        [ "let Number (Int n1) = v1"
        , "    Number (Int n2) = v2"
        , setReg res "Number (Int (wordshiftL n1 n2))"
        ]

      Shr ->
        [ "let Number (Int n1) = v1"
        , "    Number (Int n2) = v2"
        , setReg res "Number (Int (wordshiftR n1 n2))"
        ]

      Concat ->
        [ "let String s1 = v1"
        , "    String s2 = v2"
        , "    x         = toByteString s1"
        , "    y         = toByteString s2"
        , "z <- if BS.null x then return s1 else"
        , "     if BS.null y then return s2 else"
        , "                       fromByteString (BS.append x y)"
        , setReg res "String z"
        ]




--------------------------------------------------------------------------------
-- A primitive way of emitting Haskell code.

type HsModule = Doc
type HsDecl   = Doc
type HsStmt   = Doc   -- `do` stmt or expr
type HsExpr   = Doc
type Name     = Doc

doBlock :: [Doc] -> Doc
doBlock ds = text "do" <+> vcat ds

listLit :: [Doc] -> Doc
listLit = brackets . hsep . punctuate comma

vecLit :: [Doc] -> Doc
vecLit vs =
  case vs of
    []    -> "SMV.empty"
    [x]   -> "SMV.vec1" <+> x
    [x,y] -> "SMV.vec2" <+> x <+> y
    _     -> "SMV.VecMany" <+> listLit vs

