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

import Prelude hiding ((.))


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
  , "{-# Language MagicHash, UnboxedTuples #-}"
  , "module" <+> text modName <+> "(main) where"
  , "import Data.Maybe"
  , "import Data.IORef"
  , "import qualified Data.Map as Map"
  , "import           Data.Vector.Mutable(IOVector)"
  , "import qualified Data.Vector.Mutable as IOVector"
  , "import qualified Data.Vector as Vector"
  , "import qualified Data.ByteString as BS"
  , "import GHC.Prim"
  , "import GHC.Types(IO(..))"
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
  , "main cloRef vm reg_args ="
  , nest 2 $ doBlock $
      [ "let MkClosure { cloFun = LuaFunction fid fun"
      , "              , cloUpvalues = upvalues }  = referenceVal cloRef"

      , "errRef <- newIORef (error \"[bug]: used `errRef`\")"
      ] ++
     [ "IO $ \\hS ->" <+> enterBlock' EntryBlock initState ]
  ] ++
  [ blockDecl info k v $$ " " | (k,v) <- Map.toList (functionCode mf) ]
  where
  (declare,initState) = stateDecl info
  mf = translate func

  info = Info { tmpNum = functionRegsTMP mf, regNum = funcMaxStackSize func }


{-

The state:

@
type LuaState = (# {- reg_args  -} SmallVec Value
                ,  {- reg_list  -} SmallVec Value
                ,  {- reg_0     -} Value
                ,  ...
                ,  {- ref_0     -} IORef Value   -- XXX: Unpack?
                ,  ...
                ,  {- tmp_0     -} Value
                ,  ...
                #)
@

-}

data Info = Info { tmpNum, regNum :: !Int }

rLsts :: [Name]
rLsts = [ regListName r | r <- [ ArgReg, ListReg ] ]

rVals :: Info -> [Name]
rVals x = [ regValName (Reg r)    | r <- regRange (Code.Reg 0) (regNum x) ]

rRefs :: Info -> [Name]
rRefs x = [ regRefName (Reg r)    | r <- regRange (Code.Reg 0) (regNum x) ]

rTMPs :: Info -> [Name]
rTMPs x = [ regValName (TMP 3 i)  | i <- take (tmpNum x) [ 0 .. ] ]



stateDecl :: Info -> (HsDecl, HsDecl)
stateDecl info =
  ( "type LuaState =" <+> bigtup fieldTys
  , bigtup fieldVals
  )
  where
  fieldTys  = [ cmt r <+> "SmallVec Value" | r <- rLsts ] ++
              [ cmt r <+> "Value"          | r <- rVals info ] ++
              [ cmt r <+> "IORef Value"    | r <- rRefs info ] ++
              [ cmt r <+> "Value"          | r <- rTMPs info ]

  fieldVals = [ cmt r <+> (if show r == "reg_args"
                              then "reg_args" else"SMV.empty") -- hack
                     | r <- rLsts ] ++
              [ cmt r <+> "Nil"       | r <- rVals info ] ++
              [ cmt r <+> "errRef"    | r <- rRefs info ] ++
              [ cmt r <+> "Nil"       | r <- rTMPs info ]

packState :: Info -> HsExpr
packState i = bigtup (rLsts ++ rVals i ++ rRefs i ++ rTMPs i)


bigtup :: [Doc] -> Doc
bigtup xs = utupv (map utup (chunk 50 xs))

chunk :: Int -> [a] -> [[a]]
chunk _ []  = []
chunk n xs = case splitAt n xs of
               (as,bs) -> as : chunk n bs

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
    TMP {}            -> error "regRefName: TMP"

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
blockName :: LuaState -> State# RealWorld -> (# State# RealWorld, NextStep #)
blockName (# arg, list, reg_0, .., ref_0, .., tmp_0, .. #) hS =

innerStmt :: args, ..., hS :: State# RealWorld |- State# RealWorld
endStmt   :: args, ..., hS :: State# RealWorld |- (# State# RealWorld, Next #)
@


Setting temporary register tmp_N to E, followed by S:
@
  case E of tmp_N ->
  S
@

Setting value register reg_N to E, followed by S:
@
  case (# E, errRef #) of (# reg_N, ref_N #) ->
  S
@

Setting reference register ref_N to E, followed by S:
@
  case (# Nil, E #) of (# reg_N, ref_N #) ->
  S
@


Entering block B:
@
  B (# reg_arg, reg_list, reg_0, .., ref_0, .. tmp_0 #) hS
@

There is a special case for the handling of function calls:
when a function is called, we want to stop and go back to the environment,
so that we give it an option to execute the function in some other way
(e.g., slowly, step-by-step).   To do this we return a `FunCall` next step,
and its continuation we instert the remaining statements of the block,
so that whenever the caller returns back to us, the rest of the block
will be executed.

So a functional call followed by S, looks like this:

@
  FunCall ...  $ \reg_list -> IO $ \hS ->
    S
@


Performing IO:
@
  case IO of IO x -> x
@
-}

blockDecl :: Info -> BlockName -> Block -> HsDecl
blockDecl info bn stmts = vcat
  [ name <+> "::" <+> vcat [ "VM ->"
                           , "IORef Value ->"
                           , "IOVector (IORef Value) ->"
                           , "FunId ->"
                           , "Function ->"
                           , "LuaState ->"
                           , "State# RealWorld ->"
                           , " (# State# RealWorld, NextStep #)"
                           ]
  , name <+> "vm errRef upvalues fid fun" <+> packState info <+> "hS ="
  , nest 2 (goBlocks stmtList)
  ]
  where
  name     = blockNameName bn
  stmtList = map stmtCode (Vector.toList (blockBody stmts))

  goBlocks [] = stmtEndStmt info (stmtCode (blockEnd stmts))
  goBlocks (s : ss) =
    case s of
      Call f -> performCall f (goBlocks ss)
      _      -> stmtStmt s (goBlocks ss)


-- | Compilation of an end statement.
stmtEndStmt :: Info -> EndStmt -> HsExpr
stmtEndStmt i stmt =
  case stmt of
    -- Local control flow
    Case e as d             -> performCase i e as d
    If p tr fa              -> performIf i p tr fa
    Goto b                  -> performGoto i b

    TailCall clo            -> performTailCall clo
    Return                  -> performReturn
    Raise e                 -> performRaise e




-- | Compilation of a statement.
stmtStmt :: Stmt -> HsExpr -> HsExpr
stmtStmt stmt =
  case stmt of
    Comment s               -> ("{-" <+> text s <+> "-}" $$)
                                -- XXX: escape `s`?
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
getReg r = regValName r

-- | Get the reference stored in a register.
-- Type: 'IORef Value'
getRegRef :: Reg -> HsExpr
getRegRef r = regRefName r

-- | Get the value from one of the list register.
--   Type: 'SmallVec Value'
getRegList :: ListReg -> HsExpr
getRegList r = regListName r

-- | Get the value from the result register.
--   Type: 'SmallVec Value'
getListReg :: HsExpr
getListReg = getRegList ListReg


-- | Set a register to a given value.
setReg :: Reg -> HsExpr -> HsExpr -> HsExpr
setReg r e =
  case r of
    TMP {} -> regValName r <~ e
    Reg {} -> utup [regValName r, regRefName r] <~ utup [e, "errRef"]

-- | Set a registesr to a given reference.
setRegRef :: Reg -> HsExpr -> HsExpr -> HsExpr
setRegRef r e = utup [regValName r, regRefName r] <~ utup ["Nil",e]

-- | Set the value of a list register.
setRegList :: ListReg -> HsExpr -> HsExpr -> HsExpr
setRegList x e = regListName x <~ e

--------------------------------------------------------------------------------


-- | Get the value of an expression.
-- Type: (hS :: State# RealWorld) |- (# State# RealWorld, Value #)
-- The IO is used so that we can allocate string litterals.
getExpr :: Expr -> HsExpr
getExpr expr =
  case expr of
    EReg r -> done (getReg r)
    EUp {} -> error "getExpr: upvalues are references"
    ELit l ->
      case l of
        LNil    -> done "Nil"
        LBool b -> done (val "Bool" b)
        LNum d  -> done (papp "Number" (val "Double" d))
        LInt n  -> done (papp "Number" (val "Int"    n))
        LStr bs -> parens ("s" <-- val "fromByteString" bs
                            $ done "String s")
          -- XXX: In some cases the string is only used internally, in
          -- which case we may be able to avoid allocating it.
  where
  val c x = c <+> text (show x)


-- | Get the value of a reference expression.
-- Type: (# State# RealWorld, IORef Value #)
-- XXX: Can we just work with the MutVars directly?
getRefExpr :: Expr -> HsExpr
getRefExpr expr =
  case expr of
    EReg r -> done (getRegRef r)
        -- papp "IORef" $ papp "STRef" $ getRegRef r ]
    EUp (Code.UpIx x)  ->
      "v" <-- ("IOVector.unsafeRead upvalues" <+> int x) $
      done "v"

    ELit {} -> error "Bug: there should be not reference literals."



-- | Emit code to start executing the given block.
-- Type: (# State# RealWorld, NextStep #)
enterBlock :: Info -> BlockName -> HsExpr
enterBlock i b = enterBlock' b (packState i)

-- Type: (# State# RealWorld, NextStep #)
enterBlock' :: BlockName -> HsExpr -> HsExpr
enterBlock' b s =
  blockNameName b <+> "vm errRef upvalues fid fun" $$ nest 2 (s <+> "hS")






-- | Compile a decision.
-- Type: (# State# RealWorld, Bool #)
getProp :: Prop -> HsExpr
getProp (Prop pre ~(v1 : ~(v2 : _))) =
  parens $
  vcat $
  case pre of
    IsNone ->
      [ "case" <+> getExpr v1 <+> "of"
      , "  (# hS, Bool b #) -> (# hS, b     #)"
      , "  (# hS, Nil    #) -> (# hS, False #)"
      , "  (# hS, _      #) -> (# hS, True  #)"
      ]

    IsInteger ->
      [ "case" <+> getExpr v1 <+> "of"
      , "  (# hS, Number (Int {}) #) -> (# hS, True #)"
      , "  (# hS, _               #) -> (# hS, False #)"
      ]

    IsNaN ->
      [ "case" <+> getExpr v1 <+> "of"
      , "  (# hS, Number (Double d) #) -> (# hS, isNan d #)"
      , "  (# hS, _                 #) -> (# hS, False   #)"
      ]

    Equals ->
      [ "case" <+> getExpr v1 <+> "of { (# hS, v1 #) ->"
      , "case" <+> getExpr v2 <+> "of { (# hS, v2 #) -> (# hS, v1 == v2 #) }}"
      ]

    NumberLT ->
      [ "case" <+> getExpr v1 <+> "of { (# hS, Number n1 #) ->"
      , "case" <+> getExpr v2 <+>
                             "of { (# hS, Number n2 #) -> (# hS, n1 < n2 #) }}"
      ]

    NumberLEQ ->
      [ "case" <+> getExpr v1 <+> "of { (# hS, Number n1 #) ->"
      , "case" <+> getExpr v2 <+>
                          "of { (# hS, Number n2 #) -> (# hS, n1 <= n2 #) }}"
      ]

    StringLT -> -- XXX: avoid allocating string literals
      [ "case" <+> getExpr v1 <+> "of { (# hS, String s1 #) ->"
      , "case" <+> getExpr v2 <+>
                            "of { (# hS, String s2 #) -> (# hS, s1 < s2 #) }}"

      ]

    StringLEQ ->  -- XXX: avoid allocating string literals
      [ "case" <+> getExpr v1 <+> "of { (# hS, String s1 #) ->"
      , "case" <+> getExpr v2 <+>
                            "of { (# hS, String s2 #) -> (# hS, s1 <= s2 #) }}"

      ]


performAssign :: Reg -> Expr -> HsExpr -> HsExpr
performAssign r e =
  "v" <~~ getExpr e .
  setReg r "v"

performNewTable :: Reg -> HsExpr -> HsExpr
performNewTable r =
  "v" <-- "machNewTable vm 0 0" .
  setReg r "Table v"

-- | Make a pure label to be used for indexing.
-- Type: Value
strLab :: ByteString -> HsExpr
strLab x = "String" <+> parens ("unsafeFromByteString" <+> text (show x))

performLookupTable :: Reg -> Reg -> Expr -> HsExpr -> HsExpr
performLookupTable res tab (ELit (LStr x)) =
  "Table t" <~  getReg tab .
  "v"       <-- papp "getTableRaw t" (strLab x) .
  setReg res "v"

performLookupTable res tab expr =
  "Table t" <~ getReg tab .
  "s"      <~~ getExpr expr .
  "v"      <-- "getTableRaw t s" .
  setReg res "v"


performSetTable :: Reg -> Expr -> Expr -> HsExpr -> HsExpr
performSetTable tab (ELit (LStr x)) val =
  "Table t" <~ getReg tab .
  "val"     <~~ getExpr val .
  "_"       <-- (papp "setTableRaw t" (strLab x) <+> "val")

performSetTable tab ix val =
  "Table t" <~ getReg tab .
  "ix"     <~~ getExpr ix .
  "val"    <~~ getExpr val .
  "_"      <-- "setTableRaw t ix val"


performSetTableList :: Reg -> Int -> HsExpr -> HsExpr
performSetTableList tab ix =
  "Table t" <~ getReg tab .
  "_" <-- ("iForM_" <+> getListReg <+> "$ \\i a ->" <+>
                    "setTableRaw t (Number (Int (i" <+> int ix <+>"))) a")


performGetMeta :: Reg -> Expr -> HsExpr -> HsExpr
performGetMeta r e =
  "v" <~~ getExpr e .
  doLet [ "tmp = case v of"
        , "        Table tr    -> getTableMeta tr"
        , "        UserData ur -> readIORef (userDataMeta (referenceVal ur))"
        , "        _ -> let mt = machMetatablesRef (vmMachineEnv vm)"
        , "                 ty = valueType v"
        , "             in Map.lookup ty <$> readIORef mt"
        ] .
  "mb" <-- "tmp" .
  setReg r "maybe Nil Table mb"


performRaise :: Expr -> HsExpr
performRaise e =
  "v" <~~ getExpr e $
  done "ThrowError v"

performCase :: Info ->
               Expr -> [(ValueType,BlockName)] -> Maybe BlockName -> HsExpr
performCase i e as d =
  "v" <~~ getExpr e $
  "case valueType v of" $$ nest 2 (vcat branches)
  where
  pats      = [ tyPat t <+> "->" <+> enterBlock i b | (t,b) <- as ]
  branches  = case d of
                Nothing -> pats
                Just b  -> pats ++ ["_ ->" <+> enterBlock i b]

  tyPat t   = text (show t)


performIf :: Info -> Prop -> BlockName -> BlockName -> HsExpr
performIf i p tr fa =
  "b" <~~ getProp p $
  "if b then" <+> enterBlock i tr <+> "else" <+> enterBlock i fa

performGoto :: Info -> BlockName -> HsExpr
performGoto i b = enterBlock i b


performCall :: Reg -> HsExpr -> HsExpr
performCall clo after = vcat
    [ "let Closure newFun =" <+> getReg clo
    , "    args        =" <+> getListReg
    , "in"
    , done ("FunCall newFun args Nothing $ \\" <+>
                    regListName ListReg <+> "->" $$ "IO $ \\hS ->" $$ after)
    ]


performTailCall :: Reg -> HsExpr
performTailCall clo = vcat
    [ "let Closure newFun =" <+> getReg clo
    , "    vs =" <+> getListReg
    , "in"
    , done "FunTailcall newFun vs"
    ]


performReturn :: HsExpr
performReturn = done ("FunReturn" <+> getListReg)


performNewClosure :: Reg -> Int -> Code.Function -> HsExpr -> HsExpr
performNewClosure res ix fun = exprs $
  [ u <~~ getRefExpr ue | (ue,u) <- zip upexprs unames ] ++
  [ "ups" <-- papp "Vector.thaw"
                ("Vector.fromListN" <+> int upNum <+> listLit unames)
  , \k -> vcat
      [ "let fu = luaFunction" <+> parens ("Code.subFun fid" <+> int ix)
                <+> parens ("Vector.unsafeIndex (funcNested fun)" <+> int ix)
      , "in"
      , "c" <-- "machNewClosure vm fu ups" $ k
      ]
  , setReg res "Closure c"
  ]
  where
  upexprs = funcUpvalExprs fun
  upNum   = length upexprs
  unames  = [ "u" <> int i | i <- take upNum [ 0 .. ] ]



performDrop :: ListReg -> Int -> HsExpr -> HsExpr
performDrop res n = setRegList res ("SMV.drop" <+> int n <+> getRegList res)


performSetList :: ListReg -> [Expr] -> HsExpr -> HsExpr
performSetList res es = exprs $
  [ v <~~ getExpr e | (v,e) <- zip vs es ] ++
  [ setRegList res (vecLit vs) ]
  where
  vs  = [ "v" <> int i | i <- take (length es) [ 0 .. ] ]


performAppend :: ListReg -> [Expr] -> HsExpr -> HsExpr
performAppend res es = exprs $
  [ v <~~ getExpr e | (v,e) <- zip vs es ] ++
  [ setRegList res (vecLit vs <+> "SMV.++" <+> getRegList res) ]
  where
  vs  = [ "v" <> int i | i <- take (length es) [ 0 .. ] ]


performIndexList :: Reg -> ListReg -> Int -> HsExpr -> HsExpr
performIndexList res lst ix =
  setReg res ("SMV.indexWithDefault" <+> getRegList lst <+> "Nil" <+> int ix)

performNewRef :: Reg -> Expr -> HsExpr -> HsExpr
performNewRef res val =
  "v"   <~~ getExpr val .
  "ref" <-- "newIORef v" .
  setRegRef res "ref"

performReadRef :: Reg -> Expr -> HsExpr -> HsExpr
performReadRef res ref =
  "ref" <~~ getRefExpr ref .
  "v"   <-- "readIORef ref" .
  setReg res "v"


performWriteRef :: Expr -> Expr -> HsExpr -> HsExpr
performWriteRef ref val =
  "ref" <~~ getRefExpr ref .
  "v"   <~~ getExpr val .
  "_"   <-- "writeIORef ref v"


performArith1 :: Reg -> Op1 -> Expr -> HsExpr -> HsExpr
performArith1 res op e =
  "v" <~~ getExpr e . doOp
  where
  doOp =
    case op of
      ToNumber ->
        doLet ["mb = valueNumber v"] .
        setReg res "maybe Nil Number mb"

      ToInt ->
        doLet ["mb = valueInt v"] .
        setReg res "fromMaybe Nil (Number . Int) mb"

      IntToDouble ->
        "Number (Int n)" <~ "v" .
        setReg res "Number (Double (fromIntegral n))"

      ToString ->
        doLet [ "tmp = case valueString v of" 
              , "        Just s ->"
              , "          case fromByteString s of { IO go ->"
              , "          case go hS of (# hS, v1 #) -> (# hS, String v1 #) }"
              , "        Nothing -> (# hS, Nil #)"
              ] .
        "v" <~~ "tmp" .
        setReg res "v"

      ToBoolean ->
        setReg res "Bool (valueBool v)"

      StringLen ->
        "String s" <~ "v" .
        setReg res "Number (Int (luaStringLen s))"

      TableLen ->
        "Table t" <~ "v" .
         "n" <-- "tableLen t" .
         setReg res "Number (Int n)"

      NumberUnaryMinus ->
        "Number n" <~ "v" .
        setReg res "Number (neagate n)"

      Complement  ->
        "Number (Int n)" <~ "v" .
        setReg res "Number (Int (complement n))"

      BoolNot ->
        "Bool b" <~ "v" .
        setReg res "Bool (not b)"


performArith2 :: Reg -> Op2 -> Expr -> Expr -> HsExpr -> HsExpr
performArith2 res op e1 e2 =
  "v1" <~~ getExpr e1 .
  "v2" <~~ getExpr e2 .
  doOp
  where
  doOp =
    case op of

      NumberAdd ->
        "(# Number n1, Number n2 #)" <~ "(# v1, v2 #)" .
        setReg res "Number (n1 + n2)"

      NumberSub ->
        "(# Number n1, Number n2 #)" <~ "(# v1, v2 #)" .
        setReg res "Number (n1 - n2)"

      NumberMul ->
        "(# Number n1, Number n2 #)" <~ "(# v1, v2 #)" .
        setReg res "Number (n1 * n2)"

      NumberPow ->
        "(# Number n1, Number n2 #)" <~ "(# v1, v2 #)" .
        setReg res "Number (numberPow n1 n2)"

      IMod ->
        "(# Number (Int n1), Number (Int n2) #)" <~ "(# v1, v2 #)" .
        setReg res "Number (Int (mod n1 n2))"

      FMod ->
        "(# Number (Double n1), Number (Double n2) #)" <~ "(# v1, v2 #)" .
        setReg res "Number (Double (nummod n1 n2))"

      IDiv ->
        "(# Number (Int n1), Number (Int n2) #)" <~ "(# v1, v2 #)" .
        setReg res "Number (Int (div n1 n2))"

      NumberDiv ->
        "(# Number n1, Number n2 #)" <~ "(# v1, v2 #)" .
        setReg res "Number (numberDiv n1 n2)"

      And ->
        "(# Number (Int n1), Number (Int n2) #)" <~ "(# v1, v2 #)" .
        setReg res "Number (Int (n1 .&. n2))"

      Or ->
        "(# Number (Int n1), Number (Int n2) #)" <~ "(# v1, v2 #)" .
        setReg res "Number (Int (n1 .|. n2))"

      Xor ->
        "(# Number (Int n1), Number (Int n2) #)" <~ "(# v1, v2 #)" .
        setReg res "Number (Int (xor n1 n2))"

      Shl ->
        "(# Number (Int n1), Number (Int n2) #)" <~ "(# v1, v2 #)" .
        setReg res "Number (Int (wordshiftL n1 n2))"

      Shr ->
        "(# Number (Int n1), Number (Int n2) #)" <~ "(# v1, v2 #)" .
        setReg res "Number (Int (wordshiftR n1 n2))"

      Concat ->
        "(# String s1, String s2 #)" <~ "(# v1, v2 #)" .
        doLet [ "x = toByteString s1"
              , "y = toByteString s2"
              , "tmp = if BS.null x then (# hS, s1 #) else"
              , "      if BS.null y then (# hS, s2 #) else"
              , "      case fromByteString (BS.append x y) of IO go -> go hS"
              ] .
        "z" <~~ "tmp" .
        setReg res "String z"




--------------------------------------------------------------------------------
-- A primitive way of emitting Haskell code.

type HsModule = Doc
type HsDecl   = Doc
type HsExpr   = Doc
type HsPat    = Doc
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
    _     -> "SMV.fromList" <+> listLit vs

cmt :: Doc -> Doc
cmt x = "{-" <+> x <+> "-}"

utup :: [Doc] -> Doc
utup xs = "(#" <+> hsep (punctuate comma xs) <+> "#)"

utupv :: [Doc] -> Doc
utupv fs = case fs of
             [] -> "(# #)"
             x : xs -> ("(#" <+> x)
                    $$ vcat (map ("," <+>) xs) <+> "#)"



papp :: Doc -> Doc -> Doc
papp x y = x <+> parens y

infix 9 <~, <~~, <--

(<~) :: HsPat -> HsExpr -> HsExpr -> HsExpr
(p <~ e) k = "case" <+> e <+> "of {" <+> p <+> "->" $$ k <+> "}"

(<~~) :: HsPat -> HsExpr -> HsExpr -> Doc
v <~~ e = utup ["hS",v] <~ e

(<--) :: HsPat -> HsExpr -> HsExpr -> Doc
v <-- e = "IO go" <~ e .
          v <~~ "go hS"

-- Decrease the precedence of (.) so that the arrows work correctly
(.) :: (b -> c) -> (a -> b) -> (a -> c)
(g . f) x = g (f x)

infixl 8 .



done :: HsExpr -> HsExpr
done x = utup [ "hS", x ]

exprs :: [HsExpr -> HsExpr] -> HsExpr -> HsExpr
exprs = foldr (.) id

doLet :: [HsDecl] -> HsExpr -> HsExpr
doLet d k = "let" <+> vcat d $$ "in" $$ k


