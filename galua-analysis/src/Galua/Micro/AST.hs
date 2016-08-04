-- | Low-level CFG representation of a Lua program.

{-# LANGUAGE TypeSynonymInstances, OverloadedStrings #-}
module Galua.Micro.AST
  ( module Galua.Micro.AST
  , ValueType(..)
  , Constant(..)
  ) where


import           Galua.Value(ValueType(..))
import qualified Language.Lua.Bytecode as OP
import           Language.Lua.Bytecode (Constant(..))
import           Language.Lua.Bytecode.Pretty(PP(..),blankPPInfo)
import           Text.PrettyPrint
import           Data.ByteString(ByteString)
import           Data.Foldable(toList)
import           Data.Vector(Vector)
import qualified Data.Vector as Vector
import           Data.Map (Map)
import qualified Data.Map as Map

data Function = Function
  { functionCode :: Map BlockName (Vector Stmt)
  } deriving Show

data Reg        = Reg !OP.Reg
                | TMP !Int !Int     -- ^ phase, temporary
                  deriving (Eq,Ord,Show)

data ListReg    = ArgReg | ListReg
                  deriving (Eq,Ord,Show)

data BlockName  = PCBlock !Int
                | NewBlock !Int !Int
                | EntryBlock
                  deriving (Eq,Ord,Show)

blockNamePC :: BlockName -> Int
blockNamePC pc =
  case pc of
    PCBlock n     -> n
    NewBlock n _  -> n
    EntryBlock    -> 0

type Literal    = Constant
type UpIx       = OP.UpIx


data Expr       = EReg Reg
                | ELit Literal
                | EUp  UpIx
                  deriving Show

data Stmt =

    -- Basic registers
    Assign !Reg !Expr

    -- Up values
  | SetUpVal !UpIx !Reg     -- Only before "reference" phase

    -- Tables
  | NewTable     !Reg
  | LookupTable  !Reg !Reg  !Expr
  | SetTable     !Reg !Expr !Expr
  | SetTableList !Reg !Int        -- ^SetTableList r i
                                  -- [ r[i+j] = list !! j | j <- list ]

    -- Meta tables
  | GetMeta !Reg !Expr

    -- Exceptions
  | Raise !Expr             -- End of block

    -- Control flow
  | Case !Expr [ (ValueType, BlockName) ] (Maybe BlockName) -- End of block
  | If   !Prop !BlockName !BlockName                   -- End of block
  | Goto !BlockName                                    -- End of block

    -- Functions
  | Call !Reg
  | TailCall !Reg      -- End of block
  | Return             -- End of block

  | CloseStack !Reg         -- Only before "reference" phase
  | NewClosure !Reg !Int ![Expr]


  -- Argument lists
  | Drop !ListReg !Int
  | Append !ListReg [Expr] -- ^ Prepend the given expressions to the list
  | SetList !ListReg [Expr]

  | IndexList !Reg !ListReg !Int

    -- Arith
  | Arith2   !Reg !Op2 !Expr !Expr
  | Arith1   !Reg !Op1 !Expr

    -- References
  | NewRef   !Reg  !Expr  -- Only after "reference" phase
  | ReadRef  !Reg  !Expr  -- Only after "reference" phase
  | WriteRef !Expr !Expr  -- Only after "reference" phase

  | Comment !String
    deriving Show



data Op1 = ToNumber       -- a -> Maybe Number
         | ToInt          -- a -> Maybe Int
         | ToString       -- a -> Maybe String
         | ToBoolean      -- a -> Bool
         | IntToDouble    -- Int -> Double    -- best effort, sometimes inexact

         | StringLen        -- String -> Int
         | TableLen         -- Table  -> Int
         | NumberUnaryMinus -- Number -> Number
         | Complement       -- Int    -> Int
         | BoolNot          -- Bool   -> Bool
            deriving Show

data Op2 = NumberAdd
         | NumberSub
         | NumberMul
         | NumberPow

         | IMod | FMod
         | IDiv | NumberDiv

         | And | Or | Xor | Shl | Shr

         | Concat
           deriving Show


--------------------------------------------------------------------------------
-- Properties

data Prop       = Prop !Pred ![Expr]
                  deriving Show

data Pred       = IsInteger | IsNaN | IsNone
                | Equals
                | NumberLT | NumberLEQ
                | StringLT | StringLEQ
                  deriving Show

class IsExpr t where
  toExpr :: t -> Expr

instance IsExpr Expr where
  toExpr = id

instance IsExpr Literal where
  toExpr = ELit

instance IsExpr Reg where
  toExpr = EReg

instance IsExpr UpIx where
  toExpr = EUp

instance IsExpr OP.Upvalue where
  toExpr x =
    case x of
      OP.UpReg r -> toExpr r
      OP.UpUp u  -> toExpr u


instance IsExpr Bool where
  toExpr = toExpr . KBool

instance IsExpr ByteString where
  toExpr = toExpr . KString

instance IsExpr Int where
  toExpr = toExpr . KInt

instance IsExpr OP.Reg where
  toExpr = toExpr . Reg


zero :: Expr
zero = toExpr (0 :: Int)

emptyString :: Expr
emptyString = toExpr ("" :: ByteString)


--------------------------------------------------------------------------------


pp' :: PP a => a -> Doc
pp' = pp blankPPInfo

instance PP Reg where
  pp _ reg =
    case reg of
      Reg (OP.Reg r) -> "R"   <> int r
      TMP p i        -> "T_" <> int p <> "_" <> int i

instance PP Expr where
  pp p expr =
    case expr of
      EReg e -> pp p e
      ELit l -> pp p l
      EUp u  -> pp p u

instance PP ListReg where
  pp _ ArgReg = "arg"
  pp _ ListReg = "list"

instance PP Stmt where
  pp _ stmt =
    case stmt of

      Assign r e    -> pp' r <+> "=" <+> pp' e
      SetUpVal u r  -> pp' u <+> "=" <+> pp' r

      NewTable r    -> pp' r <+> "=" <+> "newtable"
      LookupTable r1 r2 e ->
        pp' r1 <+> "=" <+> pp' r2 <> brackets (pp' e)

      SetTable r e1 e2 -> pp' r <> brackets (pp' e1) <+> "=" <+> pp' e2

      SetTableList r e1 ->
        pp' r <> brackets (int e1 <> "..") <+> "= list"


      GetMeta r1 r2 -> pp' r1 <+> "=" <+> pp' r2 <> ".meta"

      Raise e -> "raise" <+> pp' e

      Goto l   -> "goto" <+> pp' l
      Case e alts deflt -> "case" <+> pp' e <+> "of" $$ nest 2 (vcat as)
        where
        as          = map ppAlt alts ++ [ ppDef ]
        ppAlt (x,g) = pp' x <+> "->" <+> pp' g
        ppDef       = case deflt of
                        Nothing -> empty
                        Just b  -> "_" <+> "->" <+> pp' b

      If p t f -> "if" <+> pp' p <+> "then" <+> pp' t <+> "else" <+> pp' f

      Arith1 r op x   -> pp' r <+> "=" <+> pp' op <+> pp' x
      Arith2 r op x y -> pp' r <+> "=" <+> pp' x <+> pp' op <+> pp' y

      Call f        -> "list =" <+> pp' f <> "(list)"
      TailCall f    -> "return" <+> pp' f <> "(list)"
      Return        -> "return"


      IndexList r list ix ->
        pp' r <+> "=" <+> pp' list <+> "!!" <+> int ix

      Drop list n ->
        pp' list <+> "=" <+> "drop" <+> int n <+> pp' list

      Append list xs ->
        pp' list <+> "=" <+> brackets (hsep (punctuate comma (map pp' xs)))
                 <+> "++" <+> pp' list

      SetList list xs ->
        pp' list <+> "=" <+> brackets (hsep (punctuate comma (map pp' xs)))

      NewRef r e    -> pp' r  <+> "=" <+> "newRef" <+> pp' e
      ReadRef r1 r2 -> pp' r1 <+> "=" <+> "readRef" <+> pp' r2
      WriteRef r1 e -> "writeRef" <+> pp' r1 <+> pp' e

      CloseStack r    -> "closeStack" <+> pp' r
      NewClosure r i es ->
        pp' r <+> "=" <+> "newClosure" <> brackets (int i) <+> hsep (map pp' es)

      Comment x -> "--" <+> text x



instance PP Op1 where
  pp _ op =
    case op of
      ToNumber    -> "toNumber"
      ToInt       -> "toInt"
      ToString    -> "toString"
      ToBoolean   -> "toBoolean"
      IntToDouble -> "intToDouble"
      NumberUnaryMinus -> "-"
      Complement  -> "~"
      StringLen   -> "stringLen"
      TableLen    -> "tableLen"
      BoolNot     -> "not"


instance PP Op2 where
  pp _ op =
    case op of
      NumberAdd -> "+"
      NumberSub -> "-"
      NumberMul -> "*"

      NumberPow -> "^"

      FMod      -> "%"
      IMod      -> "%%"
      NumberDiv -> "/"
      IDiv      -> "//"

      And -> "&"
      Or  -> "|"
      Xor -> "~"
      Shl -> "<<"
      Shr -> ">>"

      Concat -> ".."


instance PP Prop where
  pp _ (Prop p args) =
    case args of
      [l,r] -> pp' l <+> pp' p <+> pp' r
      _     -> pp' p <+> hsep (map pp' args)

instance PP Pred where
  pp _ pre =
    case pre of
      IsInteger -> "isInteger"
      IsNaN     -> "isNan"
      IsNone    -> "isNone"
      Equals    -> "="
      NumberLT  -> "<"
      NumberLEQ -> "<="
      StringLT  -> "<S"
      StringLEQ -> "<=S"

instance PP BlockName where
  pp _ bn =
    case bn of
      EntryBlock   -> "ENTRY"
      PCBlock n    -> "PC" <> int n
      NewBlock p n -> "PC" <> int p <> "_" <> int n

ppBlocks :: Map BlockName (Vector Stmt) -> Doc
ppBlocks = vcat . map ppBlock . Map.toList

ppBlock :: (BlockName, Vector Stmt) -> Doc
ppBlock (nm,xs) = (pp' nm <> colon) $$ nest 2 (ppStmts xs)

ppStmts :: Vector Stmt -> Doc
ppStmts xs = vcat $ map pp' $ Vector.toList xs

ppDot :: Map BlockName (Vector Stmt) -> Doc
ppDot m = vcat $ [ "digraph G {"
                 , "size=\"6,4\";"
                 , "ratio=\"fill\";"
                 ]
              ++ map node els
              ++ map edges els
              ++ [ "}" ]
  where
  els = Map.toList m
  node (nm,stms) =
    pp' nm <>
      brackets ("label=" <>
                  text (left $ show $ show (ppBlock (nm,stms)))) <> semi

  left ('\\' : 'n' : cs) = '\\' : 'l' : left cs
  left ['"']             = "\\l\""
  left (x : xs)          = x : left xs
  left []                = []

  edges (nm,stmts) = vcat [ edge nm x | xs <- map followers (toList stmts)
                                      , x <- xs ]
  edge a (mbLab,b) =
    pp' a <+> "->" <+> pp' b <+> maybe empty ppLab mbLab <> semi
    where ppLab l = brackets ("label=" <> text (show l))


  followers s =
    case s of
      Goto x      -> [ (Nothing,x) ]
      Case _ as b -> [ (Just (show (pp' t)),a) | (t,a) <- as ] ++
                     maybe [] (\x -> [(Just "otherwise",x)]) b
      If _ t e    -> [ (Just "true", t), (Just "false", e) ]
      _           -> []



