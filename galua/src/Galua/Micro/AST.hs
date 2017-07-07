-- | Low-level CFG representation of a Lua program.

{-# LANGUAGE TypeSynonymInstances, OverloadedStrings #-}
module Galua.Micro.AST
  ( module Galua.Micro.AST
  , ValueType(..)
  , Literal(..)
  ) where


import           Galua.ValueType(ValueType(..))
import           Data.ByteString(ByteString)
import           Data.Vector(Vector)
import qualified Data.Vector as Vector
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Foldable(toList)
import           Data.List(nub)

import qualified Galua.Code as Code
import           Galua.Code (Literal(..))
import           Galua.Pretty

{- | The intermediate form of a function, used for compilation.
The function is split into basic blocks, making all control
flow explicit. -}
data MicroFunction = MicroFunction
  { functionCode    :: Map BlockName Block
  , functionRegsTMP :: !Int
  } deriving Show

-- | A basic block.
data Block = Block
  { blockBody   :: Vector (BlockStmt Stmt)
  , blockEnd    :: BlockStmt EndStmt
  , blockInputs :: Set Input
  } deriving Show

-- | Annotate each stmt in the program with the PC of the origianl program,
-- in case we want to map back for debugging purposes.
data BlockStmt a = BlockStmt
  { stmtPC   :: !Int   -- ^ PC in original program
  , stmtCode :: a
  } deriving Show


-- | Returns the possible followers of a basic block.
blockNext :: Block -> [BlockName]
blockNext b =
  case stmtCode (blockEnd b) of
    Goto x      -> [x]
    If _ t e    -> if t == e then [t] else [t,e]
    Case _ as d -> nub (toList d ++ map snd as)
    _           -> []



--------------------------------------------------------------------------------
-- Names
--------------------------------------------------------------------------------

data BlockName  = EntryBlock
                | PCBlock !Int
                | NewBlock !Int !Int
                  deriving (Eq,Show)

data Reg        = Reg !Code.Reg     -- ^ stores a value, cannot be captured
                | TMP !Int !Int     -- ^ phase, temporary;  store a value
                | Ref !Code.Reg     -- ^ stores a reference can be captured
                  deriving (Eq,Ord,Show)

data ListReg    = ArgReg  -- ^ Function arguments, var-args in particualr
                | ListReg -- ^ Function results, mostly(?)
                  deriving (Eq,Ord,Show)

-- | A parameter to a basic block
data Input      = LReg !ListReg   -- ^ List register
                | IReg !Reg       -- ^ Value register
                  deriving (Eq,Ord,Show)

instance Ord BlockName where
  compare EntryBlock EntryBlock = EQ
  compare EntryBlock _          = LT
  compare _ EntryBlock          = GT

  compare (PCBlock x) (PCBlock y) = compare x y
  compare (PCBlock x) (NewBlock y _) =
    case compare x y of
      LT -> LT
      GT -> GT
      EQ -> LT
  compare (NewBlock x _) (PCBlock y) =
    case compare x y of
      LT -> LT
      GT -> GT
      EQ -> GT

  compare (NewBlock x y) (NewBlock a b) = compare (x,y) (a,b)


blockNamePC :: BlockName -> Int
blockNamePC pc =
  case pc of
    PCBlock n     -> n
    NewBlock n _  -> n
    EntryBlock    -> 0

type UpIx       = Code.UpIx


--------------------------------------------------------------------------------


data EndStmt =

  -- Local control flow
    Case !Expr [ (ValueType, BlockName) ] (Maybe BlockName)
  | If   !Prop !BlockName !BlockName
  | Goto !BlockName

  -- End function
  | TailCall !Reg
  | Return
  | Raise !Expr
    deriving Show


data Stmt =

    -- Basic registers
    Assign !Reg !Expr

    -- Tables
  | NewTable     !Reg
  | LookupTable  !Reg !Reg  !Expr
  | SetTable     !Reg !Expr !Expr
  | SetTableList !Reg !Int        -- ^SetTableList r i
                                  -- [ r[i+j] = list !! j | j <- list ]

    -- Meta tables
  | GetMeta !Reg !Expr

    -- Functions
  | NewClosure !Reg !Int !Code.Function
  | Call !Reg

  -- "List" registers: these are
  | Drop !ListReg !Int            -- ^ Drop the given number (POP)
  | Append !ListReg [Expr]        -- ^ Prepend to the list (PUSH).
  | SetList !ListReg [Expr]       -- ^ Set the value of a list register.
  | IndexList !Reg !ListReg !Int  -- ^ Access an element in the list.

    -- Arithmetic
  | Arith2   !Reg !Op2 !Expr !Expr
  | Arith1   !Reg !Op1 !Expr

    -- Up values
  | SetUpVal !UpIx !Reg           -- Only before "reference" phase
  | CloseStack !Reg               -- Only before "reference" phase

    -- References
  | NewRef   !Reg  !Expr          -- Only after "reference" phase
  | ReadRef  !Reg  !Expr          -- Only after "reference" phase
  | WriteRef !Expr !Expr          -- Only after "reference" phase

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
-- Expressions and Properties



-- | A simple expression.
data Expr       = EReg Reg
                | ELit Literal
                | EUp  UpIx
                  deriving Show


-- | A boolean expression used in if-then-else
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

instance IsExpr Code.Upvalue where
  toExpr x =
    case x of
      Code.UpReg r -> toExpr r
      Code.UpUp u  -> toExpr u

instance IsExpr Bool where
  toExpr = toExpr . LBool

instance IsExpr ByteString where
  toExpr = toExpr . LStr

instance IsExpr Int where
  toExpr = toExpr . LInt

instance IsExpr Code.Reg where
  toExpr = toExpr . Reg


zero :: Expr
zero = toExpr (0 :: Int)

emptyString :: Expr
emptyString = toExpr ("" :: ByteString)

-- | To be used before expilcit references
funcUpvalExprs :: Code.Function -> [Expr]
funcUpvalExprs = map toExpr . Vector.toList . Code.funcUpvalues

-- | To be used after explicit references
funcUpvalRefExprs :: Code.Function -> [Expr]
funcUpvalRefExprs = map toRefExpr . Vector.toList . Code.funcUpvalues
  where
  toRefExpr x = case x of
                  Code.UpReg r -> EReg (Ref r)
                  Code.UpUp u  -> toExpr u






--------------------------------------------------------------------------------

instance Pretty MicroFunction where
  pp = vcat . map sh . Map.toList . functionCode
    where
    sh (x,vs) = pp x <> colon $$ nest 2 (pp vs)
                              $$ text " "

instance Pretty Block where
  pp b = parens (hsep (punctuate comma (map pp inpList))) $$
         vcat (map pp (Vector.toList (blockBody b))) $$
         pp (blockEnd b)
    where inpList = Set.toList (blockInputs b)

instance Pretty Input where
  pp inp =
    case inp of
      LReg r -> pp r
      IReg r -> pp r

instance Pretty Reg where
  pp reg =
    case reg of
      Reg (Code.Reg r) -> "R"    <> int r
      TMP p i          -> "T_"   <> int p <> "_" <> int i
      Ref (Code.Reg r) -> "Ref_" <> int r

instance Pretty Expr where
  pp expr =
    case expr of
      EReg e -> pp e
      ELit l -> pp l
      EUp u  -> pp u

instance Pretty ListReg where
  pp ArgReg = "arg"
  pp ListReg = "list"

instance Pretty a => Pretty (BlockStmt a) where
  pp stmt = int (stmtPC stmt) <> colon <+> pp (stmtCode stmt)

instance Pretty EndStmt where
  pp stmt =
    case stmt of
      Goto l    -> "goto" <+> pp l
      If p t f  -> "if" <+> pp p <+> "then" <+> pp t <+> "else" <+> pp f
      Case e alts deflt -> "case" <+> pp e <+> "of" $$ nest 2 (vcat as)
        where
        as          = map ppAlt alts ++ [ ppDef ]
        ppAlt (x,g) = pp x <+> "->" <+> pp g
        ppDef       = case deflt of
                        Nothing -> empty
                        Just b  -> "_" <+> "->" <+> pp b

      Raise e     -> "raise"  <+> pp e
      TailCall f  -> "return" <+> pp f <> "(list)"
      Return      -> "return"



instance Pretty Stmt where
  pp stmt =
    case stmt of

      Assign r e    -> pp r <+> "=" <+> pp e
      SetUpVal u r  -> pp u <+> "=" <+> pp r

      NewTable r    -> pp r <+> "=" <+> "newtable"
      LookupTable r1 r2 e ->
        pp r1 <+> "=" <+> pp r2 <> brackets (pp e)

      SetTable r e1 e2 -> pp r <> brackets (pp e1) <+> "=" <+> pp e2

      SetTableList r e1 ->
        pp r <> brackets (int e1 <> "..") <+> "= list"


      GetMeta r1 r2 -> pp r1 <+> "=" <+> pp r2 <> ".meta"


      Arith1 r op x   -> pp r <+> "=" <+> pp op <+> pp x
      Arith2 r op x y -> pp r <+> "=" <+> pp x <+> pp op <+> pp y

      Call f        -> "list =" <+> pp f <> "(list)"

      IndexList r list ix ->
        pp r <+> "=" <+> pp list <+> "!!" <+> int ix

      Drop list n ->
        pp list <+> "=" <+> "drop" <+> int n <+> pp list

      Append list xs ->
        pp list <+> "=" <+> brackets (hsep (punctuate comma (map pp xs)))
                 <+> "++" <+> pp list

      SetList list xs ->
        pp list <+> "=" <+> brackets (hsep (punctuate comma (map pp xs)))

      NewRef r e    -> pp r  <+> "=" <+> "newRef" <+> pp e
      ReadRef r1 r2 -> pp r1 <+> "=" <+> "readRef" <+> pp r2
      WriteRef r1 e -> "writeRef" <+> pp r1 <+> pp e

      CloseStack r    -> "closeStack" <+> pp r
      NewClosure r i f ->
        pp r <+> "=" <+> "newClosure" <>
            brackets (int i) <+> hsep (map pp (funcUpvalRefExprs f))
            -- NOTE: we can print things either way, but usually we
            -- look at the analyzed code, so the Ref version makes more sense.

      Comment x -> "--" <+> text x



instance Pretty Op1 where
  pp op =
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


instance Pretty Op2 where
  pp op =
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


instance Pretty Prop where
  pp (Prop p args) =
    case args of
      [l,r] -> pp l <+> pp p <+> pp r
      _     -> pp p <+> hsep (map pp args)

instance Pretty Pred where
  pp pre =
    case pre of
      IsInteger -> "isInteger"
      IsNaN     -> "isNan"
      IsNone    -> "isNone"
      Equals    -> "="
      NumberLT  -> "<"
      NumberLEQ -> "<="
      StringLT  -> "<S"
      StringLEQ -> "<=S"

instance Pretty BlockName where
  pp bn =
    case bn of
      EntryBlock   -> "ENTRY"
      PCBlock n    -> "PC" <> int n
      NewBlock p n -> "PC" <> int p <> "_" <> int n

ppBlocks :: Map BlockName Block -> Doc
ppBlocks = vcat . map ppBlock . Map.toList

ppBlock :: (BlockName, Block) -> Doc
ppBlock (nm,xs) = (pp nm <> colon) $$ nest 2 (pp xs)

ppDot :: Map BlockName Block -> Doc
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
    pp nm <>
      brackets ("label=" <>
                  text (left $ show $ show (ppBlock (nm,stms)))) <> semi

  left ('\\' : 'n' : cs) = '\\' : 'l' : left cs
  left ['"']             = "\\l\""
  left (x : xs)          = x : left xs
  left []                = []

  edges (nm,stmts) = vcat [ edge nm x | x <- followers (blockEnd stmts) ]
  edge a (mbLab,b) =
    pp a <+> "->" <+> pp b <+> maybe empty ppLab mbLab <> semi
    where ppLab l = brackets ("label=" <> text (show l))


  followers s =
    case stmtCode s of
      Goto x      -> [ (Nothing,x) ]
      Case _ as b -> [ (Just (show (pp t)),a) | (t,a) <- as ] ++
                     maybe [] (\x -> [(Just "otherwise",x)]) b
      If _ t e    -> [ (Just "true", t), (Just "false", e) ]
      _           -> []


