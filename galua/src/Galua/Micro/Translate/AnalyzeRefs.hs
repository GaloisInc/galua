-- | Determines what registers are captured into closures.
-- These registers are represented as references to values in the heap.
module Galua.Micro.Translate.AnalyzeRefs where

import qualified Galua.Code as Code
import           Galua.Micro.AST hiding (Block(..),blockNext)
import qualified Galua.Micro.AST as AST

import           Data.Set(Set)
import qualified Data.Set as Set
import           Data.Map(Map)
import qualified Data.Map as Map
import           Data.List(foldl',union)

-- | Compute what registers contain references at the beginning of each block.
analyze :: Code.Reg -> Map BlockName AST.Block -> Map BlockName (Set Code.Reg)
analyze lim orig = Map.mapWithKey (\b _ -> atStart b) orig
  where
  prog       = makeProg lim orig
  endOfBlock = flowAnalysis prog
  atStart b  = incomingRefs prog endOfBlock b


makeProg :: Code.Reg -> Map BlockName AST.Block -> Prog
makeProg lim mp = Prog { predecessors = preds
                       , blocks       = blks
                       , maxReg       = lim
                       }
  where
  blks = fmap toBlock mp

  toBlock ss = Block { blockStmts = ss
                     , blockNext  = AST.blockNext ss
                     }

  preds = Map.fromListWith union
              [ (x,[y]) | (y,bl) <- Map.toList blks, x <- blockNext bl  ]



--------------------------------------------------------------------------------

-- | For each block, what registers contains references at the *end* of
-- the block.
type RefMap = Map BlockName (Set Code.Reg)

data Prog = Prog { predecessors :: Map BlockName [ BlockName ]
                 , blocks       :: Map BlockName Block
                 , maxReg       :: Code.Reg
                 }

data Block  = Block { blockNext   :: [ BlockName ]
                    , blockStmts  :: AST.Block
                    }

-- | Given a set of registers that are known to be references at the
-- beginning of a block, compute what registers are references at the end.
refsForBlock :: Code.Reg -> Set Code.Reg -> Block -> Set Code.Reg
refsForBlock lim incoming = foldl' updEsc incoming . cvt lim . blockStmts
  where
  updEsc live s =
    case s of
      Capture x -> Set.insert x live
      Close x   -> Set.delete x live
      Use _     -> live


-- | Compute what registers are known to be references at the beginning of
-- a block, by looking at its predecessors.
incomingRefs :: Prog -> RefMap -> BlockName -> Set Code.Reg
incomingRefs prog esc l =
  Set.unions [ Map.findWithDefault Set.empty p esc
                  | p <- Map.findWithDefault [] l (predecessors prog) ]


-- | For each block, we are tracking which registers contain references
-- at *the end* of each block.
flowAnalysis :: Prog -> RefMap
flowAnalysis fun = go (Map.keysSet (blocks fun)) Map.empty
  where
  go :: Set BlockName -> RefMap -> RefMap
  go todo mp =
    case Set.minView todo of
      Nothing -> mp
      Just (b,bs) ->
        let oldRefs   = Map.lookup b mp
            incoming  = incomingRefs fun mp b
            bl        = blocks fun Map.! b
            new       = refsForBlock (maxReg fun) incoming bl

            (newTodo,newMp)
              | oldRefs == Just new =
                             ( bs
                             , mp
                             )
              | Nothing <- oldRefs, Set.null new =
                             ( bs
                             , Map.insert b Set.empty mp
                             )
              | otherwise  = ( foldr Set.insert bs (blockNext bl)
                             , Map.insert b new mp
                             )
        in go newTodo newMp


--------------------------------------------------------------------------------

class Uses t where
  uses :: t -> Set Code.Reg

instance Uses Reg where
  uses reg =
    case reg of
      TMP _ _ -> Set.empty
      Reg r   -> Set.singleton r
      Ref {}  -> error "analyzeRefs: Ref before analysis"

instance Uses Expr where
  uses expr =
    case expr of
      ELit _ -> Set.empty
      EUp _  -> Set.empty
      EReg r -> uses r

instance Uses a => Uses [a] where
  uses = Set.unions . map uses

instance Uses a => Uses (Maybe a) where
  uses = maybe Set.empty uses

instance Uses () where
  uses _ = Set.empty

instance (Uses a, Uses b) => Uses (a,b) where
  uses (a,b) = Set.union (uses a) (uses b)

instance (Uses a, Uses b, Uses c) => Uses (a,b,c) where
  uses (a,b,c) = Set.union (uses a) (uses (b,c))

instance Uses Prop where
  uses (Prop _ es) = uses es

instance Uses a => Uses (BlockStmt a) where
  uses = uses . stmtCode

instance Uses EndStmt where
  uses stmt =
    case stmt of
      Raise x             -> uses x
      Goto _              -> uses ()
      Case e _ _          -> uses e
      If p _ _            -> uses p
      TailCall x          -> uses x
      Return              -> uses ()




instance Uses Stmt where
  uses stmt =
    case stmt of
      CloseStack _        -> uses () -- THESE ARE NOT USED, THEY ARE CLOSED

      Assign x y          -> uses (x,y)
      SetUpVal _ x        -> uses x
      NewTable x          -> uses x
      LookupTable x y z   -> uses (x,y,z)
      SetTable x y z      -> uses (x,y,z)
      SetTableList x _    -> uses x
      GetMeta x y         -> uses (x,y)

      Call x              -> uses x
      NewClosure x _ y    -> uses (x, funcUpvalExprs y)

      Drop {}             -> uses ()
      Append _ x          -> uses x
      SetList _ x         -> uses x
      AssignListReg {}    -> uses ()

      IndexList x _ _     -> uses x
      Arith2 x _ y z      -> uses (x,y,z)
      Arith1 x _ y        -> uses (x,y)

      NewRef   _ _        -> error "Uses: NewRef"
      ReadRef  _ _        -> error "Uses: ReadRef"
      WriteRef _ _        -> error "Uses: WriteRef"

      Comment _           -> uses ()


data Simple = Use     Code.Reg
            | Capture Code.Reg
            | Close   Code.Reg


cvt :: Code.Reg -> AST.Block -> [Simple]
cvt lim b = concatMap (cvtStmt lim) (AST.blockBody b) ++
                        cvtEnd lim (AST.blockEnd b)

cvtEnd :: Code.Reg -> BlockStmt EndStmt -> [Simple]
cvtEnd lim stmt =
  case stmtCode stmt of
    Return -> [ Close i | i <- Code.regFromTo (Code.Reg 0) lim ]
    _      -> makeUses stmt


cvtStmt :: Code.Reg -> BlockStmt Stmt -> [Simple]
cvtStmt lim stmt =
  case stmtCode stmt of
    CloseStack (Reg r) -> [ Close i | i <- Code.regFromTo r lim ]
    CloseStack _       -> error "CloseStack: non Code.Reg"

    NewClosure x _ f   -> makeUses es ++
                             [ Capture r | EReg (Reg r) <- es ] ++
                             makeUses x
      where es = funcUpvalExprs f

    _ -> makeUses stmt

makeUses :: Uses a => a -> [ Simple ]
makeUses x = [ Use r | r <- Set.toList (uses x) ]


