module Galua.Micro.AnalyzeRefs where

import qualified Language.Lua.Bytecode as OP
import           Galua.Micro.AST

import           Data.Set(Set)
import qualified Data.Set as Set
import           Data.Map(Map)
import qualified Data.Map as Map
import           Data.Vector(Vector)
import           Data.List(foldl',union)
import           Data.Maybe(maybeToList)
import           Data.Foldable(toList)

-- | Compute what registers contains references at the *beginning* of a block
analyze :: OP.Reg -> Map BlockName (Vector Stmt) -> Map BlockName (Set OP.Reg)
analyze lim orig = Map.mapWithKey (\b _ -> incomingRefs prog endOfBlock b) orig
  where
  prog       = makeProg lim orig
  endOfBlock = flowAnalysis prog


makeProg :: OP.Reg -> Map BlockName (Vector Stmt) -> Prog
makeProg lim mp = Prog { predecessors = preds
                       , blocks       = blks
                       , maxReg       = lim
                       }
  where
  blks = fmap toBlock mp

  getNext stmt =
    case stmt of
      Goto l      -> [l]
      If _ t f    -> [t,f]
      Case _ as d -> map snd as ++ maybeToList d
      _           -> []

  toBlock ss = Block { blockStmts = ss
                     , blockNext  = concatMap getNext (toList ss)
                     }

  preds = Map.fromListWith union
              [ (x,[y]) | (y,bl) <- Map.toList blks, x <- blockNext bl  ]



--------------------------------------------------------------------------------

-- | For each block, what registers contains references at the *end* of
-- the block.
type RefMap = Map BlockName (Set OP.Reg)

data Prog = Prog { predecessors :: Map BlockName [ BlockName ]
                 , blocks       :: Map BlockName Block
                 , maxReg       :: OP.Reg
                 }

data Block  = Block { blockNext   :: [ BlockName ]
                    , blockStmts  :: Vector Stmt
                    }

-- | Given a set of registers that are known to be references at the
-- beginning of a block, compute what registers are references at the end.
refsForBlock :: OP.Reg -> Set OP.Reg -> Block -> Set OP.Reg
refsForBlock lim incoming =
  foldl' updEsc incoming . concatMap (cvt lim) . toList . blockStmts
  where
  updEsc live s =
    case s of
      Capture x -> Set.insert x live
      Close x   -> Set.delete x live
      Use _     -> live


-- | Compute what registers are known to be references at the beginning of
-- a block, by looking at its predecessors.
incomingRefs :: Prog -> RefMap -> BlockName -> Set OP.Reg
incomingRefs prog esc l =
  Set.unions [ Map.findWithDefault Set.empty p esc
                  | p <- Map.findWithDefault [] l (predecessors prog) ]


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
  uses :: t -> Set OP.Reg

instance Uses Reg where
  uses reg =
    case reg of
      TMP _ _ -> Set.empty
      Reg r   -> Set.singleton r

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
      Raise x             -> uses x
      Goto _              -> uses ()
      Case e _ _          -> uses e
      If p _ _            -> uses p

      Call x              -> uses x
      TailCall x          -> uses x
      Return              -> uses ()

      NewClosure x _ y    -> uses (x,y)

      Drop {}             -> uses ()
      Append _ x          -> uses x
      SetList _ x         -> uses x

      IndexList x _ _     -> uses x
      Arith2 x _ y z      -> uses (x,y,z)
      Arith1 x _ y        -> uses (x,y)

      NewRef   _ _        -> error "Uses: NewRef"
      ReadRef  _ _        -> error "Uses: ReadRef"
      WriteRef _ _        -> error "Uses: WriteRef"

      Comment _           -> uses ()


data Simple = Use     OP.Reg
            | Capture OP.Reg
            | Close   OP.Reg

cvt :: OP.Reg -> Stmt -> [Simple]
cvt lim stmt =
  case stmt of
    CloseStack (Reg r) -> [ Close i | i <- [ r .. lim ] ]
    CloseStack _       -> error "CloseStack: non OP.Reg"

    Return            -> [ Close i | i <- [ OP.Reg 0 .. lim ] ]

    NewClosure x _ es     -> makeUses es ++
                             [ Capture r | EReg (Reg r) <- es ] ++
                             makeUses x

    _                        -> makeUses stmt

  where
  makeUses x = [ Use r | r <- Set.toList (uses x) ]



