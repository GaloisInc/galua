module Galua.Micro.Translate.ComputeInputs(computeBlockInputs) where

import           Data.Set(Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Vector as Vector
import           Data.List(mapAccumL)
import           Data.Foldable(foldl')
import           Data.Graph.SCC
import           Data.Graph(SCC(..))

import Galua.Micro.AST


-- | Compute the inputs needed by each of the basic blocks.
computeBlockInputs :: MicroFunction -> MicroFunction
computeBlockInputs mf = foldl' computeInputsSCC mf
                      $ stronglyConnCompR
                      $ map toNode
                      $ Map.toList
                      $ functionCode mf
  where
  toNode (l,b) = (b,l,blockNext b)


type BlockNode = (Block, BlockName, [BlockName])

computeInputsSCC :: MicroFunction -> SCC BlockNode -> MicroFunction
computeInputsSCC mf s =
  case s of
    AcyclicSCC b -> fst (computeInputs mf b)
    CyclicSCC bs -> computeInputsRec mf bs

-- | Compute the inputs for a group of mutually recursive basic blocks.
computeInputsRec :: MicroFunction -> [BlockNode] -> MicroFunction
computeInputsRec mf bs = if done then newMF else computeInputsRec newMF newBs
  where
  (newMF,newBs)   = mapAccumL computeInputs mf bs
  done            = and (zipWith sameInputs newBs bs)

  sameInputs (x,_,_) (y,_,_)  = blockInputs x == blockInputs y


-- | Compute the inputs for a single block, assuming that we know
-- the inputs for its successors.
computeInputs :: MicroFunction -> BlockNode -> (MicroFunction, BlockNode)
computeInputs mf (b,x,xs) = ( mf { functionCode = newCode }
                            , (newBlock, x, xs)
                            )
  where
  newCode          = Map.insert x newBlock (functionCode mf)
  newBlock         = b { blockInputs = newInputs }

  newInputs        = foldl' stmtStep beforeLast (Vector.reverse (blockBody b))

  beforeLast       = Set.unions (uses (blockEnd b) : map inputsFor xs)
  stmtStep after s = Set.union (uses s) (after `Set.difference` defines s)

  inputsFor l      = case Map.lookup l (functionCode mf) of
                       Just b' -> blockInputs b'
                       Nothing -> error "computeInputs: missing block"


--------------------------------------------------------------------------------

class Uses t where
  uses :: t -> Set Input

instance Uses Reg where
  uses r = Set.singleton (IReg r)

instance Uses ListReg where
  uses r = Set.singleton (LReg r)

instance Uses a => Uses [a] where
  uses = foldl' (\xs x -> Set.union (uses x) xs) Set.empty

instance (Uses a, Uses b) => Uses (a,b) where
  uses (x,y) = Set.union (uses x) (uses y)

instance (Uses a, Uses b, Uses c) => Uses (a,b,c) where
  uses (x,y,z) = uses (x,(y,z))

instance Uses Expr where
  uses expr =
    case expr of
      EReg r  -> Set.singleton (IReg r)
      ELit {} -> Set.empty
      EUp {}  -> Set.empty


instance Uses Prop where
  uses (Prop _ xs) = uses xs

instance Uses a => Uses (BlockStmt a) where
  uses x = uses (stmtCode x)

instance Uses Stmt where
  uses stmt =
    case stmt of
      Assign _ e        -> uses e
      NewTable _        -> Set.empty
      LookupTable _ r e -> uses (r,e)
      SetTable r e1 e2  -> uses (r,e1,e2)
      SetTableList r _  -> uses (r,ListReg)
      GetMeta _ e       -> uses e

      NewClosure _ _ f  -> uses (funcUpvalRefExprs f)
      Call r            -> uses (r,ListReg)

      Drop r _          -> uses r
      Append r es       -> uses (r,es)
      SetList _ es      -> uses es
      AssignListReg _ y -> uses y
      IndexList _ r _   -> uses r

      Arith2 _ _ e1 e2  -> uses (e1,e2)
      Arith1 _ _ e      -> uses e

      NewRef _ e        -> uses e
      ReadRef _ e       -> uses e
      WriteRef e1 e2    -> uses (e1,e2)

      Comment {}        -> Set.empty

      SetUpVal {}       -> error "uses: SetUpVal"
      CloseStack {}     -> error "uses: CloseStack"


instance Uses EndStmt where
  uses stmt =
    case stmt of
      -- For local contral flow, we also need the arguments for
      -- the blocks that we may call.
      Case e _ _  -> uses e
      If p _ _    -> uses p
      Goto _      -> Set.empty

      TailCall r  -> uses (r,ListReg)
      Return      -> uses ListReg
      Raise e     -> uses e


defines :: BlockStmt Stmt -> Set Input
defines stmt =
  case stmtCode stmt of
    Assign r _          -> iReg r
    NewTable r          -> iReg r
    LookupTable r1 _ _  -> iReg r1
    SetTable {}         -> none
    SetTableList _ _    -> none
    GetMeta r _         -> iReg r

    NewClosure r _ _    -> iReg r
    Call _              -> lReg ListReg

    Drop r _            -> lReg r
    Append r _          -> lReg r
    SetList r _         -> lReg r
    AssignListReg r _   -> lReg r
    IndexList r _ _     -> iReg r

    Arith2 r _ _ _      -> iReg r
    Arith1 r _ _        -> iReg r

    NewRef r _          -> iReg r
    ReadRef r _         -> iReg r
    WriteRef _ _        -> none

    Comment {}          -> none

    SetUpVal {}         -> error "defines: SetUpVal"
    CloseStack {}       -> error "defines: CloseStack"

  where
  iReg = Set.singleton . IReg
  lReg = Set.singleton . LReg
  none = Set.empty

