{-# Language GeneralizedNewtypeDeriving, NamedFieldPuns, TypeSynonymInstances #-}
-- Renubmer all temporaries so that they are sequential.
-- XXX: We can probably get rid of this eventually; it was here while
-- as it makes interpreting micro code easier;
-- the compiler also current assumes that inputs are sequential but only
-- to determine how many there are, pretty much.
module Galua.Micro.Translate.RenumberTMP (renumberTMP) where

import           MonadLib
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Map ( Map )
import qualified Data.Map as Map
import           Data.Vector as Vector

import Galua.Micro.AST
import Galua.Code(Function)

renumberTMP :: Int -> MicroFunction -> MicroFunction
renumberTMP ph mf0 = mf { functionRegsTMP = nextReg rw }
  where
  rw0     = RW { nextReg = 0, regMap = Map.empty }
  (mf,rw) = runId $ runStateT rw0 $ runReaderT ph $ unM $ renumber mf0

newtype M a = M { unM :: ReaderT RO (StateT RW Id) a }
  deriving (Functor,Applicative,Monad)

type RO = Int
data RW = RW { nextReg :: !Int, regMap :: !(Map (Int,Int) Reg) }

class Renumber t where
  renumber :: t -> M t

-- This does the interesting work, the rest propagate stuff
instance Renumber Reg where
  renumber reg =
    case reg of
      Reg {}  -> return reg
      TMP x y -> M $ do RW { nextReg, regMap } <- get
                        case Map.lookup (x,y) regMap of
                          Just r -> return r
                          Nothing ->
                            do ph <- ask
                               let r = TMP ph nextReg
                               set $! RW { nextReg = nextReg + 1
                                         , regMap = Map.insert (x,y) r regMap
                                         }
                               return r

instance Renumber Input where
  renumber input =
    case input of
      LReg x -> op1 LReg x
      IReg x -> op1 IReg x

instance Renumber Block where
  renumber (Block x y z) = op3 Block x y z

instance Renumber EndStmt where
  renumber stmt =
    case stmt of
      Raise e             -> op1 Raise e
      Case e as b         -> op3 Case e as b
      If p b1 b2          -> op3 If p b1 b2
      Goto n              -> op1 Goto n
      TailCall r          -> op1 TailCall r
      Return              -> op0 Return

instance Renumber Stmt where
  renumber stmt =
    case stmt of
      Assign r e          -> op2 Assign r e
      SetUpVal u r        -> op2 SetUpVal u r
      NewTable  r         -> op1 NewTable r
      LookupTable r1 r2 e -> op3 LookupTable r1 r2 e
      SetTable  r e1 e2   -> op3 SetTable r e1 e2
      SetTableList r n    -> op2 SetTableList r n
      GetMeta r e         -> op2 GetMeta r e


      Call r              -> op1 Call r

      CloseStack r        -> op1 CloseStack r
      NewClosure r n f    -> op3 NewClosure r n f

      Drop l n            -> op2 Drop l n

      Append r es         -> op2 Append r es
      SetList r es        -> op2 SetList r es

      IndexList r lr n    -> op3 IndexList r lr n

      Arith2 r op e1 e2   -> op4 Arith2 r op e1 e2
      Arith1 r op e       -> op3 Arith1 r op e

      NewRef   r e        -> op2 NewRef r e
      ReadRef  r e        -> op2 ReadRef r e
      WriteRef e1 e2      -> op2 WriteRef e1 e2

      Comment x           -> op0 (Comment x)






instance Renumber a => Renumber (Maybe a)  where renumber = traverse renumber
instance Renumber a => Renumber [a]        where renumber = traverse renumber
instance Renumber a => Renumber (Vector a) where renumber = traverse renumber
instance (Ord a, Renumber a) => Renumber (Set a) where
  renumber = fmap Set.fromList . traverse renumber . Set.toList

instance (Renumber a, Renumber b) => Renumber (a,b) where
  renumber (a,b) = (,) <$> renumber a <*> renumber b

instance Renumber MicroFunction where
  renumber f = mk <$> traverse renumber (functionCode f)
    where mk fc = f { functionCode = fc }

instance Renumber a => Renumber (BlockStmt a) where
  renumber bs = mk <$> renumber (stmtCode bs)
    where mk s = bs { stmtCode = s }

instance Renumber Expr where
  renumber expr =
    case expr of
      EReg r  -> EReg <$> renumber r
      ELit {} -> pure expr
      EUp {}  -> pure expr

instance Renumber Prop where
  renumber (Prop p es) = op2 Prop p es

instance Renumber Int where renumber = pure
instance Renumber ListReg where renumber = pure
instance Renumber UpIx where renumber = pure
instance Renumber Op1 where renumber = pure
instance Renumber Op2 where renumber = pure
instance Renumber BlockName where renumber = pure
instance Renumber ValueType where renumber = pure
instance Renumber Pred where renumber = pure

-- | This does not renumber nested functions.
instance Renumber Function where
  renumber = pure


--------------------------------------------------------------------------------
op0 :: a -> M a
op0 = pure

op1 :: Renumber a => (a -> b) -> a -> M b
op1 f a = f <$> renumber a

op2 :: (Renumber a, Renumber b) => (a -> b -> c) -> a -> b -> M c
op2 f a b = f <$> renumber a <*> renumber b

op3 :: (Renumber a, Renumber b, Renumber c) => (a -> b -> c -> d) ->
                                                a -> b -> c -> M d
op3 f a b c = f <$> renumber a <*> renumber b <*> renumber c

op4 :: (Renumber a, Renumber b, Renumber c, Renumber d) =>
  (a -> b -> c -> d -> e) -> a -> b -> c -> d -> M e
op4 f a b c d = f <$> renumber a <*> renumber b <*> renumber c <*> renumber d





