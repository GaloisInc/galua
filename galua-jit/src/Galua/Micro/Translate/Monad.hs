{-# LANGUAGE RecordWildCards, FlexibleInstances, ExistentialQuantification #-}
module Galua.Micro.Translate.Monad where


import           Data.ByteString(ByteString)
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Vector as Vector
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Foldable(toList)
import           Control.Monad(ap,liftM)
import           Control.Monad.Fix(MonadFix(..))

import Galua.Micro.AST
import qualified Galua.Code as Code

newtype M a = M (RO -> RW -> (a,RW))

instance Functor M where
  fmap = liftM

instance Applicative M where
  pure a = M (\_ s -> (a,s))
  (<*>)  = ap

instance Monad M where
  M m >>= f = M $ \r s -> case m r s of
                            (a,s1) | M m1 <- f a -> m1 r s1

instance MonadFix M where
  mfix f = M $ \r s -> let M m1 = f a
                           res@(a,_) = m1 r s
                       in res
data RO = RO
  { roCurBlock :: !BlockName
  }

data RW = RW
  { rwNextBlock :: !Int
  , rwNextTMP   :: !Int
  , rwBlocks    :: !(Map BlockName WorkBlock)
  , rwListReg   :: !(Maybe Code.Reg)
  }

data WorkBlock = Finished Block | Partial (Seq (BlockStmt Stmt))


generate :: M () -> Map BlockName Block
generate (M m) = fmap getDone
               $ rwBlocks
               $ snd
               $ m RO { roCurBlock = PCBlock 0 }
                   RW { rwNextBlock = 0
                      , rwNextTMP   = 0
                      , rwBlocks    = Map.empty
                      , rwListReg   = Nothing
                      }
  where
  getDone (Finished b) = b
  getDone (Partial _)  = error "Partial block"

newLabel :: M BlockName
newLabel = M $ \RO {..} RW {..} ->
  ( NewBlock (blockNamePC roCurBlock) rwNextBlock
  , RW { rwNextBlock = rwNextBlock + 1, .. }
  )

newPhaseTMP :: Int -> M Reg
newPhaseTMP p = M $ \_ RW {..} -> ( TMP p rwNextTMP
                                  , RW { rwNextTMP = rwNextTMP + 1, .. })

inBlock :: BlockName -> M a -> M a
inBlock b (M m) = M $ \RO { .. } rw -> m RO { roCurBlock = b, .. } rw

emit :: Stmt -> M ()
emit s = M $ \RO { .. } RW { .. } ->
              let upd ~(Partial new)
                      ~(Partial old) = Partial (old Seq.>< new)
                  pc  = blockNamePC roCurBlock
                  bs  = Partial $ Seq.singleton
                                $ BlockStmt { stmtPC = pc, stmtCode = s }
                  addBlock = Map.insertWith upd roCurBlock bs
              in ((), RW { rwBlocks = addBlock rwBlocks, .. })

emitEnd :: EndStmt -> M ()
emitEnd s = M $ \RO { .. } RW { .. } ->
  let upd mb =
        let stmts = case mb of
                      Nothing -> Vector.empty
                      Just ~(Partial se) -> Vector.fromList (toList se)
        in Just $
             Finished
               Block { blockBody = stmts
                     , blockEnd  = BlockStmt { stmtPC = blockNamePC roCurBlock
                                             , stmtCode = s
                                             }
                     , blockInputs = Set.empty -- Added later
                     }
  in ((), RW { rwBlocks = Map.alter upd roCurBlock rwBlocks, .. })




inNewBlock_ :: M () -> M BlockName
inNewBlock_ m =
  do l <- newLabel
     inBlock l m
     return l

setListReg :: Code.Reg -> M ()
setListReg r = M $ \_ RW {..} -> ( (), RW { rwListReg = Just r, .. } )

getListReg :: M (Maybe Code.Reg)
getListReg = M $ \_ RW {..} -> ( rwListReg, RW { rwListReg = Nothing, .. } )



--------------------------------------------------------------------------------

class ToBlockName a where
  toBlockName :: a -> M BlockName

instance ToBlockName (M ()) where
  toBlockName m = inNewBlock_ m

instance ToBlockName BlockName where
  toBlockName l = return l

goto :: ToBlockName a => a -> M ()
goto a = do l <- toBlockName a
            emitEnd (Goto l)

data Alts = forall k. ToBlockName k => IfType ValueType k Alts
          | forall k. ToBlockName k => Default k
          | NoDefault

typeCase :: IsExpr e => e -> Alts -> M ()
typeCase e as0 = do (arms,d) <- alts [] as0
                    emitEnd (Case (toExpr e) arms d)
  where
  alts cvted as = case as of
                    IfType t k next -> do l <- toBlockName k
                                          alts ((t,l) : cvted) next
                    Default k       -> do l <- toBlockName k
                                          return (reverse cvted, Just l)
                    NoDefault       -> return (reverse cvted, Nothing)

ite :: (ToBlockName yes, ToBlockName no) => Prop -> yes -> no -> M ()
ite p yes no =
  do ifYes <- toBlockName yes
     ifNo  <- toBlockName no
     emitEnd (If p ifYes ifNo)

ifNil :: (IsExpr e, ToBlockName yes, ToBlockName no) => e -> yes -> no -> M ()
ifNil e yes no = typeCase e $ IfType NilType yes $ Default no




--------------------------------------------------------------------------------

(=:) :: IsExpr e => Reg -> e -> M ()
x =: y = emit (Assign x (toExpr y))

arith1 :: IsExpr e => Reg -> Op1 -> e -> M ()
arith1 r op e = emit (Arith1 r op (toExpr e))

arith2 :: (IsExpr e1, IsExpr e2) => Reg -> Op2 -> e1 -> e2 -> M ()
arith2 r op e1 e2 = emit (Arith2 r op (toExpr e1) (toExpr e2))

raiseError :: ByteString -> M ()
raiseError = emitEnd . Raise . toExpr



