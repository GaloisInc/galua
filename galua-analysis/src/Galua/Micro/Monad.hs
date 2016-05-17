{-# LANGUAGE RecordWildCards, FlexibleInstances, ExistentialQuantification #-}
module Galua.Micro.Monad where

import Galua.Micro.AST

import           Data.ByteString(ByteString)
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Foldable(toList)
import           Control.Monad(ap,liftM)
import           Control.Monad.Fix(MonadFix(..))
import qualified Language.Lua.Bytecode as OP


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
  , rwBlocks    :: !(Map BlockName (Seq Stmt))
  , rwListReg   :: !(Maybe OP.Reg)
  }

generate :: M () -> Map BlockName (Vector Stmt)
generate (M m) = fmap (Vector.fromList . toList)
               $ rwBlocks
               $ snd
               $ m RO { roCurBlock = PCBlock 0 }
                   RW { rwNextBlock = 0
                      , rwNextTMP   = 0
                      , rwBlocks    = Map.empty
                      , rwListReg   = Nothing
                      }

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
              let addEnd   = flip (Seq.><)
                  addBlock = Map.insertWith addEnd roCurBlock (Seq.singleton s)
              in ((), RW { rwBlocks = addBlock rwBlocks, .. })

inNewBlock_ :: M () -> M BlockName
inNewBlock_ m =
  do l <- newLabel
     inBlock l m
     return l

setListReg :: OP.Reg -> M ()
setListReg r = M $ \_ RW {..} -> ( (), RW { rwListReg = Just r, .. } )

getListReg :: M (Maybe OP.Reg)
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
            emit (Goto l)

data Alts = forall k. ToBlockName k => IfType ValueType k Alts
          | forall k. ToBlockName k => Default k
          | NoDefault

typeCase :: IsExpr e => e -> Alts -> M ()
typeCase e as0 = do (arms,d) <- alts [] as0
                    emit (Case (toExpr e) arms d)
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
     emit (If p ifYes ifNo)

ifNil :: (IsExpr e, ToBlockName yes, ToBlockName no) => e -> yes -> no -> M ()
ifNil e yes no = typeCase e $ IfType NilType yes $ Default no

ifNone :: (IsExpr e, ToBlockName yes, ToBlockName no) => e -> yes -> no -> M ()
ifNone e = ite (Prop IsNone [toExpr e])





--------------------------------------------------------------------------------

(=:) :: IsExpr e => Reg -> e -> M ()
x =: y = emit (Assign x (toExpr y))

arith1 :: IsExpr e => Reg -> Op1 -> e -> M ()
arith1 r op e = emit (Arith1 r op (toExpr e))

arith2 :: (IsExpr e1, IsExpr e2) => Reg -> Op2 -> e1 -> e2 -> M ()
arith2 r op e1 e2 = emit (Arith2 r op (toExpr e1) (toExpr e2))

raiseError :: ByteString -> M ()
raiseError = emit . Raise . toExpr



