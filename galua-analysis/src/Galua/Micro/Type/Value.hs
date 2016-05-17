{-# LANGUAGE DeriveGeneric, TypeOperators, FlexibleContexts, RecordWildCards #-}
module Galua.Micro.Type.Value
  (module Galua.Micro.Type.Value, FunId, subFun, noFun)
  where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Data.List(intercalate)
import Data.Maybe(fromMaybe,isNothing)
import Data.ByteString(ByteString)
import GHC.Generics
import Control.Monad (liftM3, liftM, ap)
import Text.PrettyPrint(text)

import Language.Lua.Bytecode.Pretty(PP(..),pp)
import Language.Lua.Bytecode.FunId

import Galua.Micro.AST


-- | An abstract value.
data SingleV      = BasicValue        Type
                  | StringValue       (Maybe ByteString) -- ^ Nothing = top
                  | TableValue        (Maybe TableId)
                  | FunctionValue     (Maybe FunId)
                  | RefValue          (Maybe RefId)
                    deriving Eq

data Type         = Nil | Bool | Number | UserData | LightUserData | Thread
                    deriving (Show,Eq,Ord,Bounded,Enum)

data FieldName    = Metatable | Field ByteString
                    deriving (Show,Eq,Ord)

-- | This is a block name indexed by the current callsite.
-- It exists to allow instantiating a function separately for each
-- callsite.
data GlobalBlockName = GlobalBlockName CallsiteId QualifiedBlockName
                        deriving (Eq,Ord,Show)

data QualifiedBlockName = QualifiedBlockName FunId BlockName
                    deriving (Show,Eq,Ord)

data CallsiteId = CallsiteId QualifiedBlockName Int
                    deriving (Show,Eq,Ord)

initialCaller :: CallsiteId
initialCaller = CallsiteId (QualifiedBlockName noFun EntryBlock) 0


data RefId        = RefId GlobalBlockName Int
                    deriving (Show,Eq,Ord)

data TableId      = TableId GlobalBlockName Int
                    deriving (Show,Eq,Ord)


--------------------------------------------------------------------------------
-- Lattices

data GlobalState = GlobalState
  { tables      :: Map TableId TableV
  , heap        :: Map RefId   Value

  , basicMetas  :: Type :-> Value
  , stringMeta  :: Value
  , funMeta     :: Value
  } deriving (Eq,Show,Generic)

data LocalState = LocalState
  { env         :: Map Reg Value
  , argReg      :: List Value
  , listReg     :: List Value
  } deriving (Eq,Show,Generic)

data State = State
  { globaleState :: GlobalState
  , localState   :: LocalState
  } deriving (Eq,Show,Generic)

data Value        = Value { valueBasic    :: Set Type
                          , valueString   :: Lift ByteString
                          , valueFunction :: WithTop (Set FunId)
                          , valueTable    :: WithTop (Set TableId)
                          , valueRef      :: WithTop (Set RefId)
                          } deriving (Eq,Generic,Show)

data TableV       = TableV { tableFields  :: FieldName :-> Value
                           , tableKeys    :: Value
                           , tableValues  :: Value
                           } deriving (Generic,Show,Eq)




data FunBehavior  = FunBehavior
                      { funUpVals :: [Value]
                      , funArgs   :: List Value
                      , funPost   :: [Value] -> List Value -> FunPost
                      }

data FunPost      = FunPost
                      { funReturns    :: List Value
                      , funRaises     :: Value
                      , funModTables  :: Set TableId
                      , funModRefs    :: Set RefId
                      }
                    deriving (Eq,Show,Generic)

data Lift a       = NoValue | OneValue a | MultipleValues
                    deriving (Eq,Show)

data WithTop a    = NotTop a | Top
                    deriving (Eq,Show)

instance Functor     WithTop where fmap = liftM
instance Applicative WithTop where pure = NotTop
                                   (<*>) = ap
instance Monad       WithTop where Top      >>= _ = Top
                                   NotTop x >>= f = f x


--------------------------------------------------------------------------------


valueCases :: Value -> [SingleV]
valueCases Value { .. } =
  (BasicValue <$> Set.toList valueBasic) ++
  (case valueString of
     NoValue        -> []
     OneValue a     -> [StringValue (Just a)]
     MultipleValues -> [StringValue Nothing]
   ) ++
   (ifTop TableValue    valueTable) ++
   (ifTop FunctionValue valueFunction) ++
   (ifTop RefValue      valueRef)
  where
  ifTop f x = case x of
                Top      -> [ f Nothing ]
                NotTop a -> map (f . Just) (Set.toList a)

fromSingleV :: SingleV -> Value
fromSingleV val =
  case val of
    BasicValue t     -> bottom { valueBasic    = Set.singleton t }
    StringValue mb   -> bottom { valueString   = case mb of
                                                  Nothing -> MultipleValues
                                                  Just s  -> OneValue s }
    TableValue t     -> bottom { valueTable    = toTop t }
    FunctionValue f  -> bottom { valueFunction = toTop f }
    RefValue r       -> bottom { valueRef      = toTop r }
  where
  toTop mb = case mb of
               Nothing -> Top
               Just a  -> NotTop (Set.singleton a)


--------------------------------------------------------------------------------

basic :: Type -> Value
basic = fromSingleV . BasicValue

anyString :: Value
anyString = fromSingleV (StringValue Nothing)

newRef :: RefId -> Value
newRef = fromSingleV . RefValue . Just

newTable :: TableId -> Value
newTable = fromSingleV . TableValue . Just

newFun :: FunId -> Value
newFun = fromSingleV . FunctionValue . Just

topVal :: Value
topVal = Value { valueBasic    = Set.fromList [ minBound .. maxBound ]
               , valueString   = MultipleValues
               , valueFunction = Top
               , valueTable    = Top
               , valueRef      = Top
               }




--------------------------------------------------------------------------------

initLuaArgList :: List Value
initLuaArgList = listConst initLuaParam
  where
  initLuaParam =
    Value { valueBasic    = Set.fromList [ minBound .. maxBound ]
          , valueString   = MultipleValues
          , valueFunction = Top
          , valueTable    = Top
          , valueRef      = bottom     -- no refs
          }







--------------------------------------------------------------------------------


data a :-> b      = FFun (Map a b) b
                    deriving (Eq,Show)

fConst :: b -> (a :-> b)
fConst b = FFun Map.empty b

appFun :: Ord a => (a :-> b) -> a -> b
appFun (FFun mp b) a = Map.findWithDefault b a mp

appAll :: Lattice b => (a :-> b) -> b
appAll (FFun mp b) = foldr (\/) b (Map.elems mp)

-- | Overwrite the mapping for a key.
letFun :: Ord a => a -> b -> (a :-> b) -> (a :-> b)
letFun x y (FFun mp d) = FFun (Map.insert x y mp) d

-- | Add the option that any value might be `b`
letFunAll :: Lattice b => b -> (a :-> b) -> (a :-> b)
letFunAll y (FFun mp d) = FFun (fmap (y \/) mp) (y \/ d)

--------------------------------------------------------------------------------

-- | Having an explicit bottom helps avoid recursive values, when
-- implementing a list of values.
data List a = ListBottom
            | List
                Int -- The list contains at least this many elements
                [a] a -- Neither of the 'a's should be bottom.
                      -- These are strict lists.
              deriving (Show,Eq)

listDrop :: Int -> List a -> List a
listDrop n list =
  case list of
    ListBottom -> ListBottom
    List count xs d  -> List (max 0 (count-n)) (drop n xs) d

listConst :: (Eq a, Lattice a) => a -> List a
listConst a
  | a == bottom = ListBottom
  | otherwise   = List 0 [] a

listAppend :: (Eq a, Lattice a) => [a] -> List a -> List a
listAppend xs list
  | any (== bottom) xs  = ListBottom
  | otherwise =
     case list of
       ListBottom -> ListBottom
       List count [] d  -> List (length xs + count) xs d
       List count as d  -> List (length xs + count) (xs ++ as) d

appList :: Lattice a => List a -> Int -> a
appList list x =
  case list of
    ListBottom  -> bottom
    List _ xs d -> case drop x xs of
                     v:_ -> v
                     _   -> d

appListAll :: Lattice a => List a -> a
appListAll list =
  case list of
    ListBottom -> bottom
    List _ xs d  -> foldr (\/) d xs

--------------------------------------------------------------------------------

appFinMap :: (Ord a, Lattice b) => Map a b -> a -> b
appFinMap mp a = Map.findWithDefault bottom a mp



--------------------------------------------------------------------------------

class Lattice a where
  bottom     :: a
  addNewInfo :: a {-^new-} -> a {-^old-} -> Maybe a

(\/) :: Lattice a => a -> a -> a
(\/) x y = fromMaybe y (addNewInfo x y)

joins :: Lattice a => [a] -> a
joins [x] = x -- just a common speed up case
joins xs  = foldr (\/) bottom xs


class Meet a where
  (/\) :: a -> a -> a

instance Meet Value where
  v1 /\ v2 =
    Value { valueBasic    = valueBasic    v1 /\ valueBasic    v2
          , valueString   = valueString   v1 /\ valueString   v2
          , valueFunction = valueFunction v1 /\ valueFunction v2
          , valueTable    = valueTable    v1 /\ valueTable    v2
          , valueRef      = valueRef      v1 /\ valueRef      v2
          }

instance Lattice a => Lattice (WithTop a) where
  bottom = NotTop bottom
  addNewInfo _ Top = Nothing
  addNewInfo Top _ = Just Top
  addNewInfo (NotTop x) (NotTop y) = NotTop <$> addNewInfo x y

instance Meet a => Meet (WithTop a) where
  Top /\ x = x
  x /\ Top = x
  NotTop x /\ NotTop y = NotTop (x /\ y)

instance Ord a => Meet (Set a) where
  (/\) = Set.intersection

instance Eq a => Meet (Lift a) where
  x /\ y =
    case (x,y) of
      (NoValue,_)        -> NoValue
      (_,NoValue)        -> NoValue
      (MultipleValues,_) -> y
      (_,MultipleValues) -> x
      (OneValue a, OneValue b)
        | a == b         -> OneValue a
        | otherwise      -> NoValue


instance Ord a => Lattice (Set a) where
  bottom  = Set.empty
  addNewInfo new old
    | new `Set.isSubsetOf` old = Nothing
    | otherwise                = Just (Set.union new old)

instance (Ord a, Lattice b) => Lattice (Map a b) where
  bottom  = Map.empty
  addNewInfo new old
    | Map.null reallyNew = Nothing
    | otherwise          = Just (Map.union reallyNew old)
    where
    reallyNew  = Map.mergeWithKey both onlyNew onlyOld new old
    onlyOld _  = Map.empty
    onlyNew x  = x
    both _ n o = addNewInfo n o

instance (Ord a, Lattice b) => Lattice (a :-> b) where
  bottom = fConst bottom
  addNewInfo (FFun new newD) (FFun old oldD)
    | Map.null reallyNew = do d <- curDef
                              return (FFun old d)
    | otherwise          = Just (FFun (Map.union reallyNew old)
                                      (fromMaybe oldD curDef))
    where
    curDef     = addNewInfo newD oldD
    reallyNew  = Map.mergeWithKey both onlyNew onlyOld new old
    onlyOld o  = Map.mapMaybe (addNewInfo newD) o
    onlyNew n  = Map.mapMaybe (`addNewInfo` oldD) n
    both _ n o = addNewInfo n o


instance Eq a => Lattice (Lift a) where
  bottom = NoValue

  addNewInfo new old =
    case (new,old) of
      (_,MultipleValues)  -> Nothing
      (MultipleValues,_)  -> Just MultipleValues

      (NoValue,_)         -> Nothing
      (a, NoValue)        -> Just a

      (OneValue a, OneValue b)
        | a == b          -> Nothing
        | otherwise       -> Just MultipleValues


instance (Eq a, Lattice a) => Lattice (List a) where
  bottom = ListBottom

  addNewInfo newList oldList =
    case (newList,oldList) of
      (ListBottom, _) -> Nothing
      (_,ListBottom)  -> Just newList
      (List newcount new newD, List oldcount old oldD) ->
        case dflt of
          Nothing
            | all isNothing updates && newcount == oldcount -> Nothing
          _                         -> Just (List (min newcount oldcount) realFront realD)

          where
          dflt    = addNewInfo newD oldD
          updates = go new old

          realFront = zipWith fromMaybe (old ++ repeat realD) updates
          realD     = fromMaybe oldD dflt

          go ns os =
            case (ns,os) of
              (n : ns', o : os') -> addNewInfo n o : go ns' os'
              ([], _)            -> map (addNewInfo newD) os
              (_,[])             -> map (`addNewInfo` oldD) ns


instance Lattice FunPost where
  bottom = default_bottom
  addNewInfo = default_addNewInfo

--------------------------------------------------------------------------------
-- Instances for products.

class GLattice f where
  g_bottom     :: f a
  g_addNewInfo :: f a -> f a -> Maybe (f a)

instance GLattice U1 where
  g_bottom         = U1
  g_addNewInfo _ _ = Nothing

instance (GLattice f, GLattice g) => GLattice (f :*: g) where
  g_bottom = g_bottom :*: g_bottom
  g_addNewInfo (new1 :*: new2) (old1 :*: old2) =
    case (g_addNewInfo new1 old1, g_addNewInfo new2 old2) of
      (Nothing, Nothing)  -> Nothing
      (Nothing, Just x)   -> Just (old1 :*: x)
      (Just x,  Nothing)  -> Just (x    :*: old2)
      (Just x,  Just y)   -> Just (x    :*: y)

instance (GLattice f) => GLattice (M1 i c f) where
  g_bottom                       = M1 g_bottom
  g_addNewInfo (M1 new) (M1 old) = M1 <$> g_addNewInfo new old

instance Lattice a => GLattice (K1 i a) where
  g_bottom                       = K1 bottom
  g_addNewInfo (K1 new) (K1 old) = K1 <$> addNewInfo new old

default_bottom :: (Generic a, GLattice (Rep a)) => a
default_bottom = to g_bottom

default_addNewInfo :: (Generic a, GLattice (Rep a)) => a -> a -> Maybe a
default_addNewInfo x y = to <$> g_addNewInfo (from x) (from y)



instance (Lattice a, Lattice b) => Lattice (a,b) where
  bottom     = default_bottom
  addNewInfo = default_addNewInfo

instance Lattice Value where
  bottom      = default_bottom
  addNewInfo  = default_addNewInfo

instance Lattice TableV where
  bottom      = default_bottom
  addNewInfo  = default_addNewInfo


instance Lattice GlobalState where
  bottom      = default_bottom
  addNewInfo  = default_addNewInfo

instance Lattice LocalState where
  bottom      = default_bottom
  addNewInfo  = default_addNewInfo

instance Lattice State where
  bottom      = default_bottom
  addNewInfo  = default_addNewInfo

instance Lattice () where
  bottom = default_bottom
  addNewInfo = default_addNewInfo

--------------------------------------------------------------------------------





