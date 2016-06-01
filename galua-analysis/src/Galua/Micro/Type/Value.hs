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

-- | A field within a table.
data FieldName    = Metatable           -- ^ The meta-table for a table
                  | Field ByteString    -- ^ Normal field
                    deriving (Show,Eq,Ord)

-- | A block within a function
data QualifiedBlockName = QualifiedBlockName FunId BlockName
                    deriving (Show,Eq,Ord)

-- | A block name withing an instantiation of a function.
-- Functions are analyzed once per call site.
data GlobalBlockName = GlobalBlockName CallsiteId QualifiedBlockName
                        deriving (Eq,Ord,Show)

-- | An instruction, within a block, within a function.
-- Should point to a "call" instruction.
data CallsiteId = CallsiteId QualifiedBlockName Int
                    deriving (Show,Eq,Ord)

-- | A call-site "outside" the program.  It is used for the initial
-- call in the analysis.
initialCaller :: CallsiteId
initialCaller = CallsiteId (QualifiedBlockName noFun EntryBlock) 0

-- | An instruction within a specific instantiation of a function.
-- Should point to an allocation of a reference.
data RefId        = RefId GlobalBlockName Int
                    deriving (Show,Eq,Ord)

-- | An instruction within a specific instantiation of a function.
-- Should point to an allocation of a table.
data TableId      = TableId GlobalBlockName Int
                    deriving (Show,Eq,Ord)


--------------------------------------------------------------------------------
-- Lattices

-- | An abstract state of the interpreter at a sepcific program point.
-- This is the part of the state that is independent of
-- the function that is executing.
data GlobalState = GlobalState
  { tables      :: Map TableId TableV   -- ^ Info about tables
  , heap        :: Map RefId   Value    -- ^ Info about references

  , basicMetas  :: Type :-> Value       -- ^ Meta-tables for basic types
  , stringMeta  :: Value                -- ^ Meta-table for strings
  , funMeta     :: Value                -- ^ Meta-table for functions
  } deriving (Eq,Show,Generic)


-- | An abstract state describing the state of a function call.
data LocalState = LocalState
  { env         :: Map Reg Value        -- ^ Values of normal registers
  , argReg      :: List Value           -- ^ Argument register
  , listReg     :: List Value           -- ^ The "list" register.
                                        -- This is used when we make function
                                        -- calls or to return results.
  } deriving (Eq,Show,Generic)

-- | The current abstract state of the interpreter.
data State = State
  { globaleState :: GlobalState
  , localState   :: LocalState
  } deriving (Eq,Show,Generic)

-- | An abstract value. Describes what we know about a value.  Each
-- of the fields an additional pieces of information.  If a specific
-- field does not contain infomration relevant to the value, then it
-- will be set to @bottom@.
data Value = Value
  { valueBasic    :: Set Type                 -- ^ Possible basic types
  , valueString   :: Lift ByteString          -- ^ String aspects of the value
  , valueFunction :: WithTop (Set FunId)      -- ^ Which function is this
  , valueTable    :: WithTop (Set TableId)    -- ^ Which table is this
  , valueRef      :: WithTop (Set RefId)      -- ^ Which reference is this
  } deriving (Eq,Generic,Show)


-- | Information we keep about tables (i.e., "table types).
data TableV = TableV
  { tableFields  :: FieldName :-> Value
    -- ^ Types of specific fields.  Usefule for tables that are more like
    -- records or modules (i.e., they contain a fixed set fields, each
    -- of a potentially different type)

  , tableKeys    :: Value
    -- ^ A general description of the keys of the table.
    -- Used to type tables used as containers.

  , tableValues  :: Value
    -- ^ A general description of the values in the table.
    -- Used to type tables used as containers.
  } deriving (Generic,Show,Eq)


-- | A type for a function.  Similar to a pre- post-condition pair.
data FunBehavior = FunBehavior
  { funUpVals :: [Value]                -- ^ Types for up-values, if any
  , funArgs   :: List Value             -- ^ Types for arguments
  , funPost   :: [Value] -> List Value -> FunPost
    -- ^ Type of result, which may depend on the types of the upvalues
    -- and arguments.
  }

-- | Information about the return type of a function, and its side-effects.
data FunPost = FunPost
  { funReturns    :: List Value     -- ^ Values that are returns
  , funRaises     :: Value          -- ^ Exceptions that might be throws
  , funModTables  :: Set TableId    -- ^ Tables that might be modified
  , funModRefs    :: Set RefId      -- ^ Heap locations that might be modified
  } deriving (Eq,Show,Generic)


-- | A helper type to keep track of a speicific concrete values
data Lift a = NoValue               -- ^ Unused
            | OneValue a            -- ^ Always exactly this values
            | MultipleValues        -- ^ May be more than one value
              deriving (Eq,Show)

{- | A helper to add an artificail top (i.e., "I don't know") element to types.
This is useful for values where the universe is potentially very large
(e.g., table ids, function ids, reference id).  Instead of simply enumerating
all possible functions/references/tables, we use the `Top` element to
remember that we don't know the value. -}
data WithTop a = NotTop a | Top
                 deriving (Eq,Show)

--------------------------------------------------------------------------------


-- | Convert an abstract value, to all its possible individual cases.
-- This is handly to implement "pattern matching" on a value: we consider
-- the cases separately, and then union the results together.
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


-- | Convert a single value to an abstract value.
-- This is useful to make values of a specific shape (e.g. Nil)
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

-- | Make a value of this type.
basic :: Type -> Value
basic = fromSingleV . BasicValue

-- | Make a value that is some string.
anyString :: Value
anyString = fromSingleV (StringValue Nothing)

-- | Make a value that is exactly this reference.
newRef :: RefId -> Value
newRef = fromSingleV . RefValue . Just

-- | Make a value that is exactly this table.
newTable :: TableId -> Value
newTable = fromSingleV . TableValue . Just

-- | Make a value that is exactly this function.
newFun :: FunId -> Value
newFun = fromSingleV . FunctionValue . Just

-- | A value that is used but completely unknown.
topVal :: Value
topVal = Value { valueBasic    = Set.fromList [ minBound .. maxBound ]
               , valueString   = MultipleValues
               , valueFunction = Top
               , valueTable    = Top
               , valueRef      = Top
               }


-- | A value that describes the possible arguments to a function,
-- when we don't know anything else about it.  Basically we pass
-- `Top` for all arguments, but we do know that the inputs can't be references.
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


-- | A map with a default element.  Extends a finite map to a potentially
-- infinite domain, by using a fixed value everywhere where the
-- map is undefined.
-- This is useful for representing functions.
data a :-> b = FFun (Map a b) b
               deriving (Eq,Show)

-- | The constant funciont, maps everything to the given element.
fConst :: b -> (a :-> b)
fConst b = FFun Map.empty b

-- | Apply a function to a given elelement.
appFun :: Ord a => (a :-> b) -> a -> b
appFun (FFun mp b) a = Map.findWithDefault b a mp

-- | Apply a function to all its arguments at once.
-- The result is the union of the possible outputs.
appAll :: Lattice b => (a :-> b) -> b
appAll (FFun mp b) = foldr (\/) b (Map.elems mp)

-- | Overwrite the mapping for a key.
letFun :: Ord a => a -> b -> (a :-> b) -> (a :-> b)
letFun x y (FFun mp d) = FFun (Map.insert x y mp) d

-- | Specify that the given function might return `b`
letFunAll :: Lattice b => b -> (a :-> b) -> (a :-> b)
letFunAll y (FFun mp d) = FFun (fmap (y \/) mp) (y \/ d)

--------------------------------------------------------------------------------

-- | An infinite list.  This is similar to `Integer :-> a`
data List a = ListBottom
              -- ^ Having an explicit bottom helps avoid recursive values, when
              -- implementing a list of values.

            | List
                Int -- The list contains at least this many elements
                [a] a -- Neither of the 'a's should be bottom.
                      -- These are strict lists.
              deriving (Show,Eq)

-- | Drop some elements from the front of the list.
listDrop :: Int -> List a -> List a
listDrop n list =
  case list of
    ListBottom -> ListBottom
    List count xs d  -> List (max 0 (count-n)) (drop n xs) d

-- | A constant list, containing the same value in all positions.
listConst :: (Eq a, Lattice a) => a -> List a
listConst a
  | a == bottom = ListBottom
  | otherwise   = List 0 [] a

-- | Append some elements to the front of the list
listAppend :: (Eq a, Lattice a) => [a] -> List a -> List a
listAppend xs list
  | any (== bottom) xs  = ListBottom
  | otherwise =
     case list of
       ListBottom -> ListBottom
       -- this case is just an optimization to avoid rebuilding `xs`
       List count [] d  -> List (length xs + count) xs d
       List count as d  -> List (length xs + count) (xs ++ as) d

-- | "Apply" the list, (i.e., get the element at the given position.)
appList :: Lattice a => List a -> Int -> a
appList list x =
  case list of
    ListBottom  -> bottom
    List _ xs d -> case drop x xs of
                     v:_ -> v
                     _   -> d

-- | "Apply" a list to all possible indexes, (i.e., get the union of
-- all values stored in the list).
appListAll :: Lattice a => List a -> a
appListAll list =
  case list of
    ListBottom -> bottom
    List _ xs d  -> foldr (\/) d xs


-- | Lookup up a value in a map.
-- Returns `bottom` if the value is not present.
appFinMap :: (Ord a, Lattice b) => Map a b -> a -> b
appFinMap mp a = Map.findWithDefault bottom a mp



--------------------------------------------------------------------------------

class Lattice a where
  bottom     :: a
  addNewInfo :: a {-^new-} -> a {-^old-} -> Maybe a
  {- ^ Join two elements in the lattice.
    @addNewInfo a b == Nothing@ means that the answer is `b`
    This is useful when we are computing fix-points and we want to
    know if anything chaged. -}

-- | Symmetric join, when we don't care if the join changed anything.
(\/) :: Lattice a => a -> a -> a
(\/) x y = fromMaybe y (addNewInfo x y)

-- | Join together a whole bunch of things.
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
          _ -> Just (List (min newcount oldcount) realFront realD)

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





