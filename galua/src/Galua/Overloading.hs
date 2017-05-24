{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Galua.Overloading
  ( -- * Overloaded methods
    m__call
  , m__gc

  , m__len
  , m__index
  , m__newindex

  , m__concat

  , m__eq
  , m__lt
  , m__le

  , m__add
  , m__sub
  , m__mul
  , m__div
  , m__idiv
  , m__mod
  , m__pow
  , m__unm
  , m__band
  , m__bor
  , m__bxor
  , m__shl
  , m__shr
  , m__bnot

   -- * Misc
  , resolveFunction
  , get_m__index
  , valueMetatable
  , setMetatable

  ) where

import qualified Data.ByteString as B
import           Data.IORef (IORef,readIORef, writeIORef, modifyIORef')
import qualified Data.Map as Map
import           Data.Bits((.&.),(.|.),xor,complement)
import           Data.Vector ( Vector )
import qualified Data.Vector as Vector

import           Galua.Mach
import           Galua.Number
import           Galua.Value
import           Galua.LuaString
import           Galua.Util.Table (tableGetMeta)


type Cont a = a -> IO NextStep
type Meth1  = MetaTabRef -> Cont Value -> Value -> IO NextStep
type Meth2  = MetaTabRef -> Cont Value -> Value -> Value -> IO NextStep
type Rel    = MetaTabRef -> Cont Bool  -> Value -> Value -> IO NextStep



------------------------------------------------------------------------
-- Metamethod resolution
------------------------------------------------------------------------
class Metas t where
  metas :: t -> IO TypeMetatables

instance Metas TypeMetatables where
  metas = return
  {-# INLINE metas #-}


type MetaTabRef = IORef TypeMetatables

instance Metas MetaTabRef where
  metas = readIORef
  {-# INLINE metas #-}


-- | Get the metatebla for a particular type, using the given source.
getTypeMetatable :: Metas t => t -> ValueType -> IO (Maybe (Reference Table))
getTypeMetatable tabs typ =
  do metatables <- metas tabs
     return $! Map.lookup typ metatables


-- | Look up the metatable for a value
valueMetatable :: Metas t => t -> Value -> IO (Maybe (Reference Table))
valueMetatable tabs v =
  case v of
    Table    t -> getTableMeta t
    UserData u -> readIORef (userDataMeta (referenceVal u))
    _          -> getTypeMetatable tabs (valueType v)

setTypeMetatable ::
  IORef TypeMetatables -> ValueType -> Maybe (Reference Table) -> IO ()
setTypeMetatable tabs typ mb =
  modifyIORef' tabs $
    case mb of
      Just metatable -> Map.insert typ metatable
      Nothing        -> Map.delete typ

-- | Set the metatable for a value. If the value is not a userdata
-- or table then the metatable is set for the whole type.
setMetatable ::
  IORef TypeMetatables -> Maybe (Reference Table) -> Value -> IO ()
setMetatable tabs mt v =
  case v of
    Table    tref -> setTableMeta tref mt
    UserData uref -> writeIORef (userDataMeta (referenceVal uref)) mt
    _             -> setTypeMetatable tabs (valueType v) mt



-- | Look up the metamethod for a value
valueMetamethod ::
  Metas t =>
  t ->
  Value ->
  MetaMethodName {- ^ metamethod name -} ->
  IO Value
valueMetamethod tabs v (MMN event) =
  do mbMetatable <- valueMetatable tabs v
     case mbMetatable of
       Nothing        -> return Nil
       Just metatable ->
         tableGetMeta (referenceVal metatable) event allMetaMethods

-- | Look up a metamethod on the first value if it is set falling back to
-- the second value.
valueMetamethod2 ::
  IORef TypeMetatables ->
  Value ->
  Value ->
  MetaMethodName {- ^ metamethod name -} ->
  IO Value
valueMetamethod2 tabs x y event =
  do m1 <- valueMetamethod tabs x event
     case m1 of
       Nil -> valueMetamethod tabs y event
       _   -> return m1



--------------------------------------------------------------------------------
-- GC


m__gc :: MetaTabRef -> IO NextStep -> Value -> IO NextStep
m__gc tabs next val =
  do metamethod <- valueMetamethod tabs val str__gc
     case metamethod of
        Nil -> next
        _   -> m__call tabs (const next) metamethod (vec1 val)




--------------------------------------------------------------------------------
-- Function overloading


-- | Figure out what function we are actually calling (and with what arguments)
-- when calling a value.
resolveFunction ::
  MetaTabRef ->
  Cont (Reference Closure, Vector Value) {- ^ function and arugments -} ->
  Value   {- ^ function-like value -} ->
  Vector Value {- ^ arguments -} ->
  IO NextStep
resolveFunction _ next (Closure c) args = next (c, args)
resolveFunction tabs next x args =
  do metamethod <- valueMetamethod tabs x str__call
     case metamethod of
       Nil -> luaError' ("attempt to call a " ++
                                  prettyValueType (valueType x) ++ " value")
       _   -> resolveFunction tabs next metamethod (Vector.cons x args)



-- | Compute the result of applying a function to its arguments.
-- This is more general than 'runClosure' because it can use
-- metamethods to resolve the call on non-closures.
m__call ::
  MetaTabRef ->
  Cont (Vector Value) ->
  Value          {- ^ function -} ->
  Vector Value   {- ^ arguments -} ->
  IO NextStep
m__call tabs cont x args = resolveFunction tabs after x args
  where
  after (f,vs) = return (FunCall f vs Nothing cont)
{-# INLINE m__call #-}


--------------------------------------------------------------------------------
-- Table overloading

badIndex :: Value -> IO NextStep
badIndex x =
  luaError' ("attempt to index a " ++ prettyValueType (valueType x) ++ " value")



-- | Store a value at a particular index in another value. This operation
-- can be overloaded for non-tables via the `__newindex` metamethod.
--
-- @
-- table[key]=value
-- @
m__newindex ::
  MetaTabRef ->
  IO NextStep ->
  Value {- ^ table -} ->
  Value {- ^ key   -} ->
  Value {- ^ value -} ->
  IO NextStep
m__newindex tabs cont t k v =
  case t of
    Table table ->
        do let next = keyCheck k (setTableRaw table k v >> cont)
           oldv <- getTableRaw table k
           case oldv of
             Nil -> metaCase next
             _   -> next

    _ -> metaCase (badIndex t)
  where
  metaCase onFailure =
    do metamethod <- valueMetamethod tabs t str__newindex
       case metamethod of
         Closure c -> return (FunCall c (vec3 t k v) Nothing (const cont))
         Nil       -> onFailure
         _         -> m__newindex tabs cont metamethod k v

keyCheck :: Value -> IO NextStep -> IO NextStep
keyCheck (Number (Double nan)) _ | isNaN nan = luaError' "table index is NaN"
keyCheck Nil _ = luaError' "table index is nil"
keyCheck _ k = k


-- | Index a value by another value. This operation can be overloaded
-- for non-table values via the `__index` metamethod.
--
-- @
-- table[key]
-- @
m__index :: MetaTabRef -> Cont Value ->
              Value {- ^ table -} -> Value {- ^ key -} -> IO NextStep
m__index tabs cont m key =
  case m of
    Table table ->
         do v <- getTableRaw table key
            case v of
              Nil -> metaCase (cont Nil)
              _   -> cont v

    _ -> metaCase (badIndex m)
  where
  metaCase onFailure =
    do metamethod <- valueMetamethod tabs m str__index
       case metamethod of
         Closure c -> return (FunCall c (vec2 m key) Nothing
                                        (cont . trimResult1))
         Nil       -> onFailure
         _         -> m__index tabs cont metamethod key


vec1 :: a -> Vector a
vec1 = Vector.singleton

vec2 :: a -> a -> Vector a
vec2 a b = Vector.fromListN 2 [a,b]

vec3 :: a -> a -> a -> Vector a
vec3 a b c = Vector.fromListN 3 [a,b,c]




-- | The the indexing meta-method.  This is a separate function,
-- because when we resolve names, we resolve the index lookup slightly
-- differently (no function calls)
get_m__index :: TypeMetatables -> Value -> IO Value
get_m__index tabs m = valueMetamethod tabs m str__index



--------------------------------------------------------------------------------
-- Relational overloading

badCompare :: Value -> Value -> IO NextStep
badCompare x y =
  let p = prettyValueType . valueType in
  luaError' ("attempt to compare " ++ p x ++ " with " ++ p y)


-- | Return 'True' when two values are "equal". This operation
-- can be overloaded using the `__eq` metamethod.
m__eq :: Rel
m__eq tabs cont x y =
  case (x, y) of
    _ | x == y               -> cont True
    (Table   {}, Table   {}) -> metaEq
    (UserData{}, UserData{}) -> metaEq
    _                        -> cont False

  where
  metaEq =
    do metamethod <- valueMetamethod2 tabs x y str__eq
       case metamethod of
         Nil -> cont False
         _   -> m__call tabs (cont . boolRes) metamethod (vec2 x y)

-- | Return 'True' the first value is "less-than" than the second.
-- This operation can be overloaded using the `__lt` metamethod.
m__lt :: Rel
m__lt _ cont (Number x) (Number y) = cont $! x < y
m__lt _ cont (String x) (String y) = cont $! x < y
m__lt tabs cont x y =
  do metamethod <- valueMetamethod2 tabs x y str__lt
     case metamethod of
       Nil -> badCompare x y
       _   -> m__call tabs (cont . boolRes) metamethod (vec2 x y)

-- | Return 'True' the first value is "less-then-or-equal-to" than the second.
-- This operation can be overloaded using the `__le` metamethod.
m__le :: Rel
m__le _ cont (Number x) (Number y) = cont $! x <= y
m__le _ cont (String x) (String y) = cont $! x <= y
m__le tabs cont x y =
  do metamethod <- valueMetamethod2 tabs x y str__le
     case metamethod of
       Nil -> m__lt tabs (cont . not) y x
       _   -> m__call tabs (cont . boolRes) metamethod (vec2 x y)

boolRes :: Vector Value -> Bool
boolRes = valueBool . trimResult1


--------------------------------------------------------------------------------
-- Arithmetic/Logic operators

badInt :: IO NextStep
badInt = luaError' "number has no integer representation"

badArithmetic :: Value -> IO NextStep
badArithmetic x = luaError' ("attempt to perform arithmetic on a " ++
                                    prettyValueType (valueType x) ++ " value")


{-# INLINE valueArith1 #-}
valueArith1 ::
  MetaMethodName -> (Number -> Number) ->
  MetaTabRef -> Cont Value ->
  Value -> IO NextStep
valueArith1 event f = \tabs cont x ->
  case valueNumber x of
    Just n  -> cont $! Number (f n)
    Nothing -> do metamethod <- valueMetamethod tabs x event
                  case metamethod of
                    Nil -> badArithmetic x
                    _   -> m__call tabs (cont . trimResult1) metamethod (vec1 x)


{-# INLINE valueInt1 #-}
valueInt1 ::
  MetaMethodName -> (Int -> Int) ->
  MetaTabRef -> Cont Value ->
  Value -> IO NextStep
valueInt1 event f = \tabs cont x1 ->
  case valueInt x1 of
    Just i  -> cont $! Number (Int (f i))
    Nothing ->
      do metamethod <- valueMetamethod tabs x1 event
         case metamethod of
           Nil -> badInt
           _   -> m__call tabs (cont . trimResult1) metamethod (vec1 x1)


{-# INLINE valueArith2 #-}
valueArith2 ::
  MetaMethodName -> (Number -> Number -> Number) ->
  MetaTabRef -> Cont Value ->
  Value -> Value -> IO NextStep
valueArith2 event f = \tabs cont x y ->
  let overload bad =
        do metamethod <- valueMetamethod2 tabs x y event
           case metamethod of
             Nil -> badArithmetic bad
             _   -> m__call tabs (cont . trimResult1) metamethod (vec2 x y)
  in case valueNumber x of
       Nothing -> overload x
       Just l ->
         case valueNumber y of
           Nothing -> overload y
           Just r  -> cont $! Number (f l r)

{-# INLINE valueInt2 #-}
valueInt2 ::
  MetaMethodName -> (Int -> Int -> Int) ->
  MetaTabRef -> Cont Value ->
  Value -> Value -> IO NextStep
valueInt2 event f = \tabs cont x1 x2 ->
  let overload =
        do metamethod <- valueMetamethod2 tabs x1 x2 event
           case metamethod of
             Nil -> badInt
             _   -> m__call tabs (cont . trimResult1) metamethod (vec2 x1 x2)

  in case valueInt x1 of
       Nothing -> overload
       Just l ->
         case valueInt x2 of
           Just r  -> cont $! Number (Int (f l r))
           Nothing -> overload



m__add :: Meth2
m__add = valueArith2 str__add (+)

m__sub :: Meth2
m__sub = valueArith2 str__sub (-)

m__mul :: Meth2
m__mul = valueArith2 str__mul (*)

m__div :: Meth2
m__div = valueArith2 str__div numberDiv

m__idiv :: Meth2
m__idiv tabs cont x y =
  case (x,y) of
    (Number (Int _), Number (Int 0)) -> luaError' "atempt to divide by zero"
    _ -> valueArith2 str__idiv numberIDiv tabs cont x y

m__mod :: Meth2
m__mod tabs cont x y =
  case (x,y) of
    (Number (Int _), Number (Int 0)) -> luaError' "atempt to perform 'n%0'"
    _ -> valueArith2 str__mod numberMod tabs cont x y

m__pow :: Meth2
m__pow = valueArith2 str__pow numberPow

m__unm :: Meth1
m__unm = valueArith1 str__unm negate

m__band :: Meth2
m__band = valueInt2 str__band (.&.)

m__bor :: Meth2
m__bor = valueInt2 str__bor (.|.)

m__bxor :: Meth2
m__bxor = valueInt2 str__bxor xor

m__shl :: Meth2
m__shl = valueInt2 str__shl wordshiftL

m__shr :: Meth2
m__shr = valueInt2 str__shr wordshiftR

m__bnot :: Meth1
m__bnot = valueInt1 str__bnot complement



--------------------------------------------------------------------------------
-- Length of tables and strings

badLength :: Value -> IO NextStep
badLength x = luaError' ("attempt to get length of a " ++
                                    prettyValueType (valueType x) ++ " value")

m__len :: Meth1
m__len tabs cont v =
  case v of
    String x -> cont $! Number (fromIntegral (B.length (toByteString x)))
    _ -> do metamethod <- valueMetamethod tabs v str__len
            case (metamethod,v) of
              (Nil,Table t) -> cont . Number . Int =<< tableLen t
              (Nil,_      ) -> badLength v
              _ -> m__call tabs (cont . trimResult1) metamethod (vec1 v)



m__concat :: MetaTabRef -> Cont Value -> Vector Value -> IO NextStep
m__concat tabs cont = concat2 tabs cont . reverse . Vector.toList

-- Take the arguments in REVERSE order
concat2 :: MetaTabRef -> Cont Value -> [Value] -> IO NextStep
concat2 _ cont [] = cont . String =<< fromByteString ""
concat2 _ cont [x] = cont x
concat2 tabs cont (x:y:z) =
  case (valueString x, valueString y) of

    -- If there are at least two convertable strings on the head
    -- of the list we collect all of the convertable strings
    -- and concatenate them into one string.
    (Just x', Just y') ->
         let (strs,z') = takeMaybe valueString z
         in do s <- fromByteString $ B.concat $ reverse $ x':y':strs
               concat2 tabs cont (String s:z')

    -- Otherwise we use the __concat metamethod to collapse the
    -- top two arguments of the stack and try again.
    _ -> do metamethod <- valueMetamethod2 tabs y x str__concat
            case metamethod of
              Nil -> badConcat y x
              _   -> m__call tabs after metamethod (vec2 y x)
                where after rs = concat2 tabs cont (trimResult1 rs : z)
                      -- the list of arguments is reverse
                      -- so we pass y before x here

badConcat :: Value -> Value -> IO NextStep
badConcat x y =
   luaError' ("attempt to concatenate a " ++ prettyValueType (valueType x) ++
              " with a "                  ++ prettyValueType (valueType y))

-- | Take elements from the front of a list as long
-- as the applied function continues to produce 'Just' values.
takeMaybe :: (a -> Maybe b) -> [a] -> ([b],[a])
takeMaybe _ [    ] = ([],[])
takeMaybe f (x:xs) =
  case f x of
    Nothing -> ([],x:xs)
    Just y  -> let (ys,xs') = takeMaybe f xs
               in (y:ys,xs')




--------------------------------------------------------------------------------

newtype MetaMethodName = MMN Int

str__call
  , str__gc
  , str__len
  , str__index
  , str__newindex
  , str__concat
  , str__eq
  , str__lt
  , str__le
  , str__add
  , str__sub
  , str__mul
  , str__div
  , str__idiv
  , str__mod
  , str__pow
  , str__unm
  , str__band
  , str__bor
  , str__bxor
  , str__shl
  , str__shr
  , str__bnot :: MetaMethodName

str__add      = MMN 0
str__band     = MMN 1
str__bnot     = MMN 2
str__bor      = MMN 3
str__bxor     = MMN 4

str__call     = MMN 5
str__concat   = MMN 6
str__div      = MMN 7
str__eq       = MMN 8
str__gc       = MMN 9

str__idiv     = MMN 10
str__index    = MMN 11
str__le       = MMN 12
str__len      = MMN 13
str__lt       = MMN 14

str__mod      = MMN 15
str__mul      = MMN 16
str__newindex = MMN 17
str__pow      = MMN 18
str__shl      = MMN 19

str__shr      = MMN 20
str__sub      = MMN 21
str__unm      = MMN 22

allMetaMethods :: [Value]
allMetaMethods = map (String . unsafeFromByteString)
  ["__add" , "__band"  , "__bnot"    , "__bor", "__bxor",
   "__call", "__concat", "__div"     , "__eq" , "__gc"  ,
   "__idiv", "__index" , "__le"      , "__len", "__lt"  ,
   "__mod" , "__mul"   , "__newindex", "__pow", "__shl" ,
   "__shr" , "__sub"   , "__unm"]
