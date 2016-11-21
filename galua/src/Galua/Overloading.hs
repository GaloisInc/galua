{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}

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

import           Control.Monad.IO.Class(MonadIO(liftIO))
import           Control.Monad.Trans.Reader(ReaderT(..), ask)
import qualified Data.ByteString as B
import           Data.IORef (readIORef)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Bits((.&.),(.|.),xor,complement)

import           Galua.Mach
import           Galua.Number
import           Galua.Reference
import           Galua.Value
import           Galua.LuaString



------------------------------------------------------------------------
-- Metamethod resolution
------------------------------------------------------------------------

class MonadIO m => MetatableMonad m where
  getTypeMetatables :: m (Map ValueType (Reference Table))

instance MetatableMonad Mach where
  getTypeMetatables = do ref <- getsMachEnv machMetatablesRef
                         liftIO (readIORef ref)

instance MonadIO m => MetatableMonad (WithMetatables m) where
  getTypeMetatables = WithMetatables ask

newtype WithMetatables m a = WithMetatables
  (ReaderT (Map ValueType (Reference Table)) m a)
  deriving (Functor, Applicative, Monad, MonadIO)

withMetatables ::
  Map ValueType (Reference Table) ->
  WithMetatables m a -> m a
withMetatables x (WithMetatables (ReaderT f)) = f x

getTypeMetatable ::
  MetatableMonad m =>
  ValueType -> m (Maybe (Reference Table))
getTypeMetatable typ =
  do metatables <- getTypeMetatables
     return (Map.lookup typ metatables)


-- | Look up the metatable for a value
valueMetatable :: MetatableMonad m => Value -> m (Maybe (Reference Table))
valueMetatable v =
  case v of
    Table    t -> liftIO (getTableMeta t)
    UserData u -> liftIO (userDataMeta <$> readRef u)
    _          -> getTypeMetatable (valueType v)

-- | Set the metatable for a value. If the value is not a userdata
-- or table then the metatable is set for the whole type.
setMetatable :: Maybe (Reference Table) -> Value -> Mach ()
setMetatable mt v =
  case v of
    Table    tref -> setTableMeta tref mt
    UserData uref -> modifyRef uref $ \u -> u{userDataMeta = mt}
    _             -> setTypeMetatable (valueType v) mt

-- | Look up the metamethod for a value
valueMetamethod ::
  MetatableMonad m =>
  Value ->
  LuaString {- ^ metamethod name -} ->
  m Value
valueMetamethod v event =
  do mbMetatable <- valueMetatable v
     liftIO $ case mbMetatable of
       Nothing -> return Nil
       Just metatable -> getTableRaw metatable (String event)

-- | Look up a metamethod on the first value if it is set falling back to
-- the second value.
valueMetamethod2 ::
  MetatableMonad m =>
  Value ->
  Value ->
  LuaString {- ^ metamethod name -} ->
  m Value
valueMetamethod2 x y event =
  do m1 <- valueMetamethod x event
     case m1 of
       Nil -> valueMetamethod y event
       _   -> return m1

--------------------------------------------------------------------------------
-- GC


m__gc :: Value -> Mach ()
m__gc val =
  do metamethod <- valueMetamethod val str__gc
     case metamethod of
        Nil -> return ()
        _   -> () <$ m__call metamethod [val]




--------------------------------------------------------------------------------
-- Function overloading


-- | Figure out what function we are actually calling (and with what arguments)
-- when calling a value.
resolveFunction ::
  Value   {- ^ function-like value -} ->
  [Value] {- ^ arguments -} ->
  Mach (Reference Closure, [Value]) -- ^ actual function, and arugments
resolveFunction (Closure c) args = return (c, args)
resolveFunction x args =
  do metamethod <- valueMetamethod x str__call
     case metamethod of
       Nil -> luaError ("attempt to call a " ++
                                  prettyValueType (valueType x) ++ " value")
       _   -> resolveFunction metamethod (x:args)



-- | Compute the result of applying a function to its arguments.
-- This is more general than 'runClosure' because it can use
-- metamethods to resolve the call on non-closures.
m__call ::
  Value     {- ^ function -} ->
  [Value]   {- ^ arguments -} ->
  Mach [Value] {- ^ results -}
m__call x args =
  do (f,newArgs) <- resolveFunction x args
     machCall f newArgs


--------------------------------------------------------------------------------
-- Table overloading

badIndex :: Value -> Mach a
badIndex x =
  luaError ("attempt to index a " ++ prettyValueType (valueType x) ++ " value")



-- | Store a value at a particular index in another value. This operation
-- can be overloaded for non-tables via the `__newindex` metamethod.
--
-- @
-- table[key]=value
-- @
m__newindex ::
  Value {- ^ table -} ->
  Value {- ^ key   -} ->
  Value {- ^ value -} ->
  Mach ()
m__newindex t k v =
  case t of
    Table table ->
        do let next = keyCheck k >> setTableRaw table k v
           oldv <- getTableRaw table k
           case oldv of
             Nil -> metaCase next
             _ -> next

    _ -> metaCase (badIndex t)
  where
  metaCase onFailure =
    do metamethod <- valueMetamethod t str__newindex
       case metamethod of
         Closure c -> () <$ machCall c [t,k,v]
         Nil       -> onFailure
         _         -> m__newindex metamethod k v

keyCheck :: Value -> Mach ()
keyCheck (Number (Double nan)) | isNaN nan = luaError "table index is NaN"
keyCheck Nil = luaError "table index is nil"
keyCheck _ = return ()


-- | Index a value by another value. This operation can be overloaded
-- for non-table values via the `__index` metamethod.
--
-- @
-- table[key]
-- @
m__index :: Value {- ^ table -} -> Value {- ^ key -} -> Mach Value
m__index m key =
  case m of
    Table table ->
         do v <- getTableRaw table key
            case v of
              Nil -> metaCase (return Nil)
              _   -> return v

    _ -> metaCase (badIndex m)
  where
  metaCase onFailure =
    do metamethod <- valueMetamethod m str__index
       case metamethod of
         Closure c -> trimResult1 <$> machCall c [m,key]
         Nil       -> onFailure
         _         -> m__index metamethod key



-- | The the indexing meta-method.  This is a separate function,
-- because when we resolve names, we resolve the index lookup slightly
-- differently (no function calls)
get_m__index :: Map ValueType (Reference Table) -> Value -> IO Value
get_m__index tabs m = withMetatables tabs (valueMetamethod m str__index)



--------------------------------------------------------------------------------
-- Relational overloading

badCompare :: Value -> Value -> Mach a
badCompare x y =
  let p = prettyValueType . valueType in
  luaError ("attempt to compare " ++ p x ++ " with " ++ p y)


-- | Return 'True' when two values are "equal". This operation
-- can be overloaded using the `__eq` metamethod.
m__eq :: Value -> Value -> Mach Bool
m__eq x y =
  case (x, y) of
    _ | x == y               -> return True
    (Table   {}, Table   {}) -> metaEq
    (UserData{}, UserData{}) -> metaEq
    _                        -> return False

  where
  metaEq =
    do metamethod <- valueMetamethod2 x y str__eq
       case metamethod of
         Nil -> return False
         _   ->
            do rs <- m__call metamethod [x,y]
               case rs of
                 [] -> return False
                 r:_ -> return (valueBool r)

-- | Return 'True' the first value is "less-than" than the second.
-- This operation can be overloaded using the `__lt` metamethod.
m__lt :: Value -> Value -> Mach Bool
m__lt (Number x) (Number y) = return (x < y)
m__lt (String x) (String y) = return (x < y)
m__lt x y =
  do metamethod <- valueMetamethod2 x y str__lt
     case metamethod of
       Nil -> badCompare x y
       _   ->
         do rs <- m__call metamethod [x,y]
            case rs of
              [] -> return False
              r:_ -> return (valueBool r)

-- | Return 'True' the first value is "less-then-or-equal-to" than the second.
-- This operation can be overloaded using the `__le` metamethod.
m__le :: Value -> Value -> Mach Bool
m__le (Number x) (Number y) = return (x <= y)
m__le (String x) (String y) = return (x <= y)
m__le x y =
  do metamethod <- valueMetamethod2 x y str__le
     case metamethod of
       Nil -> not <$> m__lt y x
       _   ->
         do rs <- m__call metamethod [x,y]
            case rs of
              [] -> return False
              r:_ -> return (valueBool r)



--------------------------------------------------------------------------------
-- Arithmetic/Logic operators

badInt :: Mach a
badInt = luaError "number has no integer representation"

badArithmetic :: Value -> Mach a
badArithmetic x = luaError ("attempt to perform arithmetic on a " ++
                                    prettyValueType (valueType x) ++ " value")


valueArith1 :: LuaString -> (Number -> Number) -> Value -> Mach Value
valueArith1 event f x =
  case valueNumber x of
    Just n  -> return (Number (f n))
    Nothing -> do metamethod <- valueMetamethod x event
                  case metamethod of
                    Nil -> badArithmetic x
                    _   -> trimResult1 <$> m__call metamethod [x]


valueInt1 :: LuaString -> (Int -> Int) -> Value -> Mach Value
valueInt1 event f x1 =
  case valueInt x1 of
    Just i  -> return (Number (Int (f i)))
    Nothing -> do metamethod <- valueMetamethod x1 event
                  case metamethod of
                     Nil -> badInt
                     _   -> trimResult1 <$> m__call metamethod [x1]


{-# INLINE valueArith2 #-}
valueArith2 ::
  LuaString -> (Number -> Number -> Number) -> Value -> Value -> Mach Value
valueArith2 event f x y =
  case valueNumber x of
    Nothing -> overload x
    Just l ->
      case valueNumber y of
        Nothing -> overload y
        Just r -> return $! Number (f l r)

  where
  overload bad =
    do metamethod <- valueMetamethod2 x y event
       case metamethod of
         Nil -> badArithmetic bad
         _   -> trimResult1 <$> m__call metamethod [x,y]


{-# INLINE valueInt2 #-}
valueInt2 ::
  LuaString -> (Int -> Int -> Int) -> Value -> Value -> Mach Value
valueInt2 event f x1 x2 =
  case valueInt x1 of
    Nothing -> overload
    Just l ->
      case valueInt x2 of
        Just r -> return $! Number (Int (f l r))
        Nothing -> overload
  where
  overload =
    do metamethod <- valueMetamethod2 x1 x2 event
       case metamethod of
         Nil -> badInt
         _   -> trimResult1 <$> m__call metamethod [x1,x2]




m__add :: Value -> Value -> Mach Value
m__add x y = valueArith2 str__add (+) x y

m__sub :: Value -> Value -> Mach Value
m__sub x y = valueArith2 str__sub (-) x y

m__mul :: Value -> Value -> Mach Value
m__mul x y = valueArith2 str__mul (*) x y

m__div :: Value -> Value -> Mach Value
m__div x y = valueArith2 str__div numberDiv x y

m__idiv :: Value -> Value -> Mach Value
m__idiv x y =
  case (x,y) of
    (Number (Int _), Number (Int 0)) -> luaError "atempt to divide by zero"
    _ -> valueArith2 str__idiv numberIDiv x y

m__mod :: Value -> Value -> Mach Value
m__mod x y =
  case (x,y) of
    (Number (Int _), Number (Int 0)) -> luaError "atempt to perform 'n%0'"
    _ -> valueArith2 str__mod numberMod x y

m__pow :: Value -> Value -> Mach Value
m__pow x y = valueArith2 str__pow numberPow x y

m__unm :: Value -> Mach Value
m__unm x = valueArith1 str__unm negate x

m__band :: Value -> Value -> Mach Value
m__band x y = valueInt2 str__band (.&.) x y

m__bor :: Value -> Value -> Mach Value
m__bor x y = valueInt2 str__bor (.|.) x y

m__bxor :: Value -> Value -> Mach Value
m__bxor x y = valueInt2 str__bxor xor x y

m__shl :: Value -> Value -> Mach Value
m__shl x y = valueInt2 str__shl wordshiftL x y

m__shr :: Value -> Value -> Mach Value
m__shr x y = valueInt2 str__shr wordshiftR x y

m__bnot :: Value -> Mach Value
m__bnot x = valueInt1 str__bnot complement x



















--------------------------------------------------------------------------------
-- Length of tables and strings

badLength :: Value -> Mach a
badLength x = luaError ("attempt to get length of a " ++
                                    prettyValueType (valueType x) ++ " value")

m__len :: Value -> Mach Value
m__len v =
  case v of
    String x -> return (Number (fromIntegral (B.length (toByteString x))))
    _ -> do metamethod <- valueMetamethod v str__len
            case (metamethod,v) of
              (Nil,Table t) -> Number . Int <$> liftIO (tableLen t)
              (Nil,_      ) -> badLength v
              _             -> trimResult1 <$> m__call metamethod [v]



m__concat :: [Value] -> Mach Value
m__concat = concat2 . reverse

-- Take the arguments in REVERSE order
concat2 :: [Value] -> Mach Value
concat2 [] = liftIO (String <$> fromByteString "")
concat2 [x] = return x
concat2 (x:y:z) =
  case (valueString x, valueString y) of

    -- If there are at least two convertable strings on the head
    -- of the list we collect all of the convertable strings
    -- and concatenate them into one string.
    (Just x', Just y') ->
         let (strs,z') = takeMaybe valueString z
         in do s <- liftIO $ fromByteString $ B.concat $ reverse $ x':y':strs
               concat2 (String s:z')

    -- Otherwise we use the __concat metamethod to collapse the
    -- top two arguments of the stack and try again.
    _ -> do metamethod <- valueMetamethod2 y x str__concat
            case metamethod of
              Nil -> badConcat y x
              _   ->
                do r <- trimResult1 <$> m__call metamethod [y,x]
                   concat2 (r:z)            -- the list of arguments is reverse
                                            -- so we pass y before x here

badConcat :: Value -> Value -> Mach a
badConcat x y =
   luaError ("attempt to concatenate a " ++ prettyValueType (valueType x) ++
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
  , str__bnot :: LuaString

str__call     = unsafeFromByteString "__call"
str__gc       = unsafeFromByteString "__gc"
str__len      = unsafeFromByteString "__len"
str__index    = unsafeFromByteString "__index"
str__newindex = unsafeFromByteString "__newindex"
str__concat   = unsafeFromByteString "__concat"
str__eq       = unsafeFromByteString "__eq"
str__lt       = unsafeFromByteString "__lt"
str__le       = unsafeFromByteString "__le"
str__add      = unsafeFromByteString "__add"
str__sub      = unsafeFromByteString "__sub"
str__mul      = unsafeFromByteString "__mul"
str__div      = unsafeFromByteString "__div"
str__idiv     = unsafeFromByteString "__idiv"
str__mod      = unsafeFromByteString "__mod"
str__pow      = unsafeFromByteString "__pow"
str__unm      = unsafeFromByteString "__unm"
str__band     = unsafeFromByteString "__band"
str__bor      = unsafeFromByteString "__bor"
str__bxor     = unsafeFromByteString "__bxor"
str__shl      = unsafeFromByteString "__shl"
str__shr      = unsafeFromByteString "__shr"
str__bnot     = unsafeFromByteString "__bnot"




