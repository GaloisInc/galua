{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}

module Galua.Overloading where

import           Control.Monad.IO.Class(MonadIO(liftIO))
import           Control.Monad.Trans.Reader(ReaderT(..), ask)
import qualified Data.ByteString as B
import           Data.String(fromString)
import           Data.IORef (readIORef)
import           Data.Map (Map)
import qualified Data.Map as Map

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
  String {- ^ metamethod name -} ->
  m Value
valueMetamethod v event =
  do mbMetatable <- valueMetatable v
     liftIO $ case mbMetatable of
       Nothing -> return Nil
       Just metatable ->
         do keyStr <- fromByteString (fromString event)
            getTableRaw metatable (String keyStr)

-- | Look up a metamethod on the first value if it is set falling back to
-- the second value.
valueMetamethod2 ::
  MetatableMonad m =>
  Value ->
  Value ->
  String {- ^ metamethod name -} ->
  m Value
valueMetamethod2 x y event =
  do m1 <- valueMetamethod x event
     case m1 of
       Nil -> valueMetamethod y event
       _   -> return m1




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
  do metamethod <- valueMetamethod x "__call"
     case metamethod of
       Nil -> luaError ("attempt to call a " ++
                                  prettyValueType (valueType x) ++ " value")
       _   -> resolveFunction metamethod (x:args)



-- | Compute the result of applying a function to its arguments.
-- This is more general than 'runClosure' because it can use
-- metamethods to resolve the call on non-closures.
callValue ::
  Value     {- ^ function -} ->
  [Value]   {- ^ arguments -} ->
  Mach [Value] {- ^ results -}
callValue x args =
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
setTable ::
  Value {- ^ table -} ->
  Value {- ^ key   -} ->
  Value {- ^ value -} ->
  Mach ()
setTable t k v =
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
    do metamethod <- valueMetamethod t "__newindex"
       case metamethod of
         Closure c -> () <$ machCall c [t,k,v]
         Nil       -> onFailure
         _         -> setTable metamethod k v

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
indexValue :: Value {- ^ table -} -> Value {- ^ key -} -> Mach Value
indexValue m key =
  case m of
    Table table ->
         do v <- getTableRaw table key
            case v of
              Nil -> metaCase (return Nil)
              _   -> return v

    _ -> metaCase (badIndex m)
  where
  metaCase onFailure =
    do metamethod <- valueMetamethod m "__index"
       case metamethod of
         Closure c -> trimResult1 <$> machCall c [m,key]
         Nil       -> onFailure
         _         -> indexValue metamethod key






--------------------------------------------------------------------------------
-- Relational overloading

badCompare :: Value -> Value -> Mach a
badCompare x y =
  let p = prettyValueType . valueType in
  luaError ("attempt to compare " ++ p x ++ " with " ++ p y)


-- | Return 'True' when two values are "equal". This operation
-- can be overloaded using the `__eq` metamethod.
valueEqual :: Value -> Value -> Mach Bool
valueEqual x y =
  case (x, y) of
    _ | x == y               -> return True
    (Table   {}, Table   {}) -> metaEq
    (UserData{}, UserData{}) -> metaEq
    _                        -> return False

  where
  metaEq =
    do metamethod <- valueMetamethod2 x y "__eq"
       case metamethod of
         Nil -> return False
         _   ->
            do rs <- callValue metamethod [x,y]
               case rs of
                 [] -> return False
                 r:_ -> return (valueBool r)

-- | Return 'True' the first value is "less-than" than the second.
-- This operation can be overloaded using the `__lt` metamethod.
valueLess :: Value -> Value -> Mach Bool
valueLess (Number x) (Number y) = return (x < y)
valueLess (String x) (String y) = return (x < y)
valueLess x y =
  do metamethod <- valueMetamethod2 x y "__lt"
     case metamethod of
       Nil -> badCompare x y
       _   ->
         do rs <- callValue metamethod [x,y]
            case rs of
              [] -> return False
              r:_ -> return (valueBool r)

-- | Return 'True' the first value is "less-then-or-equal-to" than the second.
-- This operation can be overloaded using the `__le` metamethod.
valueLessEqual :: Value -> Value -> Mach Bool
valueLessEqual (Number x) (Number y) = return (x <= y)
valueLessEqual (String x) (String y) = return (x <= y)
valueLessEqual x y =
  do metamethod <- valueMetamethod2 x y "__le"
     case metamethod of
       Nil -> not <$> valueLess y x
       _   ->
         do rs <- callValue metamethod [x,y]
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


valueArith1 :: String -> (Number -> Number) -> Value -> Mach Value
valueArith1 event f x =
  case valueNumber x of
    Just n  -> return (Number (f n))
    Nothing -> do metamethod <- valueMetamethod x event
                  case metamethod of
                    Nil -> badArithmetic x
                    _   -> trimResult1 <$> callValue metamethod [x]


valueInt1 :: String -> (Int -> Int) -> Value -> Mach Value
valueInt1 event f x1 =
  case valueInt x1 of
    Just i  -> return (Number (Int (f i)))
    Nothing -> do metamethod <- valueMetamethod x1 event
                  case metamethod of
                     Nil -> badInt
                     _   -> trimResult1 <$> callValue metamethod [x1]



valueArith2 ::
  String -> (Number -> Number -> Number) -> Value -> Value -> Mach Value
valueArith2 event f x y =
  case (valueNumber x, valueNumber y) of
    (Just l, Just r) -> return (Number (f l r))
    (vl,vr) ->
        do metamethod <- valueMetamethod2 x y event
           case (metamethod,vl,vr) of
             (Nil,Nothing,_) -> badArithmetic x
             (Nil,_,Nothing) -> badArithmetic y
             _               -> trimResult1 <$> callValue metamethod [x,y]

valueInt2 ::
  String -> (Int -> Int -> Int) -> Value -> Value -> Mach Value
valueInt2 event f x1 x2 =
  case (valueInt x1, valueInt x2) of
    (Just l, Just r) -> return (Number (Int (f l r)))
    _ -> do metamethod <- valueMetamethod2 x1 x2 event
            case metamethod of
              Nil -> badInt
              _   -> trimResult1 <$> callValue metamethod [x1,x2]







--------------------------------------------------------------------------------
-- Length of tables and strings

badLength :: Value -> Mach a
badLength x = luaError ("attempt to get length of a " ++
                                    prettyValueType (valueType x) ++ " value")

valueLength :: Value -> Mach Value
valueLength v =
  case v of
    String x -> return (Number (fromIntegral (B.length (toByteString x))))
    _ -> do metamethod <- valueMetamethod v "__len"
            case (metamethod,v) of
              (Nil,Table t) -> Number . Int <$> liftIO (tableLen t)
              (Nil,_      ) -> badLength v
              _             -> trimResult1 <$> callValue metamethod [v]



opConcat :: [Value] -> Mach Value
opConcat = concat2 . reverse

nullLuaString :: LuaString -> Bool
nullLuaString = B.null . toByteString

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
    _ -> do metamethod <- valueMetamethod2 y x "__concat"
            case metamethod of
              Nil -> badConcat y x
              _   ->
                do r <- trimResult1 <$> callValue metamethod [y,x]
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


