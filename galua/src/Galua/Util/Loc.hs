module Galua.Util.Loc
  ( -- * Types
    Loc, MonoLoc

    -- * Getting
  , Collects, getLoc, getLocMaybe, getLocs, getLocWith

    -- * Settings
  , Sets, updLoc, setLoc

    -- * Container support
  , intMapAt
  ) where

import Data.Monoid(First(..),Endo(..))
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)

-- | Describes one or more locations, whose types do not change on update.
type MonoLoc f a x = (a -> f a) -> (x -> f x)

-- | Describes one or more locations.
type Loc f a b x y = (a -> f b) -> (x -> f y)

{-# INLINE getLocWith #-}
-- | Collects the value of the locations, using the given function.
getLocWith :: (a -> o) -> (Loc (Collects o) a b x y) -> x -> o
getLocWith g l x = as
  where K as = l (K . g) x

{-# INLINE getLoc #-}
-- | Collects the value of this location.
getLoc :: Loc (Collects a) a b x y -> x -> a
getLoc = getLocWith id

{-# INLINE getLocMaybe #-}
-- | Collects the first location, if any.
getLocMaybe :: Loc (Collects (First a)) a b x y -> x -> Maybe a
getLocMaybe l x = getFirst (getLocWith (First . Just) l x)

{-# INLINE getLocs #-}
-- | Collect the values of all locations in a list.
getLocs :: Loc (Collects (Endo [a])) a b x y -> x -> [a]
getLocs l x = appEndo (getLocWith (\a -> Endo (a:)) l x) []

{-# INLINE updLoc #-}
-- | Update the locatinos with the given function.
updLoc :: Loc Sets a b x y -> (a -> b) -> x -> y
updLoc l f x = y
  where I y = l (I . f) x

{-# INLINE setLoc #-}
-- | Set the lcoations to the specific value.
setLoc :: Loc Sets a b x y -> b -> x -> y
setLoc l v = updLoc l (const v)



--------------------------------------------------------------------------------
newtype Collects i a = K i
newtype Sets a   = I a

instance Functor Sets where
  {-# INLINE fmap #-}
  fmap f (I a) = I (f a)

instance Applicative Sets where
  {-# INLINE pure #-}
  pure        = I
  {-# INLINE (<*>) #-}
  I f <*> I x = I (f x)

instance Functor (Collects i) where
  {-# INLINE fmap #-}
  fmap _ (K i) = K i

instance Monoid i => Applicative (Collects i) where
  {-# INLINE pure #-}
  pure _      = K mempty
  {-# INLINE (<*>) #-}
  K i <*> K j = K (mappend i j)



intMapAt :: Functor f => Int -> MonoLoc f (Maybe a) (IntMap a)
intMapAt k f m = fmap up (f (IntMap.lookup k m))
  where
  up Nothing = IntMap.delete k m
  up (Just x) = IntMap.insert k x m
{-# INLINE intMapAt #-}
