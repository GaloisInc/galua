module Galua.Util.IOVector where

import qualified Data.Vector.Mutable as MVector
import Data.Vector.Mutable (MVector)
import Control.Monad.Primitive

readMaybe :: PrimMonad m => MVector (PrimState m) a -> Int -> m (Maybe a)
readMaybe v i
  | 0 <= i, i < MVector.length v = Just <$> MVector.unsafeRead v i
  | otherwise                    = pure Nothing
{-# INLINE readMaybe #-}
