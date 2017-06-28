{-# Language UnboxedTuples, MagicHash #-}

module Galua.Util.Weak
  ( MakeWeak(..)
  , mkWeakIORef'
  , mkWeakMVector'
  ) where

import GHC.Prim  (mkWeak#)
import GHC.Types (IO(IO))
import GHC.IORef (IORef(IORef))
import GHC.STRef (STRef(STRef))
import GHC.Weak  (Weak(Weak))
import Data.Vector.Mutable (MVector(MVector))
import Data.Primitive.Array (MutableArray(MutableArray))

class MakeWeak a where
  makeWeak :: a -> IO () -> IO (Weak a)

mkWeakIORef' :: IORef a -> b -> IO () -> IO (Weak b)
mkWeakIORef' (IORef (STRef r#)) v (IO finalizer) = IO $ \s ->
    case mkWeak# r# v finalizer s of (# s1, w #) -> (# s1, Weak w #)

mkWeakMVector' :: MVector s a -> b -> IO () -> IO (Weak b)
mkWeakMVector' (MVector _ _ (MutableArray r#)) v (IO finalizer) = IO $ \s ->
    case mkWeak# r# v finalizer s of (# s1, w #) -> (# s1, Weak w #)
