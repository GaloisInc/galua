{-# Language TypeFamilies, FlexibleInstances #-}
module Galua.Value where

import Galua.Util.Weak
import Galua.Util.Table (Table)

data Closure
data Value
data UserData

instance MakeWeak Closure
instance MakeWeak UserData

data family Reference a
data RefLoc

class ReferenceType a where
  constructReference :: Int -> RefLoc -> a -> Reference a
  referenceLoc     :: Reference a -> RefLoc
  referenceId      :: Reference a -> Int
  referenceVal     :: Reference a -> a

instance ReferenceType Closure
instance ReferenceType UserData
instance ReferenceType (Table Value)
