-- | Specialize the code for a function with the given type information.
module Galua.Micro.Translate.Specialize (specialize) where

import           Data.Map (Map)
import qualified Data.Map as Map

import Galua.Micro.AST
import Galua.Micro.Type.Value(LocalState(..))

specialize :: Map BlockName LocalState -> MicroFunction -> MicroFunction
specialize _ xxx = xxx

{- Observation:

If at the end of block `B` we have a case:

    case type of R of
      Nil -> goto C
      ...

and we've already computed that at the start of `C` register `R` is
of type `Number`, then this branch of the case cannot be taken.
-}

