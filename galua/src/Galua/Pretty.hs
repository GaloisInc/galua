-- | Pretty printing functionality.
module Galua.Pretty
  ( module Text.PrettyPrint
  , module Galua.Pretty
  ) where

import Text.PrettyPrint

class Pretty t where
  pp :: t -> Doc

instance Pretty Doc where
  pp = id

