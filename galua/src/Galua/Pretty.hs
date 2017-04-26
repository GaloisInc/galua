-- | Pretty printing functionality.
module Galua.Pretty
  ( module Text.PrettyPrint
  , module Galua.Pretty
  ) where

import Text.PrettyPrint
import qualified Language.Lua.Bytecode.Pretty as PP
import Language.Lua.Bytecode.FunId(FunId)

class Pretty t where
  pp :: t -> Doc

instance Pretty FunId where
  pp = PP.pp PP.blankPPInfo

instance Pretty Doc where
  pp = id

