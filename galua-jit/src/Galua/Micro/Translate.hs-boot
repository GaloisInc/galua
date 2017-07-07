module Galua.Micro.Translate (MicroFunction,blankMicroFunction,translate) where

import {-# SOURCE #-} Galua.Micro.AST(MicroFunction,blankMicroFunction)
import {-# SOURCE #-} Galua.Code(Function)

translate :: Function -> MicroFunction
