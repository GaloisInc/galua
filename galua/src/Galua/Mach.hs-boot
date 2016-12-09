module Galua.Mach where

import Galua.Util.Weak
import {-# SOURCE #-} Galua.Value

data Mach a
data Thread

instance MakeWeak Thread
instance ReferenceType Thread
