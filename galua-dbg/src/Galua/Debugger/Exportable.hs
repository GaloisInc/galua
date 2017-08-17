-- | Keeping track of expand/collapsed state of references
module Galua.Debugger.Exportable where

import           Data.Map(Map)
import qualified Data.Map as Map
import           Data.Set(Set)
import qualified Data.Set as Set

import Galua.Value(Value)
import Galua.Mach(ExecEnv)

import Galua.Debugger.ValuePath(ValuePath)

data ExportableState = ExportableState
  { expNextThing    :: !Integer
  , expClosed       :: Map Integer Exportable
  , openThreads     :: !(Set Int)
    -- ^ Reference ids of the threads that have been expended.
    -- We keep an int instead of the actual reference, so that the reference
    -- can be garbage collected if it finished.
  }

data Exportable = ExportableValue ValuePath Value
                    -- ^ a collapsed value that may be

                | ExportableStackFrame Int ExecEnv
                  -- ^ A collapse stack frame for a function call



newExportableState :: ExportableState
newExportableState = ExportableState { expNextThing = 0
                                     , expClosed = Map.empty
                                     , openThreads = Set.empty
                                     }


