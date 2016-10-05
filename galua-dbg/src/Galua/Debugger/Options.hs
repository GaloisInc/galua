{-# LANGUAGE NamedFieldPuns #-}
module Galua.Debugger.Options where

import Data.Map (Map)
import qualified Data.Map as Map

data Options = Options
  { optBreakPoints  :: CommandLineBreakPoints
  , optBreakOnError :: Bool
  }

-- Map a file, to a tree of functions that need break points
type CommandLineBreakPoints = Map FilePath [Int]

defaultOptions :: Options
defaultOptions = Options
  { optBreakPoints = Map.empty
  , optBreakOnError = True
  }
