{-# LANGUAGE NamedFieldPuns #-}
module Galua.Debugger.Options where

import System.Console.GetOpt
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Read(readMaybe)

data Options = Options
  { optBreakPoints  :: CommandLineBreakPoints
  }

-- Map a file, to a tree of functions that need break points
type CommandLineBreakPoints = Map FilePath [Int]

defaultOptions :: Options
defaultOptions = Options
  { optBreakPoints = Map.empty
  }
