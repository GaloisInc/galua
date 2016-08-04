module Main where

import Data.Macho
import System.IO
import Test.HUnit
import Control.Monad
import qualified Control.Exception as E
import qualified Data.ByteString as B

tests = TestList
    [ 
    ]

main = runTestTT tests
