module Main(main) where

import qualified Data.Text.IO as Text
import System.Environment(getArgs)
import Text.Show.Pretty(pPrint)

import Galua.Spec.Parser
import Galua.Spec.AST(pretty)

main :: IO ()
main = mapM_ testParse =<< getArgs

testParse :: FilePath -> IO ()
testParse file =
  do s <- specFromFile file
     print (pretty s)
