module Main(main) where

import qualified Data.Text.IO as Text
import System.Environment(getArgs)
import Text.Show.Pretty(pPrint)

import Galua.Spec.Parser
import Galua.Spec.AST(pretty)

import Galua.Spec.CFG
import Language.Lua.Annotated(parseFile)

main :: IO ()
main = mapM_ testCFG =<< getArgs



testCFG :: FilePath -> IO ()
testCFG file =
  do mb <- parseFile file
     case mb of
       Left err -> print err
       Right b ->
         let res  = topLevel b
         in case res of
              Left err  -> print err
              Right cfg -> print cfg

testParse :: FilePath -> IO ()
testParse file =
  do s <- specFromFile file
     print (pretty s)
