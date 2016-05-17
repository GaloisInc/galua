{-# LANGUAGE OverloadedStrings #-}
-- module Galua.MicroOpCode where

import           Language.Lua.Bytecode.Parser(parseLuaFile)
import qualified Language.Lua.Bytecode as OP
import           Language.Lua.Bytecode (DebugInfo(..),Count(..) )
import qualified Data.Vector as Vector
import           Data.List(intercalate)
import           Control.Monad(replicateM)
import           Data.IORef(newIORef)
import qualified Data.Map as Map

import           Text.Show.Pretty(ppShow)

import Galua.Value(Closure(..),FunctionValue(..),prettyValue)
import Galua.Reference(newRef,runAlloc)
import Galua.Micro.AST(Function(..),ppDot)
import Galua.Micro.Translate(translateTop,translate)
import Galua.Micro.Eval(runFunction)
import Galua.Micro.Type.Eval(analyze)
import Galua.Micro.Type.Pretty()
import Galua.Micro.Type.Filter

import Galua.Micro.Type.Value(bottom,basic,FunId(..),listConst,initLuaArgList
                              , GlobalState(..), fConst, Type(..) )
import qualified Galua.Value as Val (Value(Nil))
import Language.Lua.Bytecode.Pretty(PP(..),blankPPInfo)
import Language.Lua.Bytecode.FunId

main :: IO ()
main = testFile "test.lua"

--------------------------------------------------------------------------------
testFile :: FilePath -> IO ()
testFile f =
  do mb <- parseLuaFile f
     case mb of
       Right (OP.Chunk n fun) ->
         do let fid = rootFun 0
            let tr = translateTop 0 fun
                dotFile pre x = pre ++ "_" ++ funIdString x ++ ".dot"

            let save pre x =
                  sequence_
                    [ writeFile (dotFile pre fid)
                                (show $ ppDot $ functionCode fu) |
                                              (fid,fu) <- Map.toList x ]

            save "out" tr

            let valueAnalysis = analyze tr fid initLuaArgList

                      bottom { basicMetas = fConst (basic Nil)
                             , stringMeta = basic Nil
                             , funMeta    = basic Nil
                             }

            let tr1 = filterFunctions valueAnalysis tr
            save "trim" tr1

            writeFile "va.txt" (show (pp blankPPInfo valueAnalysis))
{-
            ups <- replicateM n (newIORef Val.Nil)
            meta <- newIORef Map.empty
            let clo = MkClosure { cloFun = LuaFunction fid fun
                                , cloUpvalues = Vector.fromList ups
                                }
            res  <- runAlloc (do clor <- newRef clo
                                 runFunction meta clor [])
            case res of
              Left e   -> putStrLn ("Error: " ++ prettyValue e)
              Right as -> mapM_ (putStrLn . prettyValue) as
-}

       Left err -> fail (show err)



