{-# LANGUAGE OverloadedStrings, ForeignFunctionInterface #-}
-- module Galua.MicroOpCode where

import           Language.Lua.Bytecode.Pretty(PP(..),blankPPInfo)
import qualified Language.Lua.Bytecode as BC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified Data.Set as Set

import           Foreign(Ptr)

import Galua.Code
import Galua.Mach(parseLua)
import Galua.Micro.AST(Function(..),ppDot)
import Galua.Micro.Translate(translateTop)
import Galua.Micro.Type.Eval(analyze)
import Galua.Micro.Type.Pretty()
import Galua.Micro.Type.Filter

import Galua.Micro.Type.Value
  ( bottom,basic,initLuaArgList
  , GlobalState(..), fConst, Type(..)
  , TableV(..), FunV(..)
  , TableId(..), RefId(..), ClosureId(..)
  , newTable, externalId
  , Lift(..), FunImpl(..), WithTop(..)
  )


main :: IO ()
main = testFile "test.lua"

--------------------------------------------------------------------------------
testFile :: FilePath -> IO ()
testFile f =
  do txt <- LBS.readFile f
     mb  <- parseLua (Just f) txt
     case mb of
       Right (Chunk _ fun) ->
         do let tr = translateTop 0 (funcOrig fun)
                dotFile pre x = pre ++ "_" ++ funIdString x ++ ".dot"

            let save pre x =
                  sequence_
                    [ writeFile (dotFile pre fid)
                                (show $ ppDot $ functionCode fu) |
                                              (fid,fu) <- Map.toList x ]

            save "out" tr

            let (cid,glob)    = initalGlobalState 0
                valueAnalysis = analyze tr Map.empty cid initLuaArgList glob

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


-- Temporary hack
foreign export ccall lua_newstate :: Ptr a -> Ptr b -> IO (Ptr c)

lua_newstate :: Ptr a -> Ptr b -> IO (Ptr c)
lua_newstate _ _ = fail "can't call lua_newstate in analysis"


-- A test global state that has no meta tables, and one empty global table
initalGlobalState :: Int -> (ClosureId,GlobalState)
initalGlobalState chunkId =
  ( cloRef
  , bottom { basicMetas = fConst (basic Nil)
           , stringMeta = basic Nil
           , funMeta    = basic Nil
           , heap       = Map.singleton upRef  (newTable tabRef)
           , tables     = Map.singleton tabRef globT
           , functions  = Map.singleton cloRef chunkFun
           }
  )

  where
  upRef   = externalId RefId     0
  tabRef  = externalId TableId   0
  cloRef  = externalId ClosureId 0

  globT = TableV { tableFields = fConst (basic Nil)
                 , tableKeys   = bottom
                 , tableValues = basic Nil
                 , tableMeta   = basic Nil
                 }

  chunkFun = FunV { functionUpVals = Map.singleton (BC.UpIx 0)
                                        (NotTop (Set.singleton upRef))
                  , functionFID    = OneValue (LuaFunImpl (rootFun chunkId))
                  }





{-
  gbn0 = GlobalBlockName initialCaller (QualifiedBlockName fid EntryBlock)

  -- XXX: we hack in an upvalue with an empty table,
  -- so that we can use globals, when running a chunk. 
  -- This should be more systematic.
  allocFuns = Map.singleton fid [upVal]
  upVal     = RefId gbn0 0

  setupGlob =
    let tid   = TableId gbn0 0
        empty = 
    in glob { tables = Map.insert tid empty (tables glob)
            , heap   = Map.insert upVal (fromSingleV (TableValue (Just tid)))
                                        (heap glob)
            }
-}



