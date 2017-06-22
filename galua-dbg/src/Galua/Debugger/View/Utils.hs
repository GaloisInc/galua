{-# LANGUAGE OverloadedStrings #-}
module Galua.Debugger.View.Utils
  ( exportFID
  , computeFunNames
  ) where

import           Data.Maybe(fromMaybe)
import           Data.List(sortBy)
import           Data.Function(on)
import qualified Data.Aeson as JS
import qualified Data.Vector as Vector
import           Data.String(fromString)

import Galua.Code
import Galua.Value(unpackUtf8)

data FunName = FunName { fun      :: Function
                       , funName  :: Maybe String
                       , funId    :: FunId
                       }

exportFID :: FunId -> JS.Value
exportFID = fromString . funIdString


computeFunNames :: Int -> Function -> [(FunId, String)]
computeFunNames chunkId fun0 =
  map nameEntry $
  sortBy (compare `on` startLine) $
  getAllProtoNames
    FunName { fun     = fun0
            , funId   = rootFun chunkId
            , funName = Just "(top level)"
            }

  where
  startLine = funcLineDefined . fun

  getAllProtoNames fn =
    fn : [ f | i <- getProtoNames (funId fn) (fun fn), f <- getAllProtoNames i]

  nameEntry i = (funId i, fromMaybe (defaultName i) (funName i))

  defaultName i = show (startLine i) ++ "--" ++
                  show (funcLastLineDefined (fun i))




getProtoNames :: FunId -> Function -> [FunName]
getProtoNames fid f = foldr check [] $ Vector.indexed $ funcCode f
  where
  check (pc,op) mp =
    case op of
      OP_CLOSURE _ k proto ->
        let info  = FunName { fun     = proto
                            , funId   = subFun fid k
                            , funName = unpackUtf8 <$> inferFunctionName f pc
                            }
        in info : mp
      _ -> mp




