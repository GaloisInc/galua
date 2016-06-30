{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Galua.Micro.Primitives (buildPrimMap) where

import Galua.Micro.Type.Value
import Galua.Micro.Type.Eval
import Galua.Micro.Type.Monad
import qualified Data.Set as Set
import Control.Monad
import Data.Maybe
import Data.ByteString(ByteString)
import           Data.Map (Map)
import qualified Data.Map as Map

primitives :: [([ByteString], PrimImpl)]
primitives = [(["type"], PrimImpl primType)]

typeToString :: Type -> ByteString
typeToString t =
  case t of
    Nil           -> "nil"
    Bool          -> "boolean"
    Number        -> "number"
    UserData      -> "userdata"
    LightUserData -> "userdata"
    Thread        -> "thread"

valueToTypeString :: SingleV -> ByteString
valueToTypeString val =
  case val of
    BasicValue        t -> typeToString t
    StringValue _       -> "string"
    TableValue        _ -> "table"
    FunctionValue     _ -> "function"

primType :: AnalysisM m => List Value -> m (List Value)
primType args =
  do val <- valueCasesM (appList args 0)
     let tyString = StringValue (Just (valueToTypeString val))
     return (listAppend [fromSingleV tyString] (listConst topVal))

buildPrimMap :: TableId -> GlobalState -> Map CFun PrimImpl
buildPrimMap globalTableId gs = Map.fromList $ catMaybes [ aux path impl | (path,impl) <- primitives ]
  where
    aux path impl =
      do cfun <- findCFunName (TableValue (Just globalTableId)) gs path
         return (cfun, impl)
