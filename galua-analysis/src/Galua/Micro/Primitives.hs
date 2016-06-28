{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Galua.Micro.Primitives where

import Galua.Micro.Type.Value
import Galua.Micro.Type.Eval
import Galua.Micro.Type.Monad
import qualified Data.Set as Set
import Control.Monad
import Data.ByteString(ByteString)

typeToString :: Type -> ByteString
typeToString t =
  case t of
    Nil           -> "nil"
    Bool          -> "boolean"
    Number        -> "number"
    UserData      -> "userdata"
    LightUserData -> "userdata"
    Thread        -> "thread"

primType :: AnalysisM m => List Value -> m (List Value)
primType args =
  do val <- go =<< valueCasesM (appList args 0)
     return (listAppend [fromSingleV val] (listConst topVal))
  where
  go val =
    case val of
      BasicValue        t -> return (StringValue (Just (typeToString t)))
      StringValue _       -> return (StringValue (Just "string"))
      TableValue        _ -> return (StringValue (Just "table"))
      FunctionValue     _ -> return (StringValue (Just "function"))
