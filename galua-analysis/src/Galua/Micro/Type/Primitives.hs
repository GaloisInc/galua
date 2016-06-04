{-# LANGUAGE OverloadedStrings #-}
module Galua.Micro.Type.Primitives where

import Galua.Micro.Type.Value
import qualified Data.Set as Set
import Data.ByteString (ByteString)

typePrim :: FunBehavior
typePrim = FunBehavior
  { funUpVals = []
  , funArgs   = List 0 [] (error "typePrim: funArgs")
  , funPost   = typePrimPost
  }

typePrimPost :: [Value] -> List Value -> FunPost
typePrimPost _ups ListBottom = bottom
typePrimPost _ups args@(List count xs d) = FunPost
  { funReturns    = List 1 [result] (basic Nil)
  , funRaises     = if count >= 1 then bottom else anyString
  , funModTables  = Set.empty
  , funModRefs    = Set.empty
  }
  where
  result = joins $ map (fromSingleV . aux) $ valueCases (args `appList` 0)

  aux val =
    case val of
      BasicValue        t -> StringValue $ Just $ typeToString t
      StringValue       _ -> StringValue $ Just $ "string"
      TableValue        _ -> StringValue $ Just $ "table"
      FunctionValue     _ -> StringValue $ Just $ "function"
      RefValue  {}        -> error "typePrim: reference as argument"

typeToString :: Type -> ByteString
typeToString t =
  case t of
    Nil           -> "nil"
    Bool          -> "boolean"
    Number        -> "number"
    UserData      -> "userdata"
    LightUserData -> "userdata"
    Thread        -> "thread"
