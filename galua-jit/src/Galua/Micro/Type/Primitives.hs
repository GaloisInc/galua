{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Galua.Micro.Type.Primitives (buildPrimMap) where

import Galua.Micro.Type.Value
import Galua.Micro.Type.Eval
import Galua.Micro.Type.Monad
import Data.Maybe
import Data.ByteString(ByteString)
import           Data.Map (Map)
import qualified Data.Map as Map

primitives :: [([ByteString], PrimImpl)]
primitives = [ (["type"], PrimImpl primType)
             , (["assert"], PrimImpl primAssert)
             , (["setmetatable"], PrimImpl primSetmetatable)
             ]

typeToString :: Type -> ByteString
typeToString t =
  case t of
    Nil           -> "nil"
    Number        -> "number"
    UserData      -> "userdata"
    LightUserData -> "userdata"
    Thread        -> "thread"

valueToTypeString :: SingleV -> ByteString
valueToTypeString val =
  case val of
    BasicValue        t -> typeToString t
    StringValue _       -> "string"
    BooleanValue _      -> "boolean"
    TableValue        _ -> "table"
    FunctionValue     _ -> "function"

primType :: AnalysisM m => GlobalState -> List Value -> m (Either Value (List Value), GlobalState)
primType glob args =
  do val <- valueCasesM (appList args 0)
     let tyString = StringValue (NotTop (valueToTypeString val))
     return (Right (singleton (fromSingleV tyString)), glob)

primAssert :: AnalysisM m => GlobalState -> List Value -> m (Either Value (List Value), GlobalState)
primAssert glob args =
  do val <- valueCasesM (appList args 0)
     let bad  = Left (fromSingleV (StringValue (NotTop "assertion failed")))
         good = Right (singleton (fromSingleV val))
     case val of
       BasicValue Nil              -> return (bad, glob)
       BooleanValue (NotTop False) -> return (bad, glob)
       BooleanValue Top            -> options [(bad, glob), (good,glob)]
       _                           -> return (good, glob)

primSetmetatable :: AnalysisM m => GlobalState -> List Value -> m (Either Value (List Value), GlobalState)
primSetmetatable glob args =
  do let arg0 = appList args 0
         arg1 = appList args 1
     val <- valueCasesM arg0
     case val of
       TableValue (NotTop tid) ->
         do let setmeta table = Just $! table { tableMeta = arg1 }
                glob' = glob { tables = Map.update setmeta tid (tables glob) }
            return (Right (listFromList [arg0]), glob')
       TableValue Top -> fail "Attempted to set metatable on top table"
       _ -> return (Left (exactString "setmetatable: bad argument"), glob)

singleton :: Value -> List Value
singleton x = listAppend [x] (listConst (basic Nil))

buildPrimMap :: TableId -> GlobalState -> Map CFun PrimImpl
buildPrimMap globalTableId gs = Map.fromList $ catMaybes [ aux path impl | (path,impl) <- primitives ]
  where
    aux path impl =
      do cfun <- findCFunName (TableValue (NotTop globalTableId)) gs path
         return (cfun, impl)
