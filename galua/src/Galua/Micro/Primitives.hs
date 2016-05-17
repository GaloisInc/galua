{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Galua.Micro.Primitives where

import Galua.Micro.Type.Value
import Galua.Micro.Type.Eval
import qualified Data.Set as Set
import Control.Monad
import Data.ByteString(ByteString)

typeToString :: Type -> EvalM ByteString
typeToString t =
  case t of
    None -> error "raise error"
    Nil           -> return "nil"
    Bool          -> return "boolean"
    Number        -> return "number"
    UserData      -> return "userdata"
    LightUserData -> return "userdata"
    Thread        -> return "thread"

primType :: List Value -> EvalM (List Value)
primType args =
  do val <- go =<< valueCasesM (appList args 0)
     let none = bottom { valueBasic = Set.singleton None }
     return (listAppend [fromSingleV val] (listConst none))
  where
  go val =
    case val of
      BasicValue        t -> StringLitValue <$> typeToString t
      StringValue         -> return (StringLitValue "string")
      StringLitValue    _ -> return (StringLitValue "string")
      TableValue        _ -> return (StringLitValue "table")
      FunctionValue     _ -> return (StringLitValue "function")
      RefValue  ref -> do
        GlobalState { heap } <- getGlobal
        let vr = appFinMap heap ref
        when (vr == bottom) impossible
        go =<< valueCasesM vr

      ListValue {} -> impossible
