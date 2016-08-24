{-# LANGUAGE DeriveGeneric #-}
module Galua.ValueType where

import GHC.Generics (Generic)
import Text.PrettyPrint(text)

import Language.Lua.Bytecode.Pretty(PP(..))

data ValueType
  = NumberType -- ^ integer and float are just representations of number
  | StringType
  | FunctionType
  | TableType
  | BoolType
  | NilType
  | UserDataType
  | LightUserDataType
  | ThreadType
  deriving (Generic,Show,Eq,Ord)

instance PP ValueType where
  pp _ ty = text (prettyValueType ty)

prettyValueType :: ValueType -> String
prettyValueType t =
  case t of
    StringType   -> "string"
    NumberType   -> "number"
    TableType    -> "table"
    FunctionType -> "function"
    BoolType     -> "boolean"
    NilType      -> "nil"
    UserDataType -> "userdata"
    LightUserDataType -> "userdata"
    ThreadType   -> "thread"


