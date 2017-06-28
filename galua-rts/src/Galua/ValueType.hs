{-# LANGUAGE DeriveGeneric #-}
module Galua.ValueType where

import GHC.Generics (Generic)
import Galua.Pretty(text, Pretty(..))

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

instance Pretty ValueType where
  pp ty = text (prettyValueType ty)

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


