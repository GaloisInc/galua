module Galua.FunValue
  ( FunctionValue
  , luaFunction
  , cFunction
  , luaOpCodes
  , funValueCode
  , FunCode(..)
  , isCFunction

  , funValueName
  , FunName(..)
  , FunId
  , CFunName(..)
  , CFun
  , blankCFunName
  ) where

import Foreign.C(CInt)
import Foreign.Ptr (FunPtr,nullFunPtr,Ptr)

import {-# SOURCE #-} Galua.Code(Function,FunId)
import Galua.CObjInfo(CObjInfo,noFunInfo)


type CFun = FunPtr (Ptr () -> IO CInt)

data FunctionValue
  = CFunction CFunName
  | LuaFunction FunId Function

isCFunction :: FunctionValue -> Bool
isCFunction (CFunction _) = True
isCFunction (LuaFunction {}) = False

cFunction :: CFunName -> FunctionValue
cFunction = CFunction

luaFunction :: FunId -> Function -> FunctionValue
luaFunction = LuaFunction

data FunName = CFID CFunName | LuaFID FunId
                deriving (Eq,Ord)

funValueName :: FunctionValue -> FunName
funValueName (CFunction   name  ) = CFID   name
funValueName (LuaFunction name _) = LuaFID name

luaOpCodes :: FunctionValue -> Maybe (FunId, Function)
luaOpCodes (LuaFunction fid f) = Just (fid,f)
luaOpCodes (CFunction {})      = Nothing


data FunCode = LuaOpCodes Function
             | CCode CFunName

funValueCode :: FunctionValue -> FunCode
funValueCode (CFunction name)  = CCode name
funValueCode (LuaFunction _ f) = LuaOpCodes f


data CFunName = CFunName
  { cfunName :: CObjInfo
  , cfunAddr :: CFun
  }

instance Eq CFunName where
  x == y = cfunAddr x == cfunAddr y

instance Ord CFunName where
  compare x y = compare (cfunAddr x) (cfunAddr y)

blankCFunName :: CFunName
blankCFunName = CFunName { cfunName = noFunInfo nullFunPtr
                         , cfunAddr = nullFunPtr }
