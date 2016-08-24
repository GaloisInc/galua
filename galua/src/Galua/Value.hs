{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}

module Galua.Value
  ( -- * Values
    Value(..)
  , packUtf8
  , unpackUtf8

  , -- * Tables
    Table
  , newTable
  , setTableRaw
  , getTableRaw
  , setTableMeta
  , getTableMeta
  , tableLen
  , tableFirst
  , tableNext

  -- * Closures
  , Closure(..)
  , FunctionValue
  , Prim
  , PrimArgument(..)
  , newClosure
  , FunName(..)
  , funValueName
  , CFun
  , CFunName(..)
  , blankCFunName

  -- * User Data
  , UserData(..)
  , newUserData

  -- * Types
  , ValueType(..)
  , valueType

  -- * Casts
  , valueNumber
  , valueBool
  , valueInt
  , valueString
  , trimResult1

  -- * Lua Function Environment
  , prettyValue
  , prettyValueType
  ) where

import           Control.Monad.IO.Class
import           Data.ByteString (ByteString)
import           Data.IORef
import qualified Data.Text as Text
import           Data.Text.Encoding (decodeUtf8With,encodeUtf8)
import           Data.Text.Encoding.Error (lenientDecode)
import           Data.Vector (Vector)
import           GHC.Generics (Generic)
import           Foreign.Ptr (FunPtr, Ptr,ptrToIntPtr)
import           Foreign.ForeignPtr (ForeignPtr)
import           Foreign.C.Types (CInt, CSize)
import           Foreign.C.String (CString, CStringLen)
import           Data.Hashable(Hashable(..))

import {-# SOURCE #-} Galua.Mach (Thread,Mach)

import qualified Galua.Util.Table as Tab

import Galua.Number
import Galua.FunValue
import Galua.ValueType
import Galua.Reference
import Galua.LuaString

data Value
  = Bool     !Bool
  | Number   !Number
  | String   !LuaString
  | Nil
  | Table    !(Reference Table)
  | Closure  !(Reference Closure)
  | UserData !(Reference UserData)
  | LightUserData !(Ptr ())
  | Thread   !(Reference Thread)
  deriving (Ord,Eq,Generic)


instance Tab.TableValue Value where
  nilTableValue = Nil
  isNilTableValue v = case v of
                        Nil -> True
                        _   -> False
  tableValueToInt v  = case v of
                         Number n -> numberToInt n
                         _        -> Nothing
  tableValueFromInt i = Number (Int i)

instance Hashable Value where
  hashWithSalt s a =
    case a of
      Bool b          -> con 1 b
      Number n        -> con 2 n
      String bs       -> con 3 bs
      Nil             -> con 4 ()
      Table r         -> con 5 (referenceId r)
      Closure r       -> con 6 (referenceId r)
      UserData r      -> con 7 (referenceId r)
      LightUserData p -> con 8 (toInteger (ptrToIntPtr p))
      Thread r        -> con 9 (referenceId r)
    where
    con n x = hashWithSalt (hashWithSalt s (n::Int)) x


------------------------------------------------------------------------
-- Threads
------------------------------------------------------------------------


------------------------------------------------------------------------
-- User Data
------------------------------------------------------------------------

data UserData = MkUserData
  { userDataPtr   :: ForeignPtr ()
  , userDataSize  :: Int
  , userDataValue :: IORef Value
  , userDataMeta  :: Maybe (Reference Table)
  }

newUserData ::
  NameM m => RefLoc -> ForeignPtr () -> Int -> m (Reference UserData)
newUserData refLoc x n =
  do uval <- liftIO (newIORef Nil)
     newRef refLoc (MkUserData x n uval Nothing)

------------------------------------------------------------------------
-- Tables
------------------------------------------------------------------------

type Table = Tab.Table Value

newTable ::
  NameM m =>
  RefLoc ->
  Int {- ^ array size -} ->
  Int {- ^ hashtable size -} ->
  m (Reference Table)
newTable refLoc arraySize hashSize =
  newRef refLoc =<< liftIO (Tab.newTable arraySize hashSize)

setTableMeta :: MonadIO m => Reference Table -> Maybe (Reference Table) -> m ()
setTableMeta tr mt = liftIO $ do t <- readRef tr
                                 Tab.setTableMeta t $ maybe Nil Table mt

getTableMeta :: MonadIO m => Reference Table -> m (Maybe (Reference Table))
getTableMeta ref = liftIO $ do t <- readRef ref
                               v <- Tab.getTableMeta t
                               case v of
                                 Table t' -> return (Just t')
                                 _        -> return Nothing

getTableRaw :: MonadIO io => Reference Table -> Value {- ^ key -} -> io Value
getTableRaw ref key = liftIO $ do t <- readRef ref
                                  Tab.getTableRaw t key

setTableRaw :: MonadIO m => Reference Table -> Value -> Value -> m ()
setTableRaw ref key !val = liftIO $ do t <- readRef ref
                                       Tab.setTableRaw t key val

tableLen :: MonadIO io => Reference Table -> io Int
tableLen ref = liftIO (Tab.tableLen =<< readRef ref)

tableFirst :: NameM m => Reference Table -> m (Maybe (Value,Value))
tableFirst ref = liftIO (Tab.tableFirst =<< readRef ref)


tableNext :: MonadIO m => Reference Table -> Value -> m (Maybe (Value,Value))
tableNext ref key = liftIO $ do t <- readRef ref
                                Tab.tableNext t key

------------------------------------------------------------------------
-- Value types
------------------------------------------------------------------------

valueType :: Value -> ValueType
valueType v =
  case v of
    String {} -> StringType
    Number {} -> NumberType
    Table  {} -> TableType
    Closure{} -> FunctionType
    Bool   {} -> BoolType
    Nil    {} -> NilType
    UserData{} -> UserDataType
    LightUserData{} -> LightUserDataType
    Thread  {} -> ThreadType

------------------------------------------------------------------------
-- Closures
------------------------------------------------------------------------



data Closure = MkClosure
  { cloFun      :: FunctionValue
  , cloUpvalues :: Vector (IORef Value)
  }

type Prim = [Value] -> Mach [Value]

data PrimArgument
  = PrimIntArg Integer
  | PrimDoubleArg Double
  | PrimCIntArg (Ptr CInt)
  | PrimCSizeArg (Ptr CSize)
  | PrimCStringArg CString
  | PrimCStringLenArg CStringLen
  | PrimPtrArg (Ptr ())
  | PrimFunPtrArg (FunPtr ())

newClosure ::
  NameM m =>
  RefLoc -> FunctionValue -> Vector (IORef Value) -> m (Reference Closure)
newClosure refLoc f us = newRef refLoc MkClosure { cloFun = f, cloUpvalues = us }


------------------------------------------------------------------------
-- String functions
------------------------------------------------------------------------

-- | This is the default implementation of tostring which can be overridden
-- by setting the __tostring metamethod.
prettyValue :: Value -> String
prettyValue v =
  case v of
    -- values
    Nil                     -> "nil"
    Bool True               -> "true"
    Bool False              -> "false"
    Number (Int x)          -> show x
    Number (Double x)       -> show x
    String x                -> unpackUtf8 (toByteString x)
    -- references
    Closure c               -> "function: " ++ prettyRef c
    Table t                 -> "table: "    ++ prettyRef t
    UserData u              -> "userdata: " ++ prettyRef u
    LightUserData p         -> "userdata: " ++ show p
    Thread t                -> "thread: "   ++ prettyRef t

unpackUtf8 :: ByteString -> String
unpackUtf8 = Text.unpack . decodeUtf8With lenientDecode

packUtf8 :: String -> ByteString
packUtf8 = encodeUtf8 . Text.pack



--------------------------------------------------------------------------------
-- Simple casts

valueNumber :: Value -> Maybe Number
valueNumber (Number n) = Just n
valueNumber (String n) = forceDouble <$> parseNumber (unpackUtf8 (toByteString n))
valueNumber _          = Nothing

valueBool :: Value -> Bool
valueBool Nil      = False
valueBool (Bool b) = b
valueBool _        = True

valueInt :: Value -> Maybe Int
valueInt (Number n) = numberToInt n
valueInt (String n) = numberToInt =<< parseNumber (unpackUtf8 (toByteString n))
valueInt _          = Nothing

valueString :: Value -> Maybe ByteString
valueString v =
  case v of
    Number x -> Just (packUtf8 (numberToString x))
    String x -> Just (toByteString x)
    _        -> Nothing


-- | Trim a list of values down to the first value.
-- The empty list is replaced with 'Nil'
trimResult1 :: [Value] -> Value
trimResult1 [] = Nil
trimResult1 (x:_) = x

--------------------------------------------------------------------------------



