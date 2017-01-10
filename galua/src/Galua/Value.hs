{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

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

  -- * References
  , Reference
  , RefLoc(..)
  , CodeLoc(..)
  , ReferenceType(..)
  ) where

import           Control.Monad.IO.Class
import           Data.ByteString (ByteString)
import           Data.Function(on)
import           Data.IORef
import qualified Data.Text as Text
import           Data.Text.Encoding (decodeUtf8With,encodeUtf8)
import           Data.Text.Encoding.Error (lenientDecode)
import           Data.Vector.Mutable (IOVector)
import           GHC.Generics (Generic)
import           Foreign.Ptr (FunPtr, Ptr,ptrToIntPtr)
import           Foreign.ForeignPtr (ForeignPtr)
import           Foreign.C.Types (CInt, CSize)
import           Foreign.C.String (CString, CStringLen)
import           Data.Hashable(Hashable(..))

import {-# SOURCE #-} Galua.Mach (Thread)

import qualified Galua.Util.Table as Tab

import Galua.Number
import Galua.FunValue
import Galua.ValueType
import Galua.Reference
import Galua.LuaString
import Galua.Util.Weak (MakeWeak(..), mkWeakIORef', mkWeakMVector')

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
    con :: Hashable a => Int -> a -> Int
    con n x = hashWithSalt (hashWithSalt s n) x


------------------------------------------------------------------------
-- Threads
------------------------------------------------------------------------


------------------------------------------------------------------------
-- User Data
------------------------------------------------------------------------

data UserData = MkUserData
  { userDataPtr   :: {-# UNPACK #-} !(ForeignPtr ())
  , userDataSize  :: {-# UNPACK #-} !Int
  , userDataValue :: {-# UNPACK #-} !(IORef Value)
  , userDataMeta  :: {-# UNPACK #-} !(IORef (Maybe (Reference Table)))
  }

newUserData ::
  AllocRef -> RefLoc -> ForeignPtr () -> Int -> IO (Reference UserData)
newUserData aref refLoc x n =
  do uval <- liftIO (newIORef Nil)
     mtref <- liftIO (newIORef Nothing)
     newRef aref refLoc (MkUserData x n uval mtref)

------------------------------------------------------------------------
-- Tables
------------------------------------------------------------------------

type Table = Tab.Table Value

newTable ::
  AllocRef ->
  RefLoc ->
  Int {- ^ array size -} ->
  Int {- ^ hashtable size -} ->
  IO (Reference Table)
newTable aref refLoc arraySize hashSize =
  newRef aref refLoc =<< Tab.newTable arraySize hashSize

setTableMeta :: MonadIO m => Reference Table -> Maybe (Reference Table) -> m ()
setTableMeta tr mt = liftIO $ Tab.setTableMeta (referenceVal tr)
                            $ maybe Nil Table mt

getTableMeta :: MonadIO m => Reference Table -> m (Maybe (Reference Table))
getTableMeta ref = liftIO $ do v <- Tab.getTableMeta (referenceVal ref)
                               case v of
                                 Table t' -> return (Just t')
                                 _        -> return Nothing

getTableRaw :: MonadIO io => Reference Table -> Value {- ^ key -} -> io Value
getTableRaw ref key = liftIO $ Tab.getTableRaw (referenceVal ref) key

setTableRaw :: MonadIO m => Reference Table -> Value -> Value -> m ()
setTableRaw ref key !val = liftIO $ Tab.setTableRaw (referenceVal ref) key val

tableLen :: MonadIO io => Reference Table -> io Int
tableLen ref = liftIO (Tab.tableLen (referenceVal ref))

tableFirst :: MonadIO m => Reference Table -> m (Maybe (Value,Value))
tableFirst ref = liftIO (Tab.tableFirst (referenceVal ref))

tableNext :: MonadIO m => Reference Table -> Value -> m (Maybe (Value,Value))
tableNext ref key = liftIO (Tab.tableNext (referenceVal ref) key)

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
  , cloUpvalues :: IOVector (IORef Value)
  }

data PrimArgument
  = PrimIntArg Integer
  | PrimDoubleArg Double
  | PrimCIntArg (Ptr CInt)
  | PrimCSizeArg (Ptr CSize)
  | PrimCStringArg CString
  | PrimCStringLenArg CStringLen
  | PrimPtrArg (Ptr ())
  | PrimFunPtrArg (FunPtr ())
  deriving Show

newClosure ::
  AllocRef ->
  RefLoc -> FunctionValue -> IOVector (IORef Value) -> IO (Reference Closure)
newClosure aref refLoc f us =
  newRef aref refLoc MkClosure { cloFun = f, cloUpvalues = us }


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

{-# INLINE valueNumber #-}
valueNumber :: Value -> Maybe Number
valueNumber (Number n) = Just n
valueNumber (String n) = forceDouble <$> parseNumber (unpackUtf8 (toByteString n))
valueNumber _          = Nothing

{-# INLINE valueBool #-}
valueBool :: Value -> Bool
valueBool Nil      = False
valueBool (Bool b) = b
valueBool _        = True

{-# INLINE valueInt #-}
valueInt :: Value -> Maybe Int
valueInt (Number n) = numberToInt n
valueInt (String n) = numberToInt =<< parseNumber (unpackUtf8 (toByteString n))
valueInt _          = Nothing

{-# INLINE valueString #-}
valueString :: Value -> Maybe ByteString
valueString v =
  case v of
    Number x -> Just (packUtf8 (numberToString x))
    String x -> Just (toByteString x)
    _        -> Nothing


-- | Trim a list of values down to the first value.
-- The empty list is replaced with 'Nil'
{-# INLINE trimResult1 #-}
trimResult1 :: [Value] -> Value
trimResult1 [] = Nil
trimResult1 (x:_) = x

--------------------------------------------------------------------------------

instance MakeWeak UserData where
  makeWeak ud = mkWeakIORef' (userDataMeta ud) ud

instance MakeWeak Closure where
  makeWeak cl = mkWeakMVector' (cloUpvalues cl) cl

data family Reference a

data instance Reference Table = TableRef
  { tableReferenceId  :: {-# UNPACK #-} !Int
  , tableReferenceLoc :: {-# UNPACK #-} !RefLoc
  , tableReferenceVal :: {-# UNPACK #-} !Table
  }

data instance Reference UserData = UserDataRef
  { userDataReferenceId  :: {-# UNPACK #-} !Int
  , userDataReferenceLoc :: {-# UNPACK #-} !RefLoc
  , userDataReferenceVal :: {-# UNPACK #-} !UserData
  }

data instance Reference Closure = ClosureRef
  { closureReferenceId  :: {-# UNPACK #-} !Int
  , closureReferenceLoc :: {-# UNPACK #-} !RefLoc
  , closureReferenceVal :: {-# UNPACK #-} !Closure
  }

class ReferenceType a where
  constructReference :: Int -> RefLoc -> a -> Reference a
  referenceLoc     :: Reference a -> RefLoc
  referenceId      :: Reference a -> Int
  referenceVal     :: Reference a -> a


instance ReferenceType UserData where
  constructReference = UserDataRef
  referenceId = userDataReferenceId
  referenceLoc = userDataReferenceLoc
  referenceVal = userDataReferenceVal

instance ReferenceType Closure where
  constructReference = ClosureRef
  referenceId = closureReferenceId
  referenceLoc = closureReferenceLoc
  referenceVal = closureReferenceVal

instance ReferenceType Table where
  constructReference = TableRef
  referenceId = tableReferenceId
  referenceVal = tableReferenceVal
  referenceLoc = tableReferenceLoc

-- | The location in the source code that allocated this reference.
data RefLoc  = RefLoc { refLocCaller :: !CodeLoc, refLocSite :: !CodeLoc }
               deriving (Eq,Ord)

data CodeLoc = MachSetup
             | InC !CFunName
             | InLua !FunId !Int  -- ^ Function, program counter
               deriving (Eq,Ord)

instance ReferenceType a => Show (Reference a) where
  show = prettyRef

instance ReferenceType a => Eq (Reference a) where
  (==) = (==) `on` referenceId
  (/=) = (/=) `on` referenceId

instance ReferenceType a => Ord (Reference a) where
  compare = compare `on` referenceId
  (<=) = (<=) `on` referenceId
  (<)  = (<)  `on` referenceId
  (>)  = (>)  `on` referenceId
  (>=) = (>=) `on` referenceId

