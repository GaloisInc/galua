{-# LANGUAGE RecordWildCards, BangPatterns #-}
module Galua.Util.Table
  ( Table
  , newTable
  , setTableMeta
  , getTableMeta
  , getTableRaw
  , setTableRaw
  , tableLen
  , tableFirst
  , tableNext
  , TableValue(..)
  , tableToList
  ) where

import qualified Data.Vector as Vector
import           Data.Vector.Mutable(IOVector)
import qualified Data.Vector.Mutable as IOVector
import           Data.HashTable.IO(CuckooHashTable,lookupIndex,nextByIndex)
import qualified Data.HashTable.IO as Hash
import           Data.Hashable(Hashable)
import           Data.IORef(IORef,newIORef,writeIORef,readIORef)

-- Desired goals (not necessarily implemented)
-- 1. Indexable by all values
-- 2. Support for weak keys and values
-- 3. Able to remove elements while iterating

type Hash v = CuckooHashTable v v

data Table v = Table
  { tableArray :: {-# UNPACK #-} !(IOVector v)
  , tableHash  :: {-# UNPACK #-} !(Hash v)
  , tableMeta  :: {-# UNPACK #-} !(IORef v) -- ^ Either "Table" or "Nil"
  }

class (Eq v, Hashable v) => TableValue v where
  nilTableValue     :: v
  isNilTableValue   :: v -> Bool
  tableValueToInt   :: v -> Maybe Int
  tableValueFromInt :: Int -> v   -- should not return `nil`


newTable :: TableValue v => Int -> Int -> IO (Table v)
newTable arraySize hashSize =
  do tableArray <- IOVector.replicate arraySize nilTableValue
     tableHash  <- Hash.newSized hashSize
     tableMeta  <- newIORef nilTableValue
     return Table { .. }

setTableMeta :: Table v -> v -> IO ()
setTableMeta Table { .. } v = writeIORef tableMeta v

getTableMeta :: Table v -> IO v
getTableMeta Table { .. } = readIORef tableMeta

getTableRaw :: TableValue v => Table v -> v {- ^ key -} -> IO v
getTableRaw Table { .. } key =
  case tableValueToInt key of
    Just i
      | 1 <= i && i <= IOVector.length tableArray ->
        IOVector.read tableArray (i-1)
      | otherwise -> lkp (tableValueFromInt i)
    _ -> lkp key
  where
  lkp k = do mb <- Hash.lookup tableHash k
             case mb of
               Nothing -> return nilTableValue
               Just v  -> return v

setTableRaw ::
  TableValue v => Table v -> v {- ^ key -} -> v {- ^ value -} -> IO ()
setTableRaw Table { .. } key !val =
  case tableValueToInt key of
    Just i
      | 1 <= i && i <= IOVector.length tableArray ->
        IOVector.write tableArray (i-1) val
      | otherwise -> setHash (tableValueFromInt i)
    _ -> setHash key

  where
  setHash k
    | isNilTableValue val = Hash.delete tableHash k
    | otherwise           = Hash.insert tableHash k val


tableLen :: TableValue v => Table v -> IO Int
tableLen Table { .. }
  | n < 1 = hashLen 0 tableHash
  | otherwise =
     do z <- IOVector.read tableArray 0
        if isNilTableValue z
           then return 0
           else do l <- arrayLen 0 (n-1) tableArray
                   if l < n - 1
                      then return (l + 1)
                      else hashLen n tableHash
  where n = IOVector.length tableArray

arrayLen :: TableValue v => Int -> Int -> IOVector v -> IO Int
arrayLen lo hi a
  | lo >= hi = return hi
  | otherwise = do let m = hi-(hi-lo) `div` 2
                   v1 <- IOVector.read a m
                   if isNilTableValue v1
                      then arrayLen lo (m-1) a
                      else arrayLen m hi a

hashLen :: TableValue v => Int -> Hash v -> IO Int
hashLen i h =
  do mb <- Hash.lookup h (tableValueFromInt (i + 1))
     case mb of
       Nothing -> return i
       Just _  -> hashLen (i + 1) h

tableFirst :: TableValue v => Table v -> IO (Maybe (v,v))
tableFirst t = tableFirstArray t 0

-- Assumes that the `Int` parameters is non-negative.
tableFirstArray :: TableValue v => Table v -> Int -> IO (Maybe (v,v))
tableFirstArray Table { .. } = go
  where
  n = IOVector.length tableArray

  go i | i >= n    = tableFirstHash tableHash
       | otherwise = do x <- IOVector.read tableArray i
                        if isNilTableValue x
                          then go (i+1)
                          else return (Just (tableValueFromInt (i+1), x))
                                -- i+1 because while vector is 0-indexed
                                -- lua sequences are 1 indexed

tableFirstHash :: TableValue v => Hash v -> IO (Maybe (v,v))
tableFirstHash t =
  do mb <- nextByIndex t 0
     return $! do (_,k,v) <- mb
                  return (k,v)

tableNext :: TableValue v => Table v -> v -> IO (Maybe (v,v))
tableNext t key
  | isNilTableValue key = tableFirst t

  | Just i <- tableValueToInt key
  , 0 <= i && i < IOVector.length (tableArray t) = tableFirstArray t i
      -- start at 'i-1' + 1, next element counteracts
      -- the vector indexes being one less

  | otherwise = do res <- lookupIndex (tableHash t) key
                   case res of
                     Nothing -> return Nothing -- XXX: raise error
                     Just ix -> do mb <- nextByIndex (tableHash t) (ix+1)
                                   return $! do (_,k,v) <- mb
                                                return (k,v)

tableToList :: TableValue v => Table v -> IO [(v,v)]
tableToList Table { .. } =
  do vec <- Vector.freeze tableArray
     let xs = filter (\(_,y) -> not (isNilTableValue y))
            $ zip (map tableValueFromInt [ 1 .. ])
                  (Vector.toList vec)
     ys <- Hash.toList tableHash
     return (xs ++ ys)
