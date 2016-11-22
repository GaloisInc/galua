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
  , tableGetMeta
  ) where

import           Data.HashTable.IO(CuckooHashTable,lookupIndex,nextByIndex)
import qualified Data.HashTable.IO as Hash
import           Data.Hashable(Hashable)
import           Data.IORef(IORef,newIORef,writeIORef,readIORef)
import qualified Galua.Util.SizedVector as SV
import qualified Data.Vector as Vector
import           Data.Vector (Vector)

-- Desired goals (not necessarily implemented)
-- 1. Indexable by all values
-- 2. Support for weak keys and values
-- 3. Able to remove elements while iterating

type Hash v = CuckooHashTable v v

data Table v = Table
  { tableArray :: {-# UNPACK #-} !(SV.SizedVector v)
  , tableHash  :: {-# UNPACK #-} !(Hash v)
  , tableMeta  :: {-# UNPACK #-} !(IORef v) -- ^ Either "Table" or "Nil"
  , tableMCache:: {-# UNPACK #-} !(IORef (Vector v))
  }

class (Eq v, Hashable v) => TableValue v where
  nilTableValue     :: v
  isNilTableValue   :: v -> Bool
  tableValueToInt   :: v -> Maybe Int
  tableValueFromInt :: Int -> v   -- should not return `nil`


newTable :: TableValue v => Int -> Int -> IO (Table v)
newTable arraySize hashSize =
  do tableArray <- SV.newN arraySize
     tableHash  <- Hash.newSized hashSize
     tableMeta  <- newIORef nilTableValue
     tableMCache <- newIORef Vector.empty
     return Table { .. }

setTableMeta :: Table v -> v -> IO ()
setTableMeta Table { .. } v = writeIORef tableMeta v

getTableMeta :: Table v -> IO v
getTableMeta Table { .. } = readIORef tableMeta

getTableRaw :: TableValue v => Table v -> v {- ^ key -} -> IO v
getTableRaw Table { .. } key =
  case tableValueToInt key of
    Just i
      | 1 <= i ->
        do n <- SV.size tableArray
           if i <= n then SV.get tableArray (i-1)
                     else lkp (tableValueFromInt i)
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
  do writeIORef tableMCache Vector.empty
     case tableValueToInt key of
       Just i
         | 1 <= i ->
             do n <- SV.size tableArray
                if i <= n then SV.set tableArray (i-1) val
                  else if i == n+1 && not (isNilTableValue val)
                     then do SV.push tableArray val
                             Hash.delete tableHash (tableValueFromInt i)
                  else setHash (tableValueFromInt i)
         | otherwise -> setHash (tableValueFromInt i)
       _ -> setHash key

  where
  setHash k
    | isNilTableValue val = Hash.delete tableHash k
    | otherwise           = Hash.insert tableHash k val


tableLen :: TableValue v => Table v -> IO Int
tableLen Table { .. } =
  do n <- SV.size tableArray
     if n < 1 then hashLen 0 tableHash
       else do z <- SV.get tableArray 0
               if isNilTableValue z then return 0
                 else do l <- arrayLen 0 (n-1) tableArray
                         if l < n - 1
                            then return (l + 1)
                            else hashLen n tableHash

arrayLen :: TableValue v => Int -> Int -> SV.SizedVector v -> IO Int
arrayLen lo hi a
  | lo >= hi = return hi
  | otherwise = do let m = hi-(hi-lo) `div` 2
                   v1 <- SV.get a m
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
tableFirstArray Table { .. } start =
  do n <- SV.size tableArray
     go n start
  where
  go n i
    | i >= n    = tableFirstHash tableHash
    | otherwise = do x <- SV.get tableArray i
                     if isNilTableValue x
                       then go n (i+1)
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
  , 0 <= i = tableFirstArray t i
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
  do elts <- SV.toList tableArray
     let xs = filter (\(_,y) -> not (isNilTableValue y))
            $ zip (map tableValueFromInt [ 1 .. ]) elts
     ys <- Hash.toList tableHash
     return (xs ++ ys)

tableGetMeta :: TableValue v => Table v -> Int -> [v] -> IO v
tableGetMeta t@Table { .. } i ks =
  do old <- readIORef tableMCache
     if Vector.null old then
       do vs <- traverse (getTableRaw t) ks
          let new = Vector.fromList vs
          writeIORef tableMCache $! new
          Vector.indexM new i
     else
          Vector.indexM old i
