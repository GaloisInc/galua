{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
-- | Implements a cache with a least-recently-used clean-up policy.
module Galua.Util.Cache
  ( Cache
  , cacheEmpty
  , cacheInsert
  , cacheLookup
  , cacheSetLimit
  , cacheGetLimit
  , cacheGC
  ) where

import qualified Data.Map as Map
import           Data.Map ( Map )
import           Data.Word(Word64)
import           Data.List(sortBy)


data Cache k v =
  Cache { cacheLimit  :: !Int
        , cacheTime   :: !Word64
        , cacheValues :: !(Map k (v,Word64))
          -- ^ Maps keys to values and time of last use
        }

-- | Set the maximum size of the cache.  If the cache contains more
-- elements than the limit the least recently used elements will be removed.
cacheSetLimit :: Ord k => Int -> Cache k v -> Cache k v
cacheSetLimit l c
  | lim < Map.size (cacheValues c) = cacheGC c'
  | otherwise                      = c'
  where
  lim = max l 2
  c'  = c { cacheLimit = lim }

-- | Get the maximum size of the cache.
cacheGetLimit :: Cache k v -> Int
cacheGetLimit = cacheLimit


-- | Empty the cache of the given size.
cacheEmpty :: Int -> Cache k v
cacheEmpty l = Cache { cacheLimit   = max l 2
                     , cacheTime    = 1
                     , cacheValues  = Map.empty
                     }

-- | Insert a new element in the cache. If this would cause us to go over
-- the limit, then remove least recently used entries.
cacheInsert :: Ord k => k -> v -> Cache k v -> Cache k v
cacheInsert k v c = c' { cacheValues = Map.insert k (v,0) (cacheValues c') }
  where
  c' | Map.size (cacheValues c) == cacheLimit c = cacheGC c
     | otherwise                                = c



-- | Lookup a value in the cache.
-- The cache is updated to reflect that the value was used recently.
cacheLookup :: Ord k => k -> Cache k v -> Maybe (v, Cache k v)
cacheLookup k Cache { .. } =
  do (v,_) <- mb
     return (v, Cache { cacheTime   = cacheTime + 1
                      , cacheValues = mp1
                      , .. })
  where
  upd _ (v,_) = Just (v,cacheTime)
  (mb,mp1)    = Map.updateLookupWithKey upd k cacheValues


-- | Shrink the cache to 2/3 of the limit, removing values that have not
-- been used recently.
cacheGC :: Ord k => Cache k v -> Cache k v
cacheGC Cache { .. }
  | size <= tgtSize = Cache { .. }
  | otherwise = Cache { cacheTime = 1
                      , cacheValues = Map.fromList updated
                      , ..
                      }
  where
  size    = Map.size cacheValues
  tgtSize = div (2 * cacheLimit) 3

  olderFirst (_,(_,age1)) (_,(_,age2)) = compare age1 age2
                                              -- smaller age is older
  sorted  = drop (size - tgtSize)
          $ sortBy olderFirst (Map.toList cacheValues)

  updated =
    case sorted of
      (_,(_,t)) : _ | t > 1 -> [ (k,(v,t'-(t-1))) | (k,(v,t')) <- sorted ]
      _                     -> sorted





