{-# Language RecordWildCards #-}
-- | Paths whose values are monitroed.
module Galua.Debugger.WatchList where

import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import           Galua.Debugger.ValuePath

data WatchList = WatchList
  { wlNextId :: !Int
  , wlItems  :: !(IntMap ValuePath)
  }

watchListEmpty :: WatchList
watchListEmpty = WatchList { wlNextId = 0, wlItems = IntMap.empty }

watchListToList :: WatchList -> [(Int,ValuePath)]
watchListToList WatchList { .. } = IntMap.toList wlItems

watchListExtend :: ValuePath -> WatchList -> (Int,WatchList)
watchListExtend vp WatchList { .. } =
  (wlNextId, WatchList { wlNextId = 1 + wlNextId
                       , wlItems  = IntMap.insert wlNextId vp wlItems
                       })

watchListRemove :: Int -> WatchList -> WatchList
watchListRemove n WatchList { .. } =
  WatchList { wlItems = IntMap.delete n wlItems, .. }


