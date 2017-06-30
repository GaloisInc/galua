{- |
If we have blocks A and B where B is the sole successr of A, and A is the
sole predecessor of B, then we can combine A and B into a single block.
-}
module Galua.Micro.Translate.JoinBlocks (joinBlocks) where

import qualified Data.Set as Set
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Vector as Vector

import           Galua.Micro.AST

joinBlocks :: MicroFunction -> MicroFunction
joinBlocks f = f { functionCode = doJoin (functionCode f) }

doJoin :: Map BlockName Block -> Map BlockName Block
doJoin blocks = go Set.empty outMap [EntryBlock]
  where
  outMap = addNext <$> blocks
  inMap  = enteredFrom outMap

  go done mp [] = Map.filterWithKey (\k _ -> k `Set.member` done) (fmap fst mp)
  go done mp (b : more)
    | b `Set.member` done = go done mp more
    | otherwise =
    case Map.lookup b mp of
      Nothing -> error "[bug] doJoin/go: a block went missing"
      Just (stmts,next) ->
        case next of
          [y] | Just [_] <- Map.lookup y inMap ->
             let (s',next') = mp Map.! y
                 mp' = Map.insert b (append stmts s',next')
                     $ Map.delete y mp
             in go done mp' (b : more)
          _ -> go (Set.insert b done) mp (more ++ next)




-- | Given the targets where each block can jump, compute
-- where each block may be entered from.
enteredFrom :: Map BlockName (Block, [BlockName]) ->
                                                Map BlockName [BlockName]
enteredFrom nextMap =
  Set.toList <$> Map.fromListWith Set.union [ (y, Set.singleton x)
                               | (x,(_,ys)) <- Map.toList nextMap, y <- ys ]

-- | Compute the successors of a block.
addNext :: Block -> (Block, [BlockName])
addNext b = (b, blockNext b)

-- | Concatenate the statements for two blocks.
append :: Block -> Block -> Block
append xs ys = Block { blockBody = blockBody xs Vector.++ blockBody ys
                     , blockEnd  = blockEnd ys
                     }

