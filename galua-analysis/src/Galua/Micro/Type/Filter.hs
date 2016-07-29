module Galua.Micro.Type.Filter (filterFunctions) where

import           Data.Map(Map)
import qualified Data.Map as Map
import           Data.Set(Set)
import qualified Data.Set as Set
import           Data.Vector(Vector)
import qualified Data.Vector as Vector
import           Data.List(groupBy)
import           Data.Function(on)
import           Data.Maybe(mapMaybe)

import Language.Lua.Bytecode.FunId

import Galua.Micro.AST
import Galua.Micro.Type.Eval
         (Result(..),GlobalBlockName(..),QualifiedBlockName(..))

filterFunctions :: Result -> Map FunId Function -> Map FunId Function
filterFunctions res = Map.mapWithKey check
  where
  used  = usedBlocks res
  check fid fu = case Map.lookup fid used of
                   Nothing -> fu  -- shouldn't really happen
                   Just us -> filterFunction us fu



filterFunction :: Set BlockName -> Function -> Function
filterFunction used f = f { functionCode = fmap (filterStmts used)
                                         $ Map.filterWithKey isUsed
                                         $ functionCode f }
  where
  isUsed k _ = k `Set.member` used

filterStmts :: Set BlockName -> Vector Stmt -> Vector Stmt
filterStmts used = Vector.fromList . concatMap (filterStmt used) . Vector.toList

filterStmt :: Set BlockName -> Stmt -> [Stmt]
filterStmt used stmt =
  case stmt of
    Case e as d ->
      case (newAlts,dflt) of
        ([], Just def)      -> [ Goto def]
        ([(_,b)],Nothing) -> [ Goto b ]
        _                 -> [ Case e newAlts dflt ]
      where
      newAlts   = mapMaybe alt as
      alt (t,b) = if isUsed b then Just (t,b) else Nothing
      dflt      = (\b -> if isUsed b then Just b else Nothing) =<< d

    If _ t e
      | isUsed t && isUsed e -> [ stmt ]
      | isUsed t             -> [ Goto t ]
      | isUsed e             -> [ Goto e ]
      | otherwise            -> []

    _ -> [ stmt ]

  where
  isUsed b = b `Set.member` used


usedBlocks :: Result -> Map FunId (Set BlockName)
usedBlocks =
  Map.fromList . map cvt . groupBy ((==) `on` getFun) . Map.keys . resStates
  where
  getFun   (GlobalBlockName _ (QualifiedBlockName f _)) = f
  getBlock (GlobalBlockName _ (QualifiedBlockName _ b)) = b
  cvt xs = (getFun (head xs), Set.fromList (map getBlock xs))

