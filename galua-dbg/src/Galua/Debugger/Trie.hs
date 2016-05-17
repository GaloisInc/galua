module Galua.Debugger.Trie where

import Data.Map (Map)
import qualified Data.Map as Map

data Trie k v = Trie [v] (Map k (Trie k v))
                deriving Show

emptyTrie :: Trie k v
emptyTrie = Trie [] Map.empty

trieRootVals :: Trie k v -> [v]
trieRootVals (Trie vs _) = vs

insertTrie :: Ord k => [k] -> v -> Trie k v -> Trie k v
insertTrie ks v (Trie vs mp) =
  case ks of
    []      -> Trie (v:vs) mp
    k : ks1 ->
      let t = Map.findWithDefault emptyTrie k mp
      in Trie vs (Map.insert k (insertTrie ks1 v t) mp)

singleTrie :: Ord k => [k] -> v -> Trie k v
singleTrie xs v = insertTrie xs v emptyTrie

mergeTrie :: Ord k => Trie k v -> Trie k v -> Trie k v
mergeTrie (Trie xs1 mp1) (Trie xs2 mp2) =
  Trie (xs1 ++ xs2) (Map.unionWith mergeTrie mp1 mp2)

trieKeys :: Trie k v -> [[k]]
trieKeys = go []
  where
  go path (Trie vs m) =
    let here = if null vs then [] else [ reverse path ]
    in here ++ [ r | (k,tr) <- Map.toList m, r <- go (k:path) tr ]

data Tree k v = Node k [Tree k v] | Leaf v

trieToForest :: Trie k v -> [Tree k v]
trieToForest (Trie vs mp) =
  map Leaf vs ++ [ Node k (trieToForest t) | (k,t) <- Map.toList mp ]

collapseTree :: (k -> k -> k) -> Tree k v -> Tree k v
collapseTree jn t =
  case t of
    Node k1 [t1] ->
      case collapseTree jn t1 of
        Node k2 ts -> Node (jn k1 k2) ts
        Leaf v     -> Leaf v
    Node k1 ts -> Node k1 (map (collapseTree jn) ts)
    Leaf v     -> Leaf v


