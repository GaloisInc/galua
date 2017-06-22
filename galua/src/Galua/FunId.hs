module Galua.FunId where

import Data.List(intercalate)
import Text.Read(readMaybe)

import Galua.Pretty

-- | Path from root to function.
newtype FunId = FunId [Int]
                deriving (Eq,Ord,Show)

-- | Is this a top-level function?
isRootFun :: FunId -> Bool
isRootFun (FunId x) = case x of
                        [_] -> True
                        _   -> False

-- | Get the identifier of the top level function.
getRoot :: FunId -> Maybe Int
getRoot (FunId xs) = if null xs then Nothing else Just (last xs)

-- | This 'FunId' does not refer to any function.
noFun :: FunId
noFun = FunId []

-- | Is this the unique identifier that refers to no function?
isNoFun :: FunId -> Bool
isNoFun (FunId xs) = null xs

-- | Make 'FunId' referring to a particular top-level function.
rootFun :: Int -> FunId
rootFun n = FunId [n]

-- | Make a reference to a particular nested function.
subFun :: FunId -> Int -> FunId
subFun (FunId xs) y = FunId (y : xs)

-- | Textual version of a 'FunId'
funIdString :: FunId -> String
funIdString (FunId xs) = intercalate "_" (map show (reverse xs))

-- | Try to parse the textual versin of a 'FunId'
funIdFromString :: String -> Maybe FunId
funIdFromString = fmap (FunId . reverse) . mapM readMaybe . words . map cvt
  where
  cvt x = if x == '_' then ' ' else x

-- | Access the 'FunId' as a list.
funIdList :: FunId -> [Int]
funIdList (FunId xs) = reverse xs

-- | How nested is this function?   Top entries are 1, 0 referes to 'noFun'.
funNestDepth :: FunId -> Int
funNestDepth (FunId xs) = length xs

instance Pretty FunId where
  pp = text . funIdString


