module Galua.FunId where

import Data.List(intercalate)
import Text.Read(readMaybe)
import Data.Word(Word8)

import Galua.Pretty
import qualified Galua.Util.ByteString as BS

-- | Path from root to function.
newtype FunId = FunId BS.ShortByteString
                deriving (Eq,Ord,Show)

-- | Is this a top-level function?
isRootFun :: FunId -> Bool
isRootFun (FunId x) = BS.length x == 1

-- | Get the identifier of the top level function.
getRoot :: FunId -> Maybe Int
getRoot (FunId xs)
  | BS.length xs > 0 = Just (fromIntegral (BS.index xs 0))
  | otherwise = Nothing

-- | This 'FunId' does not refer to any function.
noFun :: FunId
noFun = FunId BS.empty

-- | The parent of this fun-id, if any
funIdParent :: FunId -> Maybe FunId
funIdParent (FunId x) = FunId <$> BS.tail x

-- | Is this the unique identifier that refers to no function?
isNoFun :: FunId -> Bool
isNoFun (FunId xs) = BS.null xs

fits :: Int -> Word8
fits n | n >= 0 && n < 256 = fromIntegral n
       | otherwise         = error "FunId: too many functions [0--255]"

-- | Make 'FunId' referring to a particular top-level function.
rootFun :: Int -> FunId
rootFun n = FunId (BS.pack [ fits n ])

-- | Make a reference to a particular nested function.
subFun :: FunId -> Int -> FunId
subFun (FunId xs) y = FunId (BS.cons (fits y) xs)

-- | Textual version of a 'FunId'
funIdString :: FunId -> String
funIdString (FunId xs) = intercalate "_" (map show (reverse (BS.unpack xs)))

-- | Try to parse the textual versin of a 'FunId'
funIdFromString :: String -> Maybe FunId
funIdFromString = fmap funIdFromList . mapM readMaybe . words . map cvt
  where
  cvt x = if x == '_' then ' ' else x

-- | Access the 'FunId' as a list.
funIdList :: FunId -> [Int]
funIdList (FunId xs) = map fromIntegral (reverse (BS.unpack xs))

-- | Constructo a function id from a list.
funIdFromList :: [Int] -> FunId
funIdFromList = funIdFromInvertedList . reverse

funIdFromInvertedList :: [Int] -> FunId
funIdFromInvertedList = FunId . BS.pack . map fits

-- | How nested is this function?   Top entries are 1, 0 referes to 'noFun'.
funNestDepth :: FunId -> Int
funNestDepth (FunId xs) = BS.length xs

instance Pretty FunId where
  pp = text . funIdString



