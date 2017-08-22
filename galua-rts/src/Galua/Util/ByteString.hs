{-# Language UnboxedTuples, MagicHash, BangPatterns #-}
module Galua.Util.ByteString
  ( cons
  , Galua.Util.ByteString.tail
  , module BS
  ) where



import Data.ByteString.Short.Internal(ShortByteString(..))
import GHC.Prim
import GHC.Int(Int(..))
import GHC.Word(Word8(..))
import GHC.Types(IO(..))
import System.IO.Unsafe(unsafeDupablePerformIO)

import Data.ByteString.Short as BS

cons :: Word8 -> ShortByteString -> ShortByteString
cons (W8# w) (SBS p) = unsafeDupablePerformIO $ IO $ \s0 ->
  let sz = sizeofByteArray# p in
  case newByteArray# (sz +# 1#) s0      of { (# s1, x #) ->
  case copyByteArray# p 0# x 1# sz s1   of {    s2       ->
  case writeWord8Array# x 0# w s2       of {    s3       ->
  case unsafeFreezeByteArray# x s3      of { (# s4, b #) ->
  (# s4, SBS b #)
  }}}}

tail :: ShortByteString -> Maybe ShortByteString
tail sb@(SBS p) =
  case compare szI 1 of
    LT -> Nothing
    EQ -> Just BS.empty
    GT -> Just $ unsafeDupablePerformIO $ IO $ \s0 ->
          let sz' = sz -# 1# in
          case newByteArray# sz' s0            of { (# s1, x #) ->
          case copyByteArray# p 1# x 0# sz' s1 of { s2          ->
          case unsafeFreezeByteArray# x s2     of { (# s3, b #) ->
          (# s3, SBS b #)
          }}}
  where
  !szI@(I# sz) = BS.length sb


