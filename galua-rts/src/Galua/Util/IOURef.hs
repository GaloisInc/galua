{-# Language MagicHash, UnboxedTuples #-}
module Galua.Util.IOURef
  ( IOURef, newIOURef, readIOURef, writeIOURef, modifyIOURef
  , IOWordRef, newIOWordRef, readIOWordRef, writeIOWordRef
  ) where

import Data.Vector.Unboxed.Mutable
import Data.Word(Word)

import GHC.Prim
import GHC.Types

data IOWordRef = IOWordRef (MutableByteArray# RealWorld)

newIOWordRef :: Word -> IO IOWordRef
newIOWordRef (W# w) =
  IO (\s -> case newByteArray# 8# s          of { (# s1, arr #) ->
            case writeWordArray# arr 0# w s1 of { s2            ->
            (# s2, IOWordRef arr #)
            }})

readIOWordRef :: IOWordRef -> IO Word
readIOWordRef (IOWordRef arr) =
  IO (\s -> case readWordArray# arr 0# s of { (# s1, w #) ->
            (# s1, W# w #)
            })
{-# INLINE readIOWordRef #-}

writeIOWordRef :: IOWordRef -> Word -> IO ()
writeIOWordRef (IOWordRef arr) (W# w) =
  IO (\s -> case writeWordArray# arr 0# w s of { s1 ->
            (# s1, () #)
            })
{-# INLINE writeIOWordRef #-}





newtype IOURef a = IOURef (IOVector a)

{-# INLINE newIOURef #-}
newIOURef :: Unbox a => a -> IO (IOURef a)
newIOURef a = do arr <- unsafeNew 1
                 unsafeWrite arr 0 a
                 return (IOURef arr)

{-# INLINE readIOURef #-}
readIOURef :: Unbox a => IOURef a -> IO a
readIOURef (IOURef a) = unsafeRead a 0

{-# INLINE writeIOURef #-}
writeIOURef :: Unbox a => IOURef a -> a -> IO ()
writeIOURef (IOURef a) x = unsafeWrite a 0 x

{-# INLINE modifyIOURef #-}
modifyIOURef :: Unbox a => IOURef a -> (a -> a) -> IO ()
modifyIOURef r f = do a <- readIOURef r
                      writeIOURef r (f a)

