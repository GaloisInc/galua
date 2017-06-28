module Galua.Util.IOURef
  ( IOURef
  , newIOURef, readIOURef, writeIOURef, modifyIOURef
  ) where

import Data.Vector.Unboxed.Mutable

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

