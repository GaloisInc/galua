{-# Language CApiFFI, ForeignFunctionInterface #-}

module HexFloatFormat (doubleToHex) where

import Foreign
import Foreign.C
import Foreign.Marshal.Unsafe

foreign import capi unsafe "stdio.h snprintf"
  snprint_dbl :: CString -> CSize -> CString -> CDouble -> IO CInt

foreign import capi unsafe "stdio.h sprintf"
  sprint_dbl  :: CString ->          CString -> CDouble -> IO CInt

doubleToHex :: Double -> String
doubleToHex x =
  unsafeLocalState $ -- all operations on local allocations
  withCAString "%a" $ \fmt ->

    do let cx = realToFrac x

       -- determine output size (without nul)
       n <- snprint_dbl nullPtr 0 fmt cx

       -- generate the output
       allocaArray (fromIntegral (n+1)) $ \ptr ->
         do _ <- sprint_dbl ptr fmt cx
            peekCAString ptr
