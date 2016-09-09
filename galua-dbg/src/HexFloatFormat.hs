{-# Language CApiFFI, ForeignFunctionInterface #-}

module HexFloatFormat (doubleToHex) where

import Foreign
import Foreign.C
import Foreign.Marshal.Unsafe

foreign import capi unsafe "stdio.h snprintf"
  snprintf_dbl :: CString -> CSize -> CString -> CDouble -> IO CInt

doubleToHex :: Double -> String
doubleToHex x =
  unsafeLocalState $ -- all operations on local allocations
  withCAString "%a" $ \fmt ->

    do let cx = realToFrac x

       -- determine output size
       n <- snprintf_dbl nullPtr 0 fmt cx
       let n1 = n+1 -- with nul

       -- generate the output
       allocaArray (fromIntegral n1) $ \ptr ->
         do _ <- snprintf_dbl ptr (fromIntegral n1) fmt cx
            peekCAString ptr
