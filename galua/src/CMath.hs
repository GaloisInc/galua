{-# LANGUAGE ForeignFunctionInterface #-}

module CMath
  ( libc_strtod
  ) where

import           Foreign.C.Types (CDouble(..))
import           Foreign.C.String (CString, withCString)
import           Foreign.Ptr
import           System.IO.Unsafe (unsafeDupablePerformIO)

foreign import ccall unsafe "stdlib.h strtod" c_strtod
  :: CString -> Ptr CString -> IO CDouble

libc_strtod :: String -> Double
libc_strtod str
  = unsafeDupablePerformIO
  $ withCString str
  $ \ptr -> realToFrac <$> c_strtod ptr nullPtr
