{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ForeignFunctionInterface #-}

#define _GNU_SOURCE
#include <dlfcn.h>

module Galua.DlInfo (DlInfo(..), funPtrInfo) where

import Foreign
import Foreign.C

foreign import ccall dladdr :: Ptr a -> Ptr DlInfo -> IO CInt

data DlInfo = DlInfo
  { dlinfoFileName :: String -- ^ Filename of the module
  , dlinfoFileBase :: Ptr () -- ^ Base address of the module
  , dlinfoSymName  :: Maybe String -- ^ name of the nearest symbol
  , dlinfoSymAddr  :: Ptr () -- ^ Address of the nearest symbol
  , dlinfoSymOffset :: Int -- ^ symbol address + offset = original pointer
  }
  deriving Show

-- | Dereference a pointer to a @Dl_info@ struct.
-- The original pointer is used to compute an offset from the sym-base
-- address.
peekDlInfo :: Ptr () {- ^ original pointer -} -> Ptr DlInfo -> IO DlInfo
peekDlInfo original ptr =
  do dlinfoFileName <- peekCString =<< #{peek Dl_info, dli_fname} ptr
     dlinfoFileBase <- #{peek Dl_info, dli_fbase} ptr
     dlinfoSymName  <- maybePeekCString =<< #{peek Dl_info, dli_sname} ptr
     dlinfoSymAddr  <- #{peek Dl_info, dli_saddr} ptr
     let dlinfoSymOffset = minusPtr original dlinfoSymAddr
     return DlInfo{..}

-- | Temporarily allocate memory that can hold a 'DlInfo'
allocDlInfo :: (Ptr DlInfo -> IO a) -> IO a
allocDlInfo = allocaBytes #{size Dl_info}

funPtrInfo :: FunPtr a -> IO (Maybe DlInfo)
funPtrInfo funPtr =
  allocDlInfo $ \dliPtr ->
  do let ptr = castFunPtrToPtr funPtr
     res <- dladdr ptr dliPtr
     if res == 0
       then return Nothing
       else Just <$> peekDlInfo ptr dliPtr

-- | Like 'peekCString' except it returns 'Nothing' for 'nullPtr'
maybePeekCString :: CString -> IO (Maybe String)
maybePeekCString ptr
  | nullPtr == ptr = return Nothing
  | otherwise = Just <$> peekCString ptr
