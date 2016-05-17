{-# LANGUAGE CPP #-}
module Galua.CObjInfo ( CObjInfo(..), getCFunInfo, noFunInfo ) where

import Foreign(FunPtr,nullFunPtr)

#if defined (LUA_USE_LINUX)
import Control.Monad(guard)
import System.Posix.Files(readSymbolicLink)
import System.Process(readProcess)
#endif

#if defined (LUA_USE_MACOSX)
import Galua.DlInfo(DlInfo(..),funPtrInfo)
#endif



data CObjInfo = CObjInfo
  { cObjAddr  :: String
  , cObjName  :: Maybe String
  , cObjFile  :: Maybe String
  , cObjLine  :: Maybe String
  }

noFunInfo :: FunPtr a -> CObjInfo
noFunInfo fptr = CObjInfo { cObjAddr = addrName fptr
                          , cObjName = Nothing
                          , cObjFile = Nothing
                          , cObjLine = Nothing
                          }

addrName :: FunPtr a -> String
addrName fp = if fp == nullFunPtr then "(entry)" else show fp






getCFunInfo :: FunPtr a -> IO CObjInfo
-- defined ( LUA_USE_LINUX )
#if 0
getCFunInfo fptr =
  do exe <- readSymbolicLink "/proc/self/exe"
     txt <- readProcess "addr2line" ["-p", "-f", "-e", exe, show fptr ] ""
     case words txt of
       [ a, "at", loc ]
         | (fi , _ : li) <- break (== ':') loc ->
            return CObjInfo { cObjAddr = addrName fptr
                            , cObjName = known a
                            , cObjFile = known fi
                            , cObjLine = known li
                            }

       a : _ -> return CObjInfo { cObjAddr = addrName fptr
                                , cObjName = known a
                                , cObjFile = Nothing
                                , cObjLine = Nothing
                                }

       [] -> return (noFunInfo fptr)
  where
  known x = guard (take 1 x /= "?") >> return x

#elif defined ( LUA_USE_MACOSX )
getCFunInfo fptr =
  do mb <- funPtrInfo fptr
     case mb of
       Nothing -> return (noFunInfo fptr)
       Just i -> return CObjInfo { cObjAddr = addrName fptr
                                 , cObjName = dlinfoSymName i
                                 , cObjFile = Just (dlinfoFileName i)
                                 , cObjLine = Nothing
                                 }
#else
getCFunInfo fptr = return (noFunInfo fptr)
#endif
