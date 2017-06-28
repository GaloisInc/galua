{-# LANGUAGE CPP, OverloadedStrings #-}
module Galua.CObjInfo ( CObjInfo(..), cfunInfoFun, noFunInfo ) where

import Foreign(FunPtr,nullFunPtr)
import Data.Text(Text)
import qualified Data.Text as Text

#if defined (LUA_USE_LINUX)
import Data.Elf
import DWARF.Basics(sections,Endian(..))
import DWARF.Section.Line(File(..))
import DWARF.Addr2Line(Info(..),addr2line)
import Data.List(isPrefixOf)
import System.Posix.Files(readSymbolicLink)
import qualified Data.Map as Map
import qualified Data.Text.Encoding as Text (decodeUtf8)
import Foreign.Ptr
import qualified Data.ByteString as BS

-- import Control.Monad(guard)
-- import System.Process(readProcess)
#endif

#if defined (LUA_USE_MACOSX)
import Galua.DlInfo(DlInfo(..),funPtrInfo)
#endif



data CObjInfo = CObjInfo
  { cObjAddr  :: !Text
  , cObjName  :: !(Maybe Text)
  , cObjFile  :: !(Maybe Text)
  , cObjLine  :: !(Maybe Text)
  } deriving Show

noFunInfo :: FunPtr a -> CObjInfo
noFunInfo fptr = CObjInfo { cObjAddr = addrName fptr
                          , cObjName = Nothing
                          , cObjFile = Nothing
                          , cObjLine = Nothing
                          }

addrName :: FunPtr a -> Text
addrName fp = if fp == nullFunPtr then "(entry)" else Text.pack (show fp)




cfunInfoFun :: IO (FunPtr () -> IO CObjInfo)
cfunInfoFun =
#if defined ( LUA_USE_LINUX )
--  return $ \fptr -> return (noFunInfo fptr)
--{-
  do exe   <- readSymbolicLink "/proc/self/exe"
     bytes <- BS.readFile exe
     let elf = parseElf bytes
         end = case elfData elf of
                 ELFDATA2LSB -> LittleEndian
                 ELFDATA2MSB -> BigEndian
         secs = sections end $ Map.fromList [ (name, elfSectionData s)
                                            | s <- elfSections elf
                                            , let name = elfSectionName s
                                            , ".debug_" `isPrefixOf` name ]
     return $ \fptr -> return $!
        let addr = fromIntegral (ptrToIntPtr (castFunPtrToPtr fptr))
            info = addr2line secs addr
        in CObjInfo
             { cObjAddr = addrName fptr
             , cObjName =
                 case function info of
                   Nothing -> Nothing
                   Just b  -> Just $! Text.decodeUtf8 b
             , cObjFile =
                  case file info of
                    Nothing -> Nothing
                    Just f  -> Just $! Text.concat
                                          [ Text.decodeUtf8 (directory f)
                                          , "/"
                                          , Text.decodeUtf8 (fileName f) ]
             , cObjLine = case line info of
                            Nothing -> Nothing
                            Just n  -> Just $! Text.pack (show n)
             }
---}
#elif LUA_USE_MACOSX
  return $ \fptr ->
  do mb <- funPtrInfo fptr
     case mb of
       Nothing -> return (noFunInfo fptr)
       Just i -> return $!
        CObjInfo { cObjAddr = addrName fptr
                 , cObjName = case dlinfoSymName i of
                                Nothing -> Nothing
                                Just s  -> Just $! Text.pack s
                 , cObjFile = Just $!  (Text.pack (dlinfoFileName i))
                 , cObjLine = Nothing
                 }
#else
  return $ \fptr -> return (noFunInfo fptr)
#endif


