{-# LANGUAGE CPP, OverloadedStrings #-}
module Galua.CObjInfo ( CObjInfo(..), cfunInfoFun, noFunInfo ) where

import Foreign(FunPtr,nullFunPtr)

#if defined (LUA_USE_LINUX)
import Data.Elf
import DWARF.Basics(sections,Endian(..))
import DWARF.Section.Line(File(..))
import DWARF.Addr2Line(Info(..),addr2line)
import Data.List(isPrefixOf)
import System.Posix.Files(readSymbolicLink)
import qualified Data.Map as Map
import qualified Data.Text.Encoding as Text (decodeUtf8)
import qualified Data.Text as Text
import Foreign.Ptr
import qualified Data.ByteString as BS

-- import Control.Monad(guard)
-- import System.Process(readProcess)
#endif

#if defined (LUA_USE_MACOSX)
import Galua.DlInfo(DlInfo(..),funPtrInfo)
#endif



data CObjInfo = CObjInfo
  { cObjAddr  :: !String
  , cObjName  :: !(Maybe String)
  , cObjFile  :: !(Maybe String)
  , cObjLine  :: !(Maybe String)
  } deriving Show

noFunInfo :: FunPtr a -> CObjInfo
noFunInfo fptr = CObjInfo { cObjAddr = addrName fptr
                          , cObjName = Nothing
                          , cObjFile = Nothing
                          , cObjLine = Nothing
                          }

addrName :: FunPtr a -> String
addrName fp = if fp == nullFunPtr then "(entry)" else show fp




cfunInfoFun :: IO (FunPtr () -> IO CObjInfo)
cfunInfoFun =
#if defined ( LUA_USE_LINUX )
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
                   Just b  -> let s = Text.decodeUtf8 b
                              in s `seq` Just (Text.unpack s)
             , cObjFile =
                  case file info of
                    Nothing -> Nothing
                    Just f  -> let s = Text.concat
                                          [ Text.decodeUtf8 (directory f)
                                          , "/"
                                          , Text.decodeUtf8 (fileName f) ]
                               in s `seq` Just (Text.unpack s)
             , cObjLine = case line info of
                            Nothing -> Nothing
                            Just n  -> n `seq` Just (show n)
             }
#elif LUA_USE_MACOSX
  return $ \fptr ->
  do mb <- funPtrInfo fptr
     case mb of
       Nothing -> return (noFunInfo fptr)
       Just i -> return CObjInfo { cObjAddr = addrName fptr
                                 , cObjName = dlinfoSymName i
                                 , cObjFile = Just (dlinfoFileName i)
                                 , cObjLine = Nothing
                                 }
#else
  return $ \_ -> return (noFunInfo fptr)
#endif


