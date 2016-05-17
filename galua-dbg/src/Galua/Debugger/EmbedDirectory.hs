{-# LANGUAGE TemplateHaskell #-}
module Galua.Debugger.EmbedDirectory where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Directory
import System.FilePath
import Data.List
import qualified Data.ByteString.Char8 as B8

embedDirectory ::
  (FilePath -> Bool) {- ^ predicate for inclusion applied to path -} ->
  FilePath {- ^ base directory to recursively enumerate -} ->
  ExpQ
embedDirectory p root =
  do files <- runIO $
       do starts  <- getDirectoryContents' root
          matches <- traverse (getDirTree root) starts
          return (filter p (concat matches))
     listE
       [ do let fn = root </> file
            bytes <- runIO (B8.readFile fn)
            qAddDependentFile fn
            let content = B8.unpack bytes
            [| (file, content) |]
       | file <- files
       ]

expandDir :: FilePath -> FilePath -> IO [FilePath]
expandDir base dir =
  do kids <- map (dir </>) <$> getDirectoryContents' (base </> dir)
     concat <$> traverse (getDirTree base) kids

getDirTree :: FilePath -> FilePath -> IO [FilePath]
getDirTree base fp =
  do exists <- doesDirectoryExist (base </> fp)
     if exists
       then expandDir base fp
       else return [fp]

getDirectoryContents' :: FilePath -> IO [FilePath]
getDirectoryContents' dir = delete "." . delete ".."
                        <$> getDirectoryContents dir
