{-# Language CPP #-}
{-# OPTIONS_GHC -Wall #-}

#ifndef MIN_VERSION_Cabal
#define MIN_VERSION_Cabal(x,y,z) 0
#endif

module Main (main) where

import Distribution.PackageDescription ( PackageDescription(..) )
import Distribution.InstalledPackageInfo
import Distribution.Simple ( defaultMainWithHooks, UserHooks(..),
                                simpleUserHooks )
import Distribution.Simple.Utils ( rewriteFile,
                                        createDirectoryIfMissingVerbose )
import Distribution.Simple.BuildPaths ( autogenModulesDir )
import Distribution.Simple.Setup ( ConfigFlags(configVerbosity,configProfLib),
                                      fromFlag, fromFlagOrDefault)
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PackageIndex
import Distribution.Verbosity ( Verbosity )
import System.FilePath

#if MIN_VERSION_Cabal(1,24,0)
import Distribution.Package
#endif

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { postConf = \args flags pkg lbi -> do
     generateBuildModule (fromFlag (configVerbosity flags)) pkg lbi
     postConf simpleUserHooks args flags pkg lbi
  }

-- | Generate a part of a Makefile which contains all libraries and
-- include locations used by the Cabal library.
generateBuildModule ::
  Verbosity -> PackageDescription -> LocalBuildInfo -> IO ()
generateBuildModule verbosity pkgDesc lbi = do
  let autodir = autogenModulesDir lbi
  createDirectoryIfMissingVerbose verbosity True autodir
  let installDirs = absoluteInstallDirs pkgDesc lbi NoCopyDest

  withLibLBI pkgDesc lbi $ \_ libLBI -> do

    let thisLib = libPackageKey libLBI

    let pkgs = allPackages (installedPkgs lbi)
        libdirs = libdir installDirs : concatMap libraryDirs pkgs
        libNames = thisLib : map threadedVersion (concatMap hsLibraries pkgs)
        mkLibName x
          | fromFlagOrDefault False
             (configProfLib (configFlags lbi)) = x ++ "_p"
          | otherwise = x


    rewriteFile (autodir </> "HS_LIBRARIES_LIST")
        $ unlines
        $ map mkLibName libNames

    rewriteFile (autodir </> "HS_LIBRARY_PATHS_LIST")
        $ unlines libdirs

    rewriteFile (autodir </> "EXTRA_LIBRARIES_LIST")
        $ unlines
        $ extraLibraries =<< pkgs

-- We needed the threaded run-time so that SIGINT can be handled
-- cleanly when C code has called into Haskell
threadedVersion :: String -> String
threadedVersion lib =
  case lib of
    "Cffi" -> "Cffi_thr"
    "HSrts" -> "HSrts_thr"
    _ -> lib

libPackageKey :: ComponentLocalBuildInfo -> String
libPackageKey x = y
  where
#if MIN_VERSION_Cabal(1,24,0)
  SimpleUnitId (ComponentId y) = componentUnitId x
#else
  [LibraryName y] = componentLibraries x
#endif
