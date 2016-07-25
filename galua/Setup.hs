{-# Language TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import Distribution.Package
  (PackageName(..), PackageIdentifier(..), PackageInstalled)
import Distribution.PackageDescription ( PackageDescription() )
import Distribution.InstalledPackageInfo
  (InstalledPackageInfo,
   libraryDirs, hsLibraries, extraLibraries, sourcePackageId,
   license, copyright, author)
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
import Distribution.License (License(..))
import Data.Char (isAscii)
import Data.Graph (topSort)
import Data.Version (showVersion)
import System.FilePath

import Distribution.Package
import Language.Haskell.TH

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

    let pkgs = orderedPackagesList (installedPkgs lbi)
        libdirs = libdir installDirs : concatMap libraryDirs pkgs
        libNames = thisLib : map threadedVersion (concatMap hsLibraries pkgs)
        mkLibName x
          | fromFlagOrDefault False
             (configProfLib (configFlags lbi)) = x ++ "_p"
          | otherwise = x

    case filter (not . goodLicense . license) pkgs of
      []  -> return ()
      bad -> print bad >> fail "BAD LICENSE"

    -- rewriteFile tries to decode the file to check for changes but only
    -- supports ASCII
    rewriteFile (autodir </> "HS_COPYRIGHTS")
        $ filter isAscii
        $ unlines
        $ map licenseInfoString pkgs

    rewriteFile (autodir </> "HS_LIBRARIES_LIST")
        $ unlines
        $ map mkLibName libNames

    rewriteFile (autodir </> "HS_LIBRARY_PATHS_LIST")
        $ unlines libdirs

    rewriteFile (autodir </> "EXTRA_LIBRARIES_LIST")
        $ unlines
        $ extraLibraries =<< pkgs

orderedPackagesList :: PackageInstalled a => PackageIndex a -> [a]
orderedPackagesList pkgs = lookupVertex <$> topSort g
  where
  (g, lookupVertex, _findVertex) = dependencyGraph pkgs

goodLicense :: License -> Bool
goodLicense BSD3 = True
goodLicense MIT  = True
goodLicense ISC  = True
goodLicense _    = False

licenseInfoString :: InstalledPackageInfo -> String
licenseInfoString pkg = unwords
  [ unPackageName (pkgName (sourcePackageId pkg)) ++
    "-" ++
    showVersion (pkgVersion (sourcePackageId pkg))
  , "-"
  , show (license pkg)
  , "-"
  , copyright pkg
  , "-"
  , author pkg
  ]

-- We needed the threaded run-time so that SIGINT can be handled
-- cleanly when C code has called into Haskell
threadedVersion :: String -> String
threadedVersion lib =
  case lib of
    "Cffi" -> "Cffi_thr"
    "HSrts" -> "HSrts_thr"
    _ -> lib

libPackageKey :: ComponentLocalBuildInfo -> String
libPackageKey x = $(
  do mbSUI <- lookupValueName "SimpleUnitId"
     mbCI  <- lookupValueName "ComponentId"
     mbCUI <- lookupValueName "componentUnitId"
     mbLN  <- lookupValueName "LibraryName"
     mbCL  <- lookupValueName "componentLibraries"
     y     <- newName "y"

     case (mbSUI, mbCI, mbCUI, mbLN, mbCL) of

       (Just sui, Just ci, Just cui, _, _) ->
            [| case $(varE cui) x of
                $(conP sui [conP ci [varP y]]) -> $(varE y) |]

       (_,_,_,Just ln, Just cl) ->
            [| case $(varE cl) x of
                [$(conP ln [varP y])] -> $(varE y)
                _ -> error "libPackageKey: bad componentLibraries" |]

       _ -> fail "Setup.hs, libPackageKey: Unsupported Cabal version"
  )
  -- where
  -- SimpleUnitId (ComponentId y) = componentUnitId x
  -- [LibraryName y] = componentLibraries x


