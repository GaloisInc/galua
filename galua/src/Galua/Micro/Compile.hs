module Galua.Micro.Compile (compile) where


import Data.Typeable       (Typeable)
import Control.Exception   (bracket,Exception(..),throw)
import System.Directory    (removeFile, getTemporaryDirectory)
import System.FilePath     (addExtension, dropExtension)
import System.IO           (hClose, openTempFile)
import Data.Time           (getCurrentTime)
import Text.PrettyPrint(Doc)

import GHC
import GHC.Paths(libdir)
import MonadUtils(liftIO)
import StringBuffer(stringToStringBuffer)
import DynFlags
import Unsafe.Coerce(unsafeCoerce)

tempMod :: String
tempMod = "TEMP_JIT"

pack :: String -> PackageFlag
pack f = ExposePackage f (PackageArg f) (ModRenaming True [])

packages :: [ PackageFlag ]
packages = map pack [ "base", "containers", "vector", "galua-rts" ]

pack_dbs :: [ PkgConfRef ]
pack_dbs = map PkgConfFile
  [ ".stack-work/install/x86_64-linux/lts-8.13/8.0.2/pkgdb"
  , "/home/diatchki/.stack/snapshots/x86_64-linux/lts-8.13/8.0.2/pkgdb"
  ]


compile :: Typeable a => Doc -> IO a {- ^ Compile value for `main` -}
compile modText' =
  do writeFile "TEMP_JIT.hs" (show modText')
     bracket open cleanup $ \file ->
       defaultErrorHandler defaultFatalMessager defaultFlushOut $
       -- runGhc (Just "./galua-ghc") $
       runGhc (Just libdir) $
         do dflags <- getSessionDynFlags

            _      <- setSessionDynFlags dflags
                         { verbosity = 0
                         , extraPkgConfs = \_ -> GlobalPkgConf : pack_dbs
                         , packageFlags = packages
                         , optLevel = 2
                         }


            nowish <- liftIO getCurrentTime
            let modName = mkModuleName tempMod
                -- modText = unlines $ ("module " ++ tempMod ++ "(main) where") : ls
                modText = show modText'
                target = Target { targetId = TargetFile file Nothing
                                , targetAllowObjCode = True
                                , targetContents =
                                     Just (stringToStringBuffer modText, nowish)
                                }

            setTargets [target]
            r <- load LoadAllTargets
            liftIO $ putStrLn "*************COMPILING********************"
            case r of
              Failed    -> throw CompilationFail
              Succeeded ->
                do liftIO $ putStrLn "*************SET CONTEXT*****************"
                   setContext [IIDecl (simpleImportDecl modName)]
                   result <- compileExpr "TEMP_JIT.main"
                   liftIO $ putStrLn "*************COMPILE EXPR***********"
                   return $! unsafeCoerce result


-- | Exception, which is thrown if compilation fails.
data CompileException = CompilationFail | TypeError deriving Show

instance Exception CompileException

{- | Create a temporary file for compilation.
Note that this is just an empty file---the actual contents of the
module to be compiled is passed in memory.
Unfortunately, the current API still requires the existence of a file. -}
open :: IO FilePath
open =
  do tmp <- getTemporaryDirectory
     (file, h) <- openTempFile tmp (addExtension tempMod "hs")
     hClose h
     return file


-- | Delete the temporay file and relate .hi and .o files generated
-- during compilation.
cleanup :: FilePath -> IO ()
cleanup path =
  do removeFile path
     let root = dropExtension path
     removeFile (addExtension root ".hi")
     removeFile (addExtension root ".o")


