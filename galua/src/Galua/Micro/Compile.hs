module Galua.Micro.Compile (compile) where


import Data.Typeable       (Typeable)
import Control.Exception   (bracket,Exception(..),throw)
import System.Directory    (removeFile, getTemporaryDirectory)
import System.FilePath     (addExtension, dropExtension)
import System.IO           (hClose, openTempFile)
import Data.Time           (getCurrentTime)
import Text.PrettyPrint(Doc)

import GHC
import Outputable(showSDoc,ppr)
import GHC.Paths(libdir)
import MonadUtils(liftIO)
import StringBuffer(stringToStringBuffer)
import DynFlags
import Unsafe.Coerce(unsafeCoerce)

pack :: String -> PackageFlag
pack f = ExposePackage f (PackageArg f) (ModRenaming True [])

packages :: [ PackageFlag ]
packages = map pack [ "base", "containers", "bytestring", "vector", "galua-rts" ]

pack_dbs :: [ PkgConfRef ]
pack_dbs = map PkgConfFile
  [ ".stack-work/install/x86_64-linux/lts-8.13/8.0.2/pkgdb"
  , "/home/diatchki/.stack/snapshots/x86_64-linux/lts-8.13/8.0.2/pkgdb"
  ]


compile :: Typeable a => String -> Doc -> IO a {- ^ Compile value for `main` -}
compile modNameStr modText' =
  do writeFile (modNameStr ++ ".hs") (show modText')
     bracket (open modNameStr) cleanup $ \file ->
       defaultErrorHandler defaultFatalMessager defaultFlushOut $
       -- runGhc (Just "./galua-ghc") $
       runGhc (Just libdir) $
         do dflags <- getSessionDynFlags

            _      <- setSessionDynFlags dflags
                         { verbosity = 0
                         , ghcLink = LinkInMemory
                         , extraPkgConfs = \_ -> GlobalPkgConf : pack_dbs
                         , packageFlags = packages
                         , optLevel = 2
                         }
            let pp x = showSDoc dflags (ppr x)


            nowish <- liftIO getCurrentTime
            let modName = mkModuleName modNameStr
                modText = show modText'
                target  = Target { targetId = TargetFile file Nothing
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
                   summary <- getModSummary modName
                   intepr <- isModuleInterpreted summary
                   liftIO $ putStrLn $ "INTERP: " ++ show intepr
                   result <- compileExpr (modNameStr ++ ".main")
                   liftIO $ putStrLn "*************COMPILE EXPR***********"
                   return $! unsafeCoerce result


-- | Exception, which is thrown if compilation fails.
data CompileException = CompilationFail | TypeError deriving Show

instance Exception CompileException

{- | Create a temporary file for compilation.
Note that this is just an empty file---the actual contents of the
module to be compiled is passed in memory.
Unfortunately, the current API still requires the existence of a file. -}
open :: String -> IO FilePath
open tempMod =
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


