import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import System.Directory
import Control.Monad.IO.Class

buildDir :: FilePath
buildDir = "_build"

svgToPdf :: FilePath -> FilePath
svgToPdf svg = buildDir </> svg -<.> "pdf"

pdfToSvg :: FilePath -> FilePath
pdfToSvg pdf = dropDirectory1 pdf -<.> "svg"

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=buildDir} $ do

    pwd  <- liftIO getCurrentDirectory

    want ["talk.pdf"]

    phony "clean" $
      do putNormal "Cleaning build files"
         removeFilesAfter buildDir ["//*"]

    "talk.pdf" %> \out ->
       do let src = "talk.md"
          svgs <- getDirectoryFiles "." ["*.svg"]
          need (src : map svgToPdf svgs)
          cmd "pandoc" ["-t", "beamer", "-V", "institute=\"Galois Inc.\"", src, "-o", out]

    buildDir </> "*.pdf" %> \pdf ->
       cmd "inkscape" ["-A", pwd </> pdf, pwd </> pdfToSvg pdf]
