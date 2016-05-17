module Galua.Debugger.Console
  ( Line(..)
  , getConsoleLines
  , recordConsoleLine
  ) where

import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import System.IO.Unsafe
import System.IO
import Data.IORef
import Data.List
import Data.Text (Text)
import Data.Monoid
import qualified Data.Text as Text
import qualified Data.Text.Foreign as Text
import Foreign.C

data Line = Line
  { lineCount :: !Int
  , lineBody  :: !Text
  }
  deriving (Read, Show)

data Console = Console
  { consoleLines :: !(Seq Line)
  , consoleBuffer :: !Text
  }

emptyConsole = Console
  { consoleLines = Seq.empty
  , consoleBuffer = Text.empty
  }

incLine :: Line -> Line
incLine l = l { lineCount = lineCount l + 1 }

newLine :: Text -> Line
newLine txt = Line{ lineCount = 1, lineBody = txt }

consoleBufferRef :: IORef Console
consoleBufferRef = unsafePerformIO (newIORef emptyConsole)
{-# NOINLINE consoleBufferRef #-}

consoleBufferSize :: Int
consoleBufferSize = 10

getConsoleLines :: IO (Seq Line)
getConsoleLines = consoleLines <$> readIORef consoleBufferRef

recordConsoleLine :: Seq Line -> Text -> Seq Line
recordConsoleLine buf txt =

  let snoc' xs x = x `seq` xs Seq.|> x in

  case Seq.viewr buf of
    ls Seq.:> l
      | lineBody l == txt ->
          snoc' ls (incLine l)

    _ | Seq.length buf < consoleBufferSize ->
          snoc' buf (newLine txt)

    _ -> snoc' (Seq.drop 1 buf) (newLine txt)

recordConsoleInput :: Text -> Console -> Console
recordConsoleInput txt console = Console
  { consoleLines = foldl' recordConsoleLine (consoleLines console) completeLines
  , consoleBuffer = incompleteLine
  }
  where
  txt'     = consoleBuffer console <> txt
  txtLines = Text.split (=='\n') txt'
  linesN   = length txtLines
  (completeLines, [incompleteLine]) = splitAt (linesN-1) txtLines

foreign export ccall galua_writestring_dbg :: CString -> CSize -> IO CSize
galua_writestring_dbg :: CString -> CSize -> IO CSize
galua_writestring_dbg s l =
  do txt <- Text.peekCStringLen (s, fromIntegral l)
     atomicModifyIORef' consoleBufferRef $ \console ->
       (recordConsoleInput txt console, ())
     return l
