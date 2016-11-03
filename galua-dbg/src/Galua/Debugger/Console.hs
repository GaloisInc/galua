module Galua.Debugger.Console
  ( Line(..)
  , LineType(..)
  , getConsoleLines
  , recordConsoleOutputLine
  , recordConsoleInput
  ) where

import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import System.IO.Unsafe
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
  , lineType  :: !LineType
  }
  deriving (Read, Show)

data LineType = InputLine | OutputLine
                deriving (Read, Show)

data Console = Console
  { consoleLines :: !(Seq Line)
  , consoleBuffer :: !Text
    -- ^ This is the last (incomplete) line that was printed
  }

emptyConsole :: Console
emptyConsole = Console
  { consoleLines = Seq.empty
  , consoleBuffer = Text.empty
  }

incLine :: Line -> Line
incLine l = l { lineCount = lineCount l + 1 }

newLine :: LineType -> Text -> Line
newLine t txt = Line{ lineCount = 1, lineBody = txt, lineType = t }

consoleBufferRef :: IORef Console
consoleBufferRef = unsafePerformIO (newIORef emptyConsole)
{-# NOINLINE consoleBufferRef #-}

consoleBufferSize :: Int
consoleBufferSize = 10

getConsoleLines :: IO (Seq Line)
getConsoleLines = consoleLines <$> readIORef consoleBufferRef


recordConsoleInput :: Text -> IO ()
recordConsoleInput txt =
  atomicModifyIORef' consoleBufferRef $ \con ->
    ( con { consoleLines = foldr addLine (consoleLines con) (Text.lines txt) }
    , ()
    )
  where
  addLine l s = s Seq.|>
                Line { lineCount = 1, lineBody = l, lineType = InputLine }

recordConsoleOutputLine :: Seq Line -> Text -> Seq Line
recordConsoleOutputLine buf txt =

  let snoc' xs x = x `seq` xs Seq.|> x in

  case Seq.viewr buf of
    ls Seq.:> l
      | lineBody l == txt ->
          snoc' ls (incLine l)

    _ | Seq.length buf < consoleBufferSize ->
          snoc' buf (newLine OutputLine txt)

    _ -> snoc' (Seq.drop 1 buf) (newLine OutputLine txt)

recordConsoleOutput :: Text -> Console -> Console
recordConsoleOutput txt console = Console
  { consoleLines = foldl' recordConsoleOutputLine
                                (consoleLines console) completeLines
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
       (recordConsoleOutput txt console, ())
     return l
