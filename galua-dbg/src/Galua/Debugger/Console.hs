module Galua.Debugger.Console
  ( Line(..)
  , LineType(..)
  , LineWord(..)
  , getConsoleLines
  , recordConsoleOutputLine
  , recordConsoleInput
  , recordConsoleValues
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

import Galua.Value(Value)

data Line = Line
  { lineCount :: !Int
  , lineBody  :: ![LineWord]
  , lineType  :: !LineType
  }

data LineWord = TextWord Text | ValueWord Value
                  deriving Eq

data LineType = InputLine | OutputLine

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


splitWord :: Text -> [LineWord]
splitWord = map TextWord . Text.split (=='\t')



newLine :: Text -> Line
newLine txt = Line { lineCount = 1
                   , lineBody = splitWord txt
                   , lineType = OutputLine  }

consoleBufferRef :: IORef Console
consoleBufferRef = unsafePerformIO (newIORef emptyConsole)
{-# NOINLINE consoleBufferRef #-}

consoleBufferSize :: Int
consoleBufferSize = 10

getConsoleLines :: IO (Seq Line)
getConsoleLines = consoleLines <$> readIORef consoleBufferRef

recordConsoleLine :: Seq Line -> Line -> Seq Line
recordConsoleLine buf newL =

  let snoc' xs x = x `seq` xs Seq.|> x in

  case Seq.viewr buf of
    ls Seq.:> l
      | lineBody l == lineBody newL -> snoc' ls (incLine l)

    _ | Seq.length buf < consoleBufferSize -> snoc' buf newL

    _ -> snoc' (Seq.drop 1 buf) newL

recordConsoleOutputLine :: Seq Line -> Text -> Seq Line
recordConsoleOutputLine buf txt = recordConsoleLine buf (newLine txt)

recordConsoleValues :: [Value] -> IO ()
recordConsoleValues vs = atomicModifyIORef' consoleBufferRef $ \con ->
  ( con { consoleLines = recordConsoleLine (consoleLines con) newL }
  , ()
  )
  where
  newL = Line { lineCount = 1
              , lineType  = OutputLine
              , lineBody  = map ValueWord vs
              }

recordConsoleInput :: Text -> IO ()
recordConsoleInput txt =
  atomicModifyIORef' consoleBufferRef $ \con ->
    ( con { consoleLines = foldr addLine (consoleLines con) (Text.lines txt) }
    , ()
    )
  where
  addLine l s = recordConsoleLine s Line { lineCount = 1
                                         , lineBody = [TextWord l]
                                         , lineType = InputLine
                                         }





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
