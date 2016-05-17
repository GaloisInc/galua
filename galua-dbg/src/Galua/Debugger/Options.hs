{-# LANGUAGE NamedFieldPuns #-}
module Galua.Debugger.Options where

import System.Console.GetOpt
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Read(readMaybe)

data Options = Options
  { optPrintHelp    :: Bool
  , optBreakPoints  :: CommandLineBreakPoints
  }

-- Map a file, to a tree of functions that need break points
type CommandLineBreakPoints = Map FilePath [Int]

defaultOptions :: Options
defaultOptions = Options
  { optPrintHelp   = False
  , optBreakPoints = Map.singleton "" [0]
  }


addOpt :: Either String (Options -> Options) ->
          Either [String] Options -> Either [String] Options
addOpt thing mb =
  case (mb,thing) of
    (Left errs, Left err) -> Left (errs ++ [err])
    (_, Left err)         -> Left [err]
    (Left errs, Right _)  -> Left errs
    (Right o, Right f)    -> Right (f o)


options :: [ OptDescr (Either [String] Options -> Either [String] Options) ]
options =
  [ Option ['h','?'] ["help"]
    (NoArg $ \_ -> Right defaultOptions { optPrintHelp = True })
    "Show the list of options on stderr"

  , Option ['b'] ["break"]
    (flip ReqArg "FILE:LINE" $ \s ->
      addOpt $
      case parseOptionBreak s of
        Nothing -> Left "Invalid break-point"
        Just OptionBreak { optBrkFile, optBrkLine } ->
          Right $ \o -> o { optBreakPoints =
                              Map.insertWith (++) optBrkFile [optBrkLine]
                                                            (optBreakPoints o) }
    )
    "Add a breakpoint."
  ]


data OptionBreak = OptionBreak
  { optBrkFile :: FilePath
  , optBrkLine :: Int
  }

parseOptionBreak :: String -> Maybe OptionBreak
parseOptionBreak txt =
  case break (== ':') txt of
    (f,_:num) -> do n <- readMaybe num
                    return OptionBreak { optBrkFile = f, optBrkLine = n }
    (f,"")    -> return OptionBreak { optBrkFile = f, optBrkLine = 0 }




