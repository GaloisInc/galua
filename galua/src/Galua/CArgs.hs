-- | Get command-line parameters out of argc and argv.
module Galua.CArgs where

import Data.List(foldl')
import Foreign
import Foreign.C
import System.Environment
import System.Console.GetOpt

galuaGetArgs ::
  Ptr CInt                  {- ^ OUT: How many arguments are for us. -} ->
  [OptDescr a]              {- ^ format of the options               -} ->
  IO (Either [String] [a])  {- ^ Errors on the Left, OK on the right -}
galuaGetArgs argu opts =
  do args <- getArgs
     case getOpt RequireOrder opts args of
       (as,remaining,[]) ->
          -- invariant: used + length remaining == argc
          do let used = length args - length remaining
             poke argu (fromIntegral used)
             return (Right as)
       (_,_,errs) ->
          do poke argu 0
             return (Left errs)

galuaWithArgs :: Ptr CInt -> [OptDescr (opt -> opt)] -> opt ->
                 b -> (opt -> IO b) -> IO b
galuaWithArgs argu opts defaults onFail onOK =
  do mb <- galuaGetArgs argu opts
     case mb of
       Left _   -> return onFail
       Right as -> onOK (foldl' (\acc opt -> opt acc) defaults as)

