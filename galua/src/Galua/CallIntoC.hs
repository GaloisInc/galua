{-# LANGUAGE ForeignFunctionInterface #-}
module Galua.CallIntoC (execCFunction, handleCCallState) where

import           Galua.Mach
import           Galua.Value
import           Galua.Overloading
import           Galua.CObjInfo

import qualified Galua.Util.SizedVector as SV

import           Control.Concurrent
import           Data.Traversable
import           Control.Monad.IO.Class
import           Data.Foldable
import           Data.IORef

execCFunction :: CFunName -> Mach a
execCFunction cfun =
  do mvar <- getsMachEnv machCServer
     liftIO (putMVar mvar (CCallback (cfunAddr cfun)))
     machWaitForC

handleCCallState :: CCallState -> Mach a
handleCCallState cResult =
  case cResult of
    CReturned n -> returnFromC n
    CReEntry label returnAddress primargs k ->
      do handleGC
         reentryFromC label returnAddress primargs k
         machWaitForC

handleGC :: Mach ()
handleGC =
  do garbageRef <- getsMachEnv machGarbage
     garbage    <- liftIO (atomicModifyIORef garbageRef (\xs -> ([], xs)))
     traverse_ gcValue garbage

gcValue :: Value -> Mach ()
gcValue val =
  do metamethod <- valueMetamethod val "__gc"
     case metamethod of
        Nil -> return ()
        _   -> () <$ callValue metamethod [val]

reentryFromC ::
  String         {- ^ name of entry point      -} ->
  CObjInfo       {- ^ return address           -} ->
  [PrimArgument] {- ^ arguments at entry point -} ->
  Mach ()        {- ^ code to run              -} ->
  Mach ()
reentryFromC label returnAddress primargs k =
  do let apiCall = ApiCall
           { apiCallMethod = label
           , apiCallReturn = returnAddress
           , apiCallArgs = primargs
           }
     machApiCall apiCall k



-- | Clean up memory allocations during handling of C call, extract results
-- from the stack, and return from the current call frame.
returnFromC ::
  Int  {- ^ Number of values returned from the C call -} ->
  Mach a
returnFromC n =
  do eenv  <- getsExecEnv id

     rs <- liftIO $
       do sz <- SV.size (execStack eenv)
          for [ sz - n .. sz - 1 ] $ \i ->
            readIORef =<< SV.get (execStack eenv) i

     machReturn rs

