{-# LANGUAGE ForeignFunctionInterface, BangPatterns #-}
module Galua.CallIntoC where

import           Galua.Mach
import           Galua.Value
import           Galua.Overloading
import           Galua.CObjInfo

import qualified Galua.Util.SizedVector as SV

import           Control.Concurrent
import           Data.Traversable
import           Data.IORef
import           Foreign.Ptr
import           Foreign.C.Types
import           System.IO
import           Data.Maybe

callC :: VM -> CFun -> IO NextStep
callC vm cfun =
  do let l = threadCPtr (vmCurThread vm)

     getFunInfo <- cfunInfoFun -- :: IO (FunPtr () -> IO CObjInfo)
     objInfo    <- getFunInfo (castFunPtr cfun)
     let name = fromMaybe "unknown" (cObjName objInfo)

     writeIORef (machVMRef (vmMachineEnv vm)) vm
     res <- call_c cfun l

     case res of
       -1 -> return (ThrowError undefined) -- XXX which error??
       -2 -> fail "C functions called from Lua must return non-negative number"
       _ | res < 0 -> fail "Panic: capi_entry had invalid return value"
         | otherwise ->
             do let n     = fromIntegral res
                stack <- execStack <$> getThreadField stExecEnv (vmCurThread vm)
                FunReturn <$>
                  do sz <- SV.size stack
                     for [ sz - n .. sz - 1 ] $ \i ->
                        readIORef =<< SV.get stack i

foreign import ccall "galua_call_c" call_c ::
  CFun -> Ptr () -> IO CInt
