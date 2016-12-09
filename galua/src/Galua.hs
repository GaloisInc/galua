{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Galua where

import           Galua.Mach( VM, MachConfig(..)
                           , MachineEnv(..), newMachineEnv, NextStep(WaitForC)
                           , emptyVM, threadCPtr)
import           Galua.Reference(newAllocRef)
import           Galua.Stepper (runAllSteps)
import           Galua.Value (Value(Nil), referenceVal)

import           Control.Monad(void)

import           Foreign (Ptr)
import           Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import           Control.Concurrent (forkIO)




setupLuaState :: MachConfig -> IO (Ptr (), VM, NextStep)
setupLuaState cfg =
  do allocref <- newAllocRef
     menv <- newMachineEnv allocref cfg

     vm <- emptyVM allocref menv
     let cptr = unsafeForeignPtrToPtr
              $ threadCPtr $ referenceVal $ machMainThreadRef menv
     return (cptr, vm, WaitForC)


foreign export ccall "galua_newstate"
  newLuaState :: IO (Ptr ())

newLuaState :: IO (Ptr ())
newLuaState =
  do (cptr, vm, next) <- setupLuaState cfg
     _  <- forkIO $ void $ runAllSteps vm next
     return cptr

  where cfg = MachConfig
                { machOnChunkLoad = \_ _ _ _ -> return ()
                , machOnShutdown  = return () -- TODO: free stable pointers
                , machOnQuery     = \_ -> return Nil
                }
