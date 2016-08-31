{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Galua where

import           Galua.Mach( VM, MachConfig(..)
                           , MachineEnv(..), newMachineEnv, NextStep
                           , runMach, emptyVM, threadCPtr)
import           Galua.Reference(AllocRef, runAllocWith, exposeAllocRef,
                                  runAlloc, readRef)
import           Galua.Stepper (runAllSteps)
import           Galua.CallIntoC (cNextStepLoop)

import           Control.Monad(void)

import           Foreign (Ptr)
import           Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import           Control.Concurrent (forkIO)




setupLuaState :: MachConfig -> IO (Ptr (), AllocRef, VM, NextStep)
setupLuaState cfg =
  do (menv,allocref) <- runAlloc $
        do menv <- newMachineEnv cfg
           allocref <- exposeAllocRef
           return (menv,allocref)

     let vm   = emptyVM menv
         next = runMach vm cNextStepLoop

     mainThread <- readRef (machMainThreadRef menv)
     let cptr = unsafeForeignPtrToPtr (threadCPtr mainThread)
     return (cptr, allocref, vm, next)


foreign export ccall "galua_newstate"
  newLuaState :: IO (Ptr ())

newLuaState :: IO (Ptr ())
newLuaState =
  do (cptr, allocref, vm, next) <- setupLuaState cfg
     _  <- forkIO $ void $ runAllocWith allocref $ runAllSteps vm next
     return cptr

  where cfg = MachConfig
                { machOnChunkLoad     = \_ _ _ _ -> return ()
                , machOnShutdown = return () -- TODO: free stable pointers
                }
