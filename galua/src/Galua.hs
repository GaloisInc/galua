{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Galua where

import           Galua.Mach( VM, MachConfig(..)
                           , MachineEnv(..), newMachineEnv
                           , emptyVM, threadCPtr)
import           Galua.Reference(newAllocRef)
import           Galua.Stepper (runAllSteps)
import           Galua.Value (Value(Nil), referenceVal)

import           Control.Monad(void)

import           Foreign (Ptr)
import           Data.IORef


setupLuaState :: MachConfig -> IO (Ptr ())
setupLuaState cfg =
  do allocref <- newAllocRef

     menv  <- newMachineEnv allocref cfg
     vm    <- emptyVM menv
     writeIORef (machVMRef menv) vm

     return (threadCPtr (machMainThreadRef menv))


foreign export ccall "galua_newstate"
  newLuaState :: IO (Ptr ())

newLuaState :: IO (Ptr ())
newLuaState = setupLuaState cfg

  where cfg = MachConfig
                { machOnChunkLoad = \_ _ _ _ -> return ()
                , machOnShutdown  = return () -- TODO: free stable pointers
                , machOnQuery     = \_ -> return Nil
                }
