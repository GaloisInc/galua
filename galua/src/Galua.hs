{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Galua where

import           Galua.Mach( MachConfig(..), MachineEnv(..)
                           , newMachineEnv, threadCPtr)
import           Galua.Stepper (runAllSteps)
import           Galua.Value (Value(Nil))

import           Foreign (Ptr)


setupLuaState :: MachConfig -> IO (Ptr ())
setupLuaState cfg = threadCPtr . machMainThreadRef <$> newMachineEnv cfg


foreign export ccall "galua_newstate" newLuaState :: IO (Ptr ())

newLuaState :: IO (Ptr ())
newLuaState = setupLuaState cfg

  where cfg = MachConfig
                { machOnChunkLoad = \_ _ _ _ -> return ()
                , machOnShutdown  = return () -- TODO: free stable pointers
                , machOnQuery     = \_ -> return Nil
                }
