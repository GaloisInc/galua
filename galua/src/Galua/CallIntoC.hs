{-# LANGUAGE ForeignFunctionInterface, BangPatterns #-}
module Galua.CallIntoC (execCFunction, handleCCallState) where

import           Galua.Mach
import           Galua.Value
import           Galua.Overloading

import qualified Galua.Util.SizedVector as SV

import           Control.Concurrent
import           Data.Traversable
import           Data.IORef

execCFunction :: VM -> CFunName -> IO NextStep
execCFunction !vm cfun =
  do let mvar = machCServer (vmMachineEnv vm)
     putMVar mvar (CCallback (cfunAddr cfun))
     return WaitForC

handleCCallState :: VM -> CCallState -> IO NextStep
handleCCallState !vm cResult =
  case cResult of
    CReturned n           -> returnFromC vm n
    CReEntry apiCall impl -> handleGC vm (ApiStart apiCall (impl vm))

handleGC :: VM -> NextStep -> IO NextStep
handleGC vm next =
  do let menv       = vmMachineEnv vm
         garbageRef = machGarbage menv
         tabsRef    = machMetatablesRef menv
     garbage    <- atomicModifyIORef garbageRef (\xs -> ([], xs))
     let collect []       = return next
         collect (v : vs) = m__gc tabsRef (collect vs) v
     collect garbage

-- | Clean up memory allocations during handling of C call, extract results
-- from the stack, and return from the current call frame.
returnFromC ::
  VM ->
  Int  {- ^ Number of values returned from the C call -} ->
  IO NextStep
returnFromC vm n =
  do let stack = execStack (vmCurExecEnv vm)
     FunReturn <$>
       do sz <- SV.size stack
          for [ sz - n .. sz - 1 ] $ \i ->
             readIORef =<< SV.get stack i

