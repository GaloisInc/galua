{-# Language RecordWildCards #-}
module Galua.Micro.Stepper (runClosure) where

import           Data.Vector(Vector)
import           Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as IOVector
import qualified Data.Map as Map
import           Data.IORef(IORef,newIORef)

import Galua.Value(Value,refLocCaller)
import Galua.Code(FunId,Function(..))
import Galua.Reference(AllocRef)
import Galua.Mach(VM(..),NextStep(..),MachineEnv(..),machRefLoc)
import Galua.Micro.AST(BlockStmt,BlockName(..),functionCode)
import Galua.Micro.OpcodeInterpreter
        (runStmtAt,Frame(..),Next(..),crash,setListReg)

run :: Int -> AllocRef -> Frame -> Vector BlockStmt -> Int -> IO NextStep
run opCodeNum aref frame block pc =
  do next <- runStmtAt aref frame block pc
     case next of
       Continue -> run opCodeNum aref frame block (pc + 1)
       EnterBlock b ->
        case Map.lookup b (ourCode frame) of
          Just newBlock -> run opCodeNum aref frame newBlock 0
          Nothing       -> crash ("Missing block: " ++ show b)

       MakeCall clo args -> return $!
         FunCall clo args Nothing $ \vs ->
           do setListReg frame vs
              run opCodeNum aref frame block (pc + 1)

       MakeTailCall clo args -> return $! FunTailcall clo args
       RaiseError v          -> return $! ThrowError v
       ReturnWith vs         -> return $! FunReturn vs



runClosure :: VM -> FunId -> Function -> IOVector (IORef Value) -> [Value] ->
              IO NextStep
runClosure vm ourFID fun upvals args =
  do let n = funcMaxStackSize fun
     regs       <- IOVector.new n
     regsTMP    <- newIORef Map.empty
     argRegRef  <- newIORef args
     listRegRef <- newIORef []

     let metaTables = machMetatablesRef (vmMachineEnv vm)
         ourCode    = functionCode (funcMicroCode fun)

     ourCaller <- refLocCaller <$> machRefLoc vm

     let frame = Frame { .. }

     run 0 (vmAllocRef vm) frame (ourCode Map.! EntryBlock) 0



