{-# Language RecordWildCards #-}
module Galua.Micro.Stepper (runClosure) where

import           Data.Vector(Vector)
import           Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as IOVector
import qualified Data.Vector as Vector
import qualified Data.Map as Map
import           Data.IORef(IORef,newIORef)

import Galua.Value(Value,refLocCaller)
import Galua.Code(FunId,Function(..))
import Galua.Reference(AllocRef)
import Galua.Mach(VM(..),NextStep(..),MachineEnv(..),machRefLoc)
import Galua.Micro.AST(BlockStmt,BlockName(..),functionCode)
import Galua.Micro.OpcodeInterpreter
        (runStmtAt,Frame(..),Next(..),crash,setListReg)
import qualified Galua.Util.SmallVec as SMV

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
         FunCall clo (SMV.fromList args) Nothing $ \vs ->
           do setListReg frame (SMV.toList vs)
              run opCodeNum aref frame block (pc + 1)

       MakeTailCall clo args -> return $! FunTailcall clo (SMV.fromList args)
       RaiseError v          -> return $! ThrowError v
       ReturnWith vs         -> return $! FunReturn (SMV.fromList vs)



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



