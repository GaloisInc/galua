{-# Language RecordWildCards #-}
module Galua.Micro.Stepper where

import           Data.Vector(Vector)
import           Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as IOVector
import qualified Data.Map as Map
import           Data.IORef(IORef,newIORef)

import Galua.Value(Value,refLocCaller)
import Galua.Mach(TypeMetatables)
import Galua.Code(FunId,Function(..))
import Galua.Reference(AllocRef)
import Galua.Mach(VM,MLuaExecEnv(..),NextStep(..))
import Galua.Micro.AST(BlockStmt,BlockName(..),functionCode)
import Galua.Micro.OpcodeInterpreter
        (runStmtAt,Next(..),crash,setListReg)
import qualified Galua.Util.SmallVec as SMV

run :: VM -> MLuaExecEnv -> Vector BlockStmt -> Int -> IO NextStep
run vm frame block pc =
  do next <- runStmtAt vm frame block pc
     case next of
       Continue -> run vm frame block (pc + 1)
       EnterBlock b ->
        case Map.lookup b (mluaExecCode frame) of
          Just newBlock -> run vm frame newBlock 0
          Nothing       -> crash ("Missing block: " ++ show b)

       MakeCall clo args -> return $!
         FunCall clo (SMV.fromList args) Nothing $ \vs ->
           do setListReg frame (SMV.toList vs)
              run vm frame block (pc + 1)

       MakeTailCall clo args -> return $! FunTailcall clo (SMV.fromList args)
       RaiseError v          -> return $! ThrowError v
       ReturnWith vs         -> return $! FunReturn (SMV.fromList vs)




