{-# Language NamedFieldPuns #-}
module Galua.Micro.Stepper where

import           Data.Vector(Vector)
import qualified Data.Vector.Mutable as IOVector
import qualified Data.Map as Map
import           Data.IORef(newIORef)

import Galua.Code(Function(..))
import Galua.Micro.ExecEnv(MLuaExecEnv(..))
import Galua.Mach(VM,NextStep(..))
import Galua.FunValue(FunctionValue(..))
import Galua.Value(Value(..), Reference, Closure(..),referenceVal)
import Galua.Micro.AST(BlockStmt,BlockName(..),MicroFunction(..))
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


funcMicroCode :: Function -> MicroFunction
funcMicroCode = undefined "XXX: Use Translate here"

-- | Enter a closure using the MicroLua way
useMicroLua :: VM ->
               Reference Closure ->
               SMV.SmallVec Value ->
               IO (MLuaExecEnv, IO NextStep)
useMicroLua vm c vs =
  do let MkClosure { cloFun, cloUpvalues } = referenceVal c
     case cloFun of
       CFunction {} -> error "This is C code, not Lua!"
       LuaFunction fid f ->
         do let micro = funcMicroCode f

            regsVal <- IOVector.new (funcMaxStackSize f)
            regsRef <- IOVector.new (funcMaxStackSize f)
            regsTMP <- IOVector.new (functionRegsTMP micro)

            argRef  <- newIORef (SMV.toList vs)
            listRef <- newIORef []

            let code = functionCode micro
                entry = code Map.! EntryBlock
                eenv = MLuaExecEnv
                         { mluaExecRegsValue = regsVal
                         , mluaExecRegsRefs  = regsRef
                         , mluaExecRegsTMP   = regsTMP
                         , mluaExecArgReg    = argRef
                         , mluaExecListReg   = listRef
                         , mluaExecUpvals    = cloUpvalues
                         , mluaExecCode      = code
                         , mluaExecClosure   = Closure c
                         , mluaExecFID       = fid
                         , mluaExecFunction  = f
                         }

                start = run vm eenv entry 0

            return (eenv, start)


