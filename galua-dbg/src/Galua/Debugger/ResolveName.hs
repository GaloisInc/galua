{-# Language NamedFieldPuns, OverloadedStrings #-}
module Galua.Debugger.ResolveName where

import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as IOVector
import qualified Data.Map as Map
import           Data.Text (Text)
import           Data.Text.Read(decimal)
import qualified Data.Text as Text
import           Data.IORef

import Galua.FunValue(luaOpCodes)
import Galua.Value
import Galua.Reference
import Galua.Mach
import Galua.Names.Eval
import Galua.Code

import Galua.Debugger.Exportable


-- | Identifies an execution environment.
-- This is used when we resolve name references.
data ExecEnvId = StackFrameExecEnv !Integer
                 -- ^ Exportable id of a stack frame

               | ThreadExecEnv !Int
                 -- ^ Reference id of a thread object

               | ClosureEnvId !Int
                 -- ^ Reference id of a closure object

                 deriving Show

exportExecEnvId :: ExecEnvId -> Text
exportExecEnvId eid =
  Text.pack $
  case eid of
    StackFrameExecEnv n -> "s_" ++ show n
    ThreadExecEnv n     -> "t_" ++ show n
    ClosureEnvId n      -> "c_" ++ show n

importExecEnvId :: Text -> Maybe ExecEnvId
importExecEnvId txt =
  case Text.splitAt 2 txt of
    ("s_", s) | Just n <- num s -> Just (StackFrameExecEnv n)
    ("t_", s) | Just n <- num s -> Just (ThreadExecEnv n)
    ("c_", s) | Just n <- num s -> Just (ClosureEnvId n)
    _                           -> Nothing
  where
  num s = case decimal s of
            Right (a,"") -> Just a
            _            -> Nothing

findNameResolveEnv ::
  IORef ExportableState -> VM -> ExecEnvId -> IO (FunId, NameResolveEnv)
findNameResolveEnv expS vm eid =
  do metaTabs <- readIORef $ machMetatablesRef $ vmMachineEnv vm
     case eid of
       StackFrameExecEnv sid ->
         do ExportableState { expClosed } <- readIORef expS
            case Map.lookup sid expClosed of
              Just (ExportableStackFrame _ env) ->
                execEnvToNameResolveEnv metaTabs env
              _ -> nameResolveException ("Invalid stack frame: " ++ show sid)

       ThreadExecEnv tid ->
         do mb <- lookupRef (vmAllocRef vm) tid
            case mb of
              Nothing  -> nameResolveException "Invalid thread."
              Just ref ->
                do eenv <- getThreadField stExecEnv ref
                   execEnvToNameResolveEnv metaTabs eenv

       ClosureEnvId cid ->
         do mb <- lookupRef (vmAllocRef vm) cid
            case mb of
              Nothing  -> nameResolveException "Invalid closure."
              Just ref ->
                do let closure = referenceVal ref
                   closureToResolveEnv metaTabs closure

closureToResolveEnv ::
  TypeMetatables -> Closure -> IO (FunId, NameResolveEnv)
closureToResolveEnv metaTabs c =
  do (fid,func) <- case luaOpCodes (cloFun c) of
                     Just (fid,func) -> return (fid,func)
                     _ -> nameResolveException "Not in a Lua function."
     let sz = funcMaxStackSize func
     stack <- IOVector.replicateM sz (newIORef Nil)
     ups <- Vector.freeze (cloUpvalues c)
     let nre = NameResolveEnv
                 { nrUpvals   = ups
                 , nrStack    = stack
                 , nrFunction = func
                 , nrMetas    = metaTabs
                 }
     return (fid, nre)

execEnvToNameResolveEnv ::
  TypeMetatables -> ExecEnv -> IO (FunId, NameResolveEnv)
execEnvToNameResolveEnv metaTabs eenv =
  case eenv of
    ExecInLua lenv ->
     do ups <- Vector.freeze (luaExecUpvals lenv)
        let nre = NameResolveEnv
                    { nrUpvals   = ups
                    , nrStack    = luaExecRegs lenv
                    , nrFunction = luaExecFunction lenv
                    , nrMetas    = metaTabs
                    }

        nre `seq` return (luaExecFID lenv, nre)
    ExecInC {} -> nameResolveException "Not in a Lua function."


