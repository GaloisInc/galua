module Galua.Debugger.Infer where

import           Data.Map(Map)
import qualified Data.Map as Map
import           Data.ByteString(ByteString)
import           Data.IORef(readIORef)

import Galua.Mach
import Galua.Code
import qualified Galua.Micro.AST as Micro
import Galua.Micro.AST(BlockName,blockNamePC)
import qualified Galua.Micro.Type.Value as Ty
import Galua.Micro.Translate.InferTypes
import Galua.Debugger.Source



inferFun :: Chunks {- ^ Loaded modules -} ->
            VM     {- ^ Current state of the debugger -} ->
            FunId  {- ^ Function to analyze -} ->
            IO (Function, Map Int (Map Reg (Maybe ByteString, Ty.Value)))
              -- ^ Function, and mapping from PC to Register to (name,type)
inferFun chunks vm fid =
  case lookupFun chunks fid of
    Nothing -> fail "Unknwon function ID"
    Just fun ->
      do let menv  = vmMachineEnv vm
             env   = machGlobals menv
         metas <- readIORef (machMetatablesRef menv)
         tys <- inferFunTypes (allMicroFuns chunks) metas env fid
         let pcMap = extractInfo tys
             lkpName pc reg v = (lookupLocalName fun pc reg, v)
             addNames pc mp   = Map.mapWithKey (lkpName pc) mp
         return (fun, Map.mapWithKey addNames pcMap)


extractInfo :: Map BlockName Ty.LocalState ->
               Map Int (Map Reg Ty.Value) -- ^ For each PC, types of all regs
extractInfo = Map.fromListWith jnRegs . map cvt . Map.toList
  where
  cvt (x,y) = (blockNamePC x, cvtRegs y)

  jnRegs = Map.unionWith (Ty.\/)

  cvtRegs y =
    Map.fromListWith (Ty.\/)
      [ (r, cvtRegVal rv) | (Micro.Reg r, rv) <- Map.toList (Ty.env y) ]

  cvtRegVal rv = case rv of
                   Ty.RegBottom -> Ty.bottom
                   Ty.RegVal v  -> v
                   Ty.RegRef {} -> error "cvtRegVal: RegRef"

