module Galua.Micro.Translate.InferTypes where

import qualified Data.Map as Map
import Text.PrettyPrint

import Galua.Value
import Galua.Code(FunId)
import Galua.Pretty(pp)
import Galua.Mach(TypeMetatables)
import Galua.Micro.AST(MicroFunction)
import Galua.Micro.Type.Value(initLuaArgList,State(..))
import Galua.Micro.Type.Import(importClosure)
import Galua.Micro.Type.Primitives(buildPrimMap)
import Galua.Micro.Type.Eval(analyze,Result(..))


inferTypes :: TypeMetatables    {- ^ Metatables for prim types -} ->
              Reference Table   {- ^ Global environemnt -}        ->
              FunId             {- ^ FunId from the closure -}    ->
              Reference Closure {- ^ We'd like to analyze this -} ->
              MicroFunction     {- ^ Code for the current function -} ->

              IO ()
inferTypes meta env fid clo fun =
  do (cid,gid,gs) <- importClosure meta env clo
     let prims    = buildPrimMap gid gs
         args     = initLuaArgList -- XXX: more precise arguments
         funsCode = Map.singleton fid fun -- XXX: we need more code
         result   = analyze funsCode prims cid args gs
     writeFile (show (pp fid)) (show (extractInteresting fid result))


extractInteresting :: FunId -> Result -> Doc
extractInteresting r s = vcat (map pr states)
  where
  states = [ (b, localState info) | (b,info) <- Map.toList (resStates s) ]
  pr (b,s) = pp b $$ nest 2 (pp s)


