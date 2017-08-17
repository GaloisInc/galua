module Galua.Micro.Translate.InferTypes where

import           Data.Map ( Map )
import qualified Data.Map as Map
import Text.PrettyPrint

import Galua.Value
import Galua.Code(FunId,UpIx(..))
import Galua.Pretty(pp)
import Galua.Mach(TypeMetatables)
import Galua.Micro.AST(MicroFunction,BlockName)
import Galua.Micro.Type.Value( GlobalBlockName(..), QualifiedBlockName(..)
                             , CallsiteId
                             , initLuaArgList,State(..)
                             , LocalState
                             , (\/)
                             , WithTop(..)
                             )
import Galua.Micro.Type.Import(importClosure,importEnv)
import Galua.Micro.Type.Primitives(buildPrimMap)
import Galua.Micro.Type.Eval(analyze,analyzeFun,Result(..))


-- | Gather information about a function in the current environment.
inferFunTypes ::
  Map FunId MicroFunction {- ^ Source code for functions -} ->
  TypeMetatables          {- ^ Metatables for prim types -} ->
  Reference Table         {- ^ Global environemnt -}        ->
  FunId                   {- ^ FunId to analyze -}          ->
  IO (Map BlockName LocalState)
inferFunTypes funsCode meta env fid =
  do (gid,gs) <- importEnv meta env
     let prims    = buildPrimMap gid gs
         args     = initLuaArgList
         ups      = Map.empty
         generalResult   = analyzeFun funsCode prims fid ups args gs
         result = extractInteresting fid generalResult
     writeFile (show (pp fid)) $ show $ prInteresting $ result -- for debug
     return result




inferTypes :: TypeMetatables    {- ^ Metatables for prim types -} ->
              Reference Table   {- ^ Global environemnt -}        ->
              FunId             {- ^ FunId from the closure -}    ->
              Reference Closure {- ^ We'd like to analyze this -} ->
              MicroFunction     {- ^ Code for the current function -} ->
              IO (Map BlockName LocalState)
inferTypes meta env fid clo fun =
  do (cid,gid,gs) <- importClosure meta env clo
     let prims    = buildPrimMap gid gs
         args     = initLuaArgList -- XXX: more precise arguments
         funsCode = Map.singleton fid fun -- XXX: we need more code
         generalResult   = analyze funsCode prims cid args gs
         result = extractInteresting fid generalResult
     writeFile (show (pp fid)) $ show $ prInteresting $ result -- for debug
     return result


-- | Get the local state for this function.
-- If there multiple call-site wew just "join" them together.
extractInteresting :: FunId -> Result -> Map BlockName LocalState
extractInteresting fid s =
  Map.fromListWith (\/)
    [ (b, localState info)
    | (gb,info) <- Map.toList (resStates s)
    , (_callsite,b) <- ourBlockName fid gb
    ]

prInteresting :: Map BlockName LocalState -> Doc
prInteresting = vcat . map pr . Map.toList
  where
  pr (b,s) = pp b $$ nest 2 (pp s)


ourBlockName :: FunId -> GlobalBlockName -> [ (CallsiteId, BlockName) ]
ourBlockName fid (GlobalBlockName cs (QualifiedBlockName fid' b))
  | fid == fid' = [ (cs,b) ]
  | otherwise   = []



