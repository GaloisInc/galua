{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Galua.Debugger.View.Analysis where

import qualified Data.Aeson as JS
import qualified Data.Aeson.Types as JS
import           Data.Aeson (toJSON, (.=))
import           Data.Text(Text)
import qualified Data.Text as Text
import           Data.Text.Encoding(decodeUtf8)
import           Data.String(fromString)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.ByteString(ByteString)
import qualified Data.ByteString.Lazy as BS (fromStrict)
import           Data.Maybe(mapMaybe)
import           Data.List(unfoldr)
import qualified Data.Vector as Vector

import qualified Language.Lua.Bytecode.Debug as OP
import qualified Language.Lua.Bytecode       as OP
import           Language.Lua.Bytecode.FunId
import           Language.Lua.StringLiteral (constructStringLiteral)

import           Galua.Micro.AST
import           Galua.Micro.Type.Value
import           Galua.Micro.Type.Eval(Result(..))
import           Galua.Debugger.View.Utils

--------------------------------------------------------------------------------
-- Function Info

data Info1 = Info1
  { infoFuns     :: Map FunId OP.Function
  , infoFunNames :: Map FunId String
  , infoResult   :: Result
  }

data Info2 = Info2
  { info1       :: Info1
  , infoCaller  :: CallsiteId
  , infoCurFID  :: FunId
  , infoCurFun  :: OP.Function
  , infoPC      :: Int
  }

data Info3 = Info3
  { info2       :: Info2
  , infoGlobal  :: GlobalState
  }


--------------------------------------------------------------------------------
-- Misc/helpers

tag :: String -> JS.Pair
tag x = "tag" .= x

tagged :: String -> [JS.Pair] -> JS.Value
tagged x xs = JS.object (tag x : xs)


--------------------------------------------------------------------------------
-- Keys/Ids

blockIdString :: BlockName -> String
blockIdString b =
  case b of
    EntryBlock   -> "entry"
    PCBlock n    -> "pc_" ++ show n
    NewBlock x y -> "new_" ++ show x ++ "_" ++ show y

globalBlockNameString :: GlobalBlockName -> String
globalBlockNameString (GlobalBlockName cid qn) =
  callSiteIdString cid ++ "-" ++ qualBlockNameIdString qn

callSiteIdString :: CallsiteId -> String
callSiteIdString (CallsiteId qn n) = qualBlockNameIdString qn ++ "-" ++ show n

qualBlockNameIdString :: QualifiedBlockName -> String
qualBlockNameIdString (QualifiedBlockName fid bn) =
  funIdString fid ++ "-" ++ blockIdString bn

tableIdString :: TableId -> String
tableIdString (TableId bn pc) = globalBlockNameString bn ++ "-" ++ show pc

refIdString :: RefId -> String
refIdString (RefId bn pc) = globalBlockNameString bn ++ "-" ++ show pc


--------------------------------------------------------------------------------


exportReg :: Info2 -> Reg -> Maybe Text
exportReg Info2 { .. } reg =
  case reg of
    Reg r | Just nm <- OP.lookupLocalName infoCurFun infoPC r
            -> Just (decodeUtf8 nm)
    Reg r   -> Just (Text.pack ("R[" ++ show r ++ "]"))
    TMP a b -> Nothing

exportFID :: FunId -> JS.Value
exportFID = fromString . funIdString




exportType :: Type -> Text
exportType t =
  case t of
    Nil             -> "nil"
    Bool            -> "bool"
    Number          -> "number"
    UserData        -> "user data"
    LightUserData   -> "light user data"
    Thread          -> "thread"

-- XXX: Add meta-tables to primitives.
-- XXX: Dereference references (?)
exportSingleV :: Info3 -> SingleV -> JS.Value
exportSingleV info v =
  case v of
    BasicValue t            -> simp (exportType t)
    StringValue Nothing     -> simp "string"
    StringValue (Just x)    -> val "string"
                                 (constructStringLiteral (BS.fromStrict x))
    TableValue Nothing      -> simp "table"
    TableValue (Just r)     -> val "table" (tableIdString r)
    FunctionValue Nothing   -> simp "function"
    FunctionValue (Just f)  -> val "function" (funIdString f)
    RefValue Nothing        -> simp "reference"
    RefValue (Just r)       -> val "reference" (refIdString r)

  where
  simp x  = tagged "simple" [ "type" .= (x :: Text) ]
  val x y = tagged x [ "value" .= y ]

exportValue :: Info3 -> Value -> JS.Value
exportValue info = toJSON . map (exportSingleV info) . valueCases

exportListVals :: Info3 -> List Value -> Maybe JS.Value
exportListVals info xs =
  case xs of
    ListBottom  -> Nothing
    List n xs a -> Just $ JS.object [ "min_len"  .= n
                                    , "elements" .= map (exportValue info) xs
                                    , "default"  .= exportValue info a
                                    ]

exportLocalState :: Info3 -> LocalState -> JS.Value
exportLocalState info LocalState { .. } =
  JS.object [ "env"  .= JS.object (mapMaybe keyVal (Map.toList env))
            , "args" .= exportListVals info argReg
            , "list" .= exportListVals info listReg
            ]
  where
  keyVal (r,v) = do lab <- exportReg (info2 info) r
                    return (lab .= exportValue info v)

exportTableV :: Info3 -> TableV -> JS.Value
exportTableV info TableV { .. } =
  JS.object [ "meta"  .= exportValue info meta
            , "key"   .= exportValue info tableKeys
            , "value" .= exportValue info tableValues
            , "attrs" .= JS.object [ x .= exportValue info v | (x,v) <- attrs ]
            ]
  where
  meta  = appFun tableFields Metatable
  attrs = case tableFields of
            FFun mp _ -> [ (decodeUtf8 f,v) | (Field f, v) <- Map.toList mp ]


exportGlobalState :: Info3 -> JS.Value
exportGlobalState info =
  JS.object [ "tables" .= JS.object (map expT (Map.toList tables))
            , "heap"   .= JS.object (map expR (Map.toList heap))
            ]
  where
  GlobalState { .. } = infoGlobal info
  expT (t,v)         = Text.pack (tableIdString t) .= exportTableV info v
  expR (r,v)         = Text.pack (refIdString r)   .= exportValue info v


exportState :: Info2 -> State -> JS.Value
exportState info State { .. } =
  JS.object [ "local"  .= exportLocalState  info3 localState
            , "global" .= exportGlobalState info3
            ]
  where
  info3 = Info3 { infoGlobal = globaleState, info2 = info }


exportPC :: Info2 -> Maybe (JS.Value,Info2)
exportPC info
  | pc >= Vector.length (OP.funcCode (infoCurFun info)) = Nothing
  | otherwise =
    let i1    = info1 info
        qual  = QualifiedBlockName (infoCurFID info) (PCBlock pc)
        glob  = GlobalBlockName (infoCaller info) qual
    in case Map.lookup glob (resStates (infoResult i1)) of
         Just s  -> Just (exportState info s, info { infoPC = pc + 1 })
         Nothing -> error "Missing analysis result"
                    -- XXX: Maybe this means unreachable code?
  where pc = infoPC info


-- XXX: Maybe export the Entry block also?
exportFun :: Info1 -> FunId -> JS.Value
exportFun info1 fid =
  case Map.lookup fid (infoFuns info1) of
    Just fun ->
      let info2 = Info2 { info1      = info1
                        , infoCurFID = fid
                        , infoCurFun = fun
                        , infoPC     = 0
                        }
      in toJSON (unfoldr exportPC info2)
    Nothing  -> error "Missing function"




