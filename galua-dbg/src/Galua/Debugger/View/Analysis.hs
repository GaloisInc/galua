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




exportResult :: Result -> JS.Value
exportResult Result { .. } =
  JS.object
    [ "returns" .= exportListVals    maps resReturns
    , "raises"  .= exportValue       maps resRaises
    , "post"    .= exportGlobalState maps resGlobals
    ]
  where
  maps = idMaps resGlobals



exportGlobalState :: IdMaps -> GlobalState -> JS.Value
exportGlobalState maps GlobalState { .. } =
  JS.object [ "tables"    .= [ exportTableV maps v | v <- Map.elems tables ]
            , "heap"      .= [ exportValue  maps v | v <- Map.elems heap ]
            , "functions" .= [ exportFunV   maps v | v <- Map.elems functions ]
            ]

data IdMaps = IdMaps
  { tableIds :: Map TableId   Int
  , refIds   :: Map RefId     Int
  , cloIds   :: Map ClosureId Int
  }

idMaps :: GlobalState -> IdMaps
idMaps GlobalState { .. } = IdMaps { tableIds = toIds tables
                                   , refIds   = toIds heap
                                   , cloIds   = toIds functions
                                   }
  where
  toIds m = Map.fromList (zip (Map.keys m) [ 0 .. ])






--------------------------------------------------------------------------------
-- Keys/Ids

{-
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

cloIdString :: ClosureId -> String
cloIdString (ClosureId bn pc) = globalBlockNameString bn ++ "-" ++ show pc
-}


--------------------------------------------------------------------------------

exportType :: Type -> Text
exportType t =
  case t of
    Nil             -> "nil"
    Bool            -> "bool"
    Number          -> "number"
    UserData        -> "user data"
    LightUserData   -> "light user data"
    Thread          -> "thread"

exportSingleV :: IdMaps -> SingleV -> JS.Value
exportSingleV IdMaps { .. } v =
  case v of
    BasicValue t    -> ty (exportType t) (Nothing :: Maybe ())
    StringValue r   -> ty "string"       (fmap shStr r)
    TableValue r    -> ty "table"        (fmap (tableIds Map.!) r)
    FunctionValue r -> ty "function"     (fmap (cloIds Map.!)   r)
    RefValue r      -> ty "reference"    (fmap (refIds Map.!)   r)

  where
  ty t mbRef = JS.object [ "type" .= (t :: Text), "more" .= mbRef ]
  shStr x    = constructStringLiteral (BS.fromStrict x)

exportValue :: IdMaps -> Value -> JS.Value
exportValue maps = toJSON . map (exportSingleV maps) . valueCases

exportListVals :: IdMaps -> List Value -> Maybe JS.Value
exportListVals maps xs =
  case xs of
    ListBottom  -> Nothing
    List n xs a -> Just $ JS.object [ "min_len"  .= n
                                    , "elements" .= map (exportValue maps) xs
                                    , "default"  .= exportValue maps a
                                    ]


exportTableV :: IdMaps -> TableV -> JS.Value
exportTableV maps TableV { .. } =
  JS.object [ "meta"  .= exportValue maps meta
            , "key"   .= exportValue maps tableKeys
            , "value" .= exportValue maps tableValues
            , "attrs" .= JS.object [ x .= exportValue maps v | (x,v) <- attrs ]
            ]
  where
  meta  = appFun tableFields Metatable
  attrs = case tableFields of
            FFun mp _ -> [ (decodeUtf8 f,v) | (Field f, v) <- Map.toList mp ]

exportFunV :: IdMaps -> FunV -> JS.Value
exportFunV IdMaps { .. } FunV { .. } =
  JS.object
    [ "fid"    .= expFun functionFID
    , "upvals" .= map expRef (Map.elems functionUpVals)
    ]
  where
  expFun x = case x of
               NoValue            -> "(no function)"
               MultipleValues     -> "(unknown)"
               OneValue Nothing   -> "(C function)"
               OneValue (Just f)  -> exportFID f

  expRef x  = case x of
                NoValue        -> -1
                MultipleValues -> -2
                OneValue r     -> refIds Map.! r



--------------------------------------------------------------------------------
-- Misc/helpers

tag :: String -> JS.Pair
tag x = "tag" .= x

tagged :: String -> [JS.Pair] -> JS.Value
tagged x xs = JS.object (tag x : xs)



