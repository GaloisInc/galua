{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Galua.Debugger.View.Analysis where

import qualified Data.Aeson as JS
import qualified Data.Aeson.Types as JS
import           Data.Aeson ((.=))
import           Data.Text(Text)
import qualified Data.Text as Text
import           Data.Text.Encoding(decodeUtf8)
import qualified Data.Set as Set
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BS (fromStrict)

import           Language.Lua.StringLiteral (constructStringLiteral)

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

exportType :: Type -> Text
exportType t =
  case t of
    Nil             -> "nil"
    Number          -> "number"
    UserData        -> "user data"
    LightUserData   -> "light user data"
    Thread          -> "thread"

exportValue :: IdMaps -> Value -> JS.Value
exportValue maps v = JS.object
                        [ "simple"    .= names
                        , "table"     .= seeAlso valueTable tableIds
                        , "function"  .= seeAlso valueFunction cloIds
                        ]
  where
  names = Set.unions [ name "table"     valueTable
                     , name "function"  valueFunction
                     , booleanName
                     , stringName
                     , Set.map exportType (valueBasic v)
                     ]

  stringName = case valueString v of
                 NoValue         -> Set.empty
                 OneValue s      -> Set.singleton (shStr s)
                 MultipleValues  -> Set.singleton "string"

  booleanName = case valueBoolean v of
                 NoValue         -> Set.empty
                 OneValue True   -> Set.singleton "true"
                 OneValue False  -> Set.singleton "false"
                 MultipleValues  -> Set.singleton "boolean"

  shStr x  = Text.pack (constructStringLiteral (BS.fromStrict x))

  name x f = case f v of
               Top      -> Set.singleton x
               NotTop _ -> Set.empty

  seeAlso f g = case f v of
                  NotTop xs -> Set.map (g maps Map.!) xs
                  _         -> Set.empty

exportListVals :: IdMaps -> List Value -> Maybe JS.Value
exportListVals maps lv =
  case lv of
    ListBottom  -> Nothing
    List n xs a -> Just $ JS.object [ "min_len"  .= n
                                    , "elements" .= map (exportValue maps) xs
                                    , "default"  .= exportValue maps a
                                    ]


exportTableV :: IdMaps -> TableV -> JS.Value
exportTableV maps TableV { .. } =
  JS.object [ "meta"  .= exportValue maps tableMeta
            , "key"   .= exportValue maps tableKeys
            , "value" .= exportValue maps tableValues
            , "attrs" .= JS.object [ x .= exportValue maps v | (x,v) <- attrs ]
            ]
  where
  attrs = case tableFields of
            FFun mp _ -> [ (decodeUtf8 f,v) | (f, v) <- Map.toList mp ]

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
               OneValue (CFunImpl _) -> "(C function)"
               OneValue (LuaFunImpl f)  -> exportFID f

  expRef x  = case x of
                Top            -> [-2]
                NotTop xs      -> map (refIds Map.!) (Set.toList xs)



--------------------------------------------------------------------------------
-- Misc/helpers

tag :: String -> JS.Pair
tag x = "tag" .= x

tagged :: String -> [JS.Pair] -> JS.Value
tagged x xs = JS.object (tag x : xs)



