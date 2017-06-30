{-# Language OverloadedStrings #-}
module Galua.Debugger.View.MicroCode where

import qualified Data.Aeson as JS
import           Data.Aeson (toJSON, (.=))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Vector as Vector

import Galua.Pretty(Pretty,pp)
import Galua.Micro.AST



exportMicroFunction :: MicroFunction -> JS.Value
exportMicroFunction f =
  JS.object [ exportBlockName l .= exportBlock b
              | (l,b) <- Map.toList (functionCode f) ]

exportBlockName :: BlockName -> Text
exportBlockName bn =
  case bn of
    PCBlock n    -> Text.append "PC" (sh n)
    NewBlock x y -> Text.concat [ "PC", sh x, "_", sh y ]
    EntryBlock   -> "ENTRY"
  where sh = Text.pack . show

exportBlock :: Block -> JS.Value
exportBlock xs = JS.object
  [ "stmts" .= (map exportBlockStatemt (Vector.toList (blockBody xs))
                                     ++ [exportBlockStatemt (blockEnd xs)])
  , "next"  .= map exportBlockName (blockNext xs)
  ]


exportBlockStatemt :: Pretty a => BlockStmt a -> JS.Value
exportBlockStatemt bs = exportStmt (stmtCode bs)

exportStmt :: Pretty a => a -> JS.Value
exportStmt stmt = toJSON (Text.pack (show (pp stmt)))



