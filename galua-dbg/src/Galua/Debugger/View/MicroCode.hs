{-# Language OverloadedStrings #-}
module Galua.Debugger.View.MicroCode where

import qualified Data.Aeson as JS
import           Data.Aeson (toJSON, (.=))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Map as Map
import           Data.Vector(Vector)
import qualified Data.Vector as Vector
import           Data.Maybe(maybeToList)

import Galua.Pretty(pp)
import Galua.Micro.AST



exportMicroFunction :: MicroFunction -> JS.Value
exportMicroFunction f =
  JS.object [ exportBlockName l .= exportBlock b
              | (l,b) <- Map.toList (functionCode f) ]

exportBlockName :: BlockName -> Text
exportBlockName bn =
  case bn of
    PCBlock n    -> Text.append "pc_" (sh n)
    NewBlock x y -> Text.concat [ "new_", sh x, sh y ]
    EntryBlock   -> "entry"
  where sh = Text.pack . show

exportBlock :: Vector BlockStmt -> JS.Value
exportBlock xs = JS.object [ "stmts" .= fmap exportBlockStatemt xs
                           , "next"  .= map exportBlockName (blockNext xs)
                           ]

blockNext :: Vector BlockStmt -> [BlockName]
blockNext v
  | Vector.null v = []
  | otherwise =
     case stmtCode (Vector.last v) of
       Case _ xs mb -> map snd xs ++ maybeToList mb
       If _ x y     -> [ x, y ]
       Goto x       -> [ x ]
       _            -> []


exportBlockStatemt :: BlockStmt -> JS.Value
exportBlockStatemt bs = exportStmt (stmtCode bs)

exportStmt :: Stmt -> JS.Value
exportStmt stmt = toJSON (Text.pack (show (pp stmt)))



