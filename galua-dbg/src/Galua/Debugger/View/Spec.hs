{-# LANGUAGE OverloadedStrings #-}
module Galua.Debugger.View.Spec where

import Galua.Spec.AST
import Galua.Spec.Parser(Parsed)
import Galua.Debugger(GlobalTypeEntry(..))

import qualified Data.Aeson as JS
import qualified Data.Aeson.Types as JS
import           Data.Aeson (toJSON, (.=))
import           Text.PrettyPrint
import qualified Data.Text as Text


exportT :: GlobalTypeEntry -> JS.Value
exportT t = JS.object [ "text" .= txt ] -- XXX: more structure?
  where
  txt = case t of
          GlobalNamespace _ -> "namespace"
          GlobalType ts     -> show (vcat $ map exportVD ts)


exportVD :: ValDecl Parsed -> Doc
exportVD vd = mut <+> vars <+> pretty (valType vd)
  where
  txt  = text . Text.unpack
  mut  = if valMutable vd then "mutable" else empty
  vars = case valVars vd of
           [] -> empty
           xs -> "<" <> hcat (punctuate comma (map (txt . nameText) xs)) <> ">"



