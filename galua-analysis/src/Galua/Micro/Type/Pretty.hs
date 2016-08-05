{-# LANGUAGE OverloadedStrings, RecordWildCards, TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Galua.Micro.Type.Pretty where

import           Data.ByteString (ByteString)
import qualified Data.Set as Set
import           Data.Map ( Map )
import qualified Data.Map as Map
import Data.List(intersperse)
import Text.PrettyPrint
import Language.Lua.Bytecode.Pretty(PP(..),PPInfo)
import Galua.Micro.Type.Value

instance PP Int where
  pp _ x = text (show x)

instance PP Type where
  pp _ ty = text (show ty)

instance PP RefId where
  pp i (RefId b n)      = "ref:" <> pp i b <> colon <> int n

instance PP TableId where
  pp i (TableId b n)    = "tab:" <> pp i b <> colon <> int n

instance PP ClosureId where
  pp i (ClosureId b n)  = "clo:" <> pp i b <> colon <> int n


instance PP QualifiedBlockName where
  pp i (QualifiedBlockName fid b) = pp i fid <> colon <> pp i b

instance PP GlobalBlockName where
  pp i (GlobalBlockName caller qbn) = pp i caller <> colon <> pp i qbn

instance PP CallsiteId where
  pp i (CallsiteId qbn opcode) = pp i qbn <> colon <> pp i opcode

instance PP a => PP (List a) where
  pp n xs = case xs of
              ListBottom -> "⟂"
              List _count xs' y  ->
               case xs' of
                 [] -> "repeat" <+> pp n y
                 _  -> brackets (fsep (punctuate comma (map (pp n) xs'))) <+>
                       "++" <+> "repeat" <+> pp n y

instance PP ByteString where
  pp _ = text . show

instance PP a => PP (WithTop a) where
  pp _ Top = "⊤"
  pp n (NotTop p) = pp n p

instance PP RegVal where
  pp n val =
    case val of
      RegBottom -> "⊥"
      RegVal v  -> pp n v
      RegRef r  -> ppSet n "ref" r

instance PP Value where
  pp _ t | t == bottom = "⊥"
  pp n Value { .. } =
      ppOpts $ map (pp n) (Set.toList valueBasic) ++
               ppStr ++
               [ ppSet n "fun"   valueFunction
               , ppSet n "table" valueTable ]

    where
    ppStr = case valueString of
              NoValue        -> []
              OneValue x     -> [ text (show x) ]
              MultipleValues -> [ "String" ]

ppOpts :: [Doc] -> Doc
ppOpts ds = hsep (intersperse "/" ds)



ppSet :: PP a => PPInfo -> String -> WithTop (Set.Set a) -> Doc
ppSet _ tag Top = braces (text tag <> ": ⊤")
ppSet n tag (NotTop xs)
  | Set.null xs = braces (text tag <> ": ⊥")
  | otherwise   = braces (text tag <> ":" <+>
                                    ppOpts (map (pp n) (Set.toList xs)))


instance (PP a, PP b) => PP (a :-> b) where
  pp n (FFun mp b) =
    vcat [ pp n k <+> "->" <+> pp n v | (k,v) <- Map.toList mp ]
    $$ "_" <+> "->" <+> pp n b

instance (PP a, PP b) => PP (Map a b) where
  pp n mp = vcat [ pp n k <+> "->" <+> pp n v | (k,v) <- Map.toList mp ]

instance PP a => PP (Lift a) where
  pp n v = case v of
             NoValue        -> "⊥"
             OneValue x     -> pp n x
             MultipleValues -> "⊤"



instance PP TableV where
  pp n TableV { .. } = braces (vcat (pp n tableFields : metatable : rest))
      where
      metatable = "meta ->" <+> pp n tableMeta
      rest | tableKeys == bottom || tableValues == bottom = []
           | otherwise = [ pp n tableKeys <+> "->" <+> pp n tableValues ]

instance PP FunV where
  pp n FunV { .. } = braces ( pp n (lab <$> functionFID) <+> "|" <+>
                              hsep (map (ppSet n "ref")
                                        (Map.elems functionUpVals)) )
    where lab mb = case mb of
                     CFunImpl ptr -> "C:" <> text (show ptr)
                     LuaFunImpl fid -> "LUA" <+> pp n fid


instance PP LocalState where
  pp n LocalState { .. } =
    vcat [ entry "env" env
         , "upvals" <> colon <+> hsep (map (ppSet n "ref") (Map.elems upvals))
         ]
    where
    entry x y
      | y == bottom = empty
      | otherwise   = x <> colon $$ nest 2 (pp n y)

instance PP GlobalState where
  pp n GlobalState { .. } =
    vcat [ entry "tables" tables
         , entry "type.meta"  basicMetas
         , entry "string.meta" stringMeta
         , entry "function.meta" funMeta
         , entry "heap" heap
         , entry "tables" tables
         , entry "functions" functions
         ]
    where
    entry x y
      | y == bottom = empty
      | otherwise   = x <> colon $$ nest 2 (pp n y)

instance PP State where
  pp n State { .. } = pp n localState $$ pp n globalState










