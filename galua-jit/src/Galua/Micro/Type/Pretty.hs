{-# LANGUAGE OverloadedStrings, RecordWildCards, TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Galua.Micro.Type.Pretty where

import           Data.ByteString (ByteString)
import qualified Data.Set as Set
import           Data.Map ( Map )
import qualified Data.Map as Map
import Data.List(intersperse)
import Data.Maybe(maybeToList,fromMaybe)
import Text.PrettyPrint
import Galua.Micro.Type.Value
import Galua.Pretty

instance Pretty Int where
  pp x = text (show x)

instance Pretty Type where
  pp ty = text (show ty)

instance Pretty RefId where
  pp (RefId b n)      = "ref:" <> pp b <> colon <> int n

instance Pretty TableId where
  pp (TableId b n)    = "tab:" <> pp b <> colon <> int n

instance Pretty ClosureId where
  pp (ClosureId b n)  = "clo:" <> pp b <> colon <> int n


instance Pretty QualifiedBlockName where
  pp (QualifiedBlockName fid b) = pp fid <> colon <> pp b

instance Pretty GlobalBlockName where
  pp (GlobalBlockName caller qbn) = pp caller <> colon <> pp qbn

instance Pretty CallsiteId where
  pp (CallsiteId qbn opcode) = pp qbn <> colon <> pp opcode

instance Pretty a => Pretty (List a) where
  pp xs = case xs of
              ListBottom -> "⟂"
              List _count xs' y  ->
               case xs' of
                 [] -> "repeat" <+> pp y
                 _  -> brackets (fsep (punctuate comma (map pp xs'))) <+>
                       "++" <+> "repeat" <+> pp y

instance Pretty ByteString where
  pp = text . show

instance Pretty a => Pretty (WithTop a) where
  pp Top = "⊤"
  pp (NotTop p) = pp p

instance Pretty RegVal where
  pp val =
    case val of
      RegBottom -> "⊥"
      RegVal v  -> pp v
      RegRef r  -> ppSet "ref" r

instance Pretty Value where
  pp t | t == bottom = "⊥"
  pp Value { .. } =
      ppOpts $ map pp (Set.toList valueBasic) ++
               ppStr ++
               [ d | d <- maybeToList $ ppSetNonBot "fun" valueFunction ] ++
               [ d | d <- maybeToList $ ppSetNonBot "table " valueTable ]
    where
    ppStr = case valueString of
              NoValue        -> []
              OneValue x     -> [ text (show x) ]
              MultipleValues -> [ "String" ]

ppOpts :: [Doc] -> Doc
ppOpts [] = text "⊥"
ppOpts ds = hsep (intersperse "/" ds)

ppSetNonBot :: Pretty a => String -> WithTop (Set.Set a) -> Maybe Doc
ppSetNonBot tag Top = Just (braces (text tag <> ": ⊤"))
ppSetNonBot tag (NotTop xs)
  | Set.null xs = Nothing
  | otherwise   = Just $ braces (text tag <> ":" <+>
                                    ppOpts (map pp (Set.toList xs)))

ppSet :: Pretty a => String -> WithTop (Set.Set a) -> Doc
ppSet tag s = fromMaybe (braces (text tag <> ": ⊥")) (ppSetNonBot tag s)

instance (Pretty a, Pretty b) => Pretty (a :-> b) where
  pp (FFun mp b) =
    vcat [ pp k <+> "->" <+> pp v | (k,v) <- Map.toList mp ]
    $$ "_" <+> "->" <+> pp b

instance (Pretty a, Pretty b) => Pretty (Map a b) where
  pp mp = vcat [ pp k <+> "->" <+> pp v | (k,v) <- Map.toList mp ]

instance Pretty a => Pretty (Lift a) where
  pp v = case v of
           NoValue        -> "⊥"
           OneValue x     -> pp x
           MultipleValues -> "⊤"



instance Pretty TableV where
  pp TableV { .. } = braces (vcat (pp tableFields : metatable : rest))
      where
      metatable = "meta ->" <+> pp tableMeta
      rest | tableKeys == bottom || tableValues == bottom = []
           | otherwise = [ pp tableKeys <+> "->" <+> pp tableValues ]

instance Pretty FunV where
  pp FunV { .. } = braces ( pp (lab <$> functionFID) <+> "|" <+>
                              hsep (map (ppSet "ref")
                                        (Map.elems functionUpVals)) )
    where lab mb = case mb of
                     CFunImpl ptr -> "C:" <> text (show ptr)
                     LuaFunImpl fid -> "LUA" <+> pp fid


instance Pretty LocalState where
  pp LocalState { .. } =
    vcat [ entry "env" env
         , "upvals" <> colon <+> hsep (map (ppSet "ref") (Map.elems upvals))
         ]
    where
    entry x y
      | y == bottom = empty
      | otherwise   = x <> colon $$ nest 2 (pp y)

instance Pretty GlobalState where
  pp GlobalState { .. } =
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
      | otherwise   = x <> colon $$ nest 2 (pp y)

instance Pretty State where
  pp State { .. } = pp localState $$ pp globalState










