{-# LANGUAGE RecordWildCards, OverloadedStrings, TypeFamilies #-}
module Galua.Spec.AST
  ( Annot
  , TVar

  , Name(..)
  , Spec(..)
  , Decl(..)
  , ClassDecl(..)
  , ValDecl(..)
  , TypeDecl(..)
  , NamespaceDecl(..)
  , Type(..)
  , TCon(..)
  , Pretty(..)
  , prettyType
  ) where

import           Data.Text(Text)
import qualified Data.Text as Text
import           Data.List(intersperse)

import           AlexTools(HasRange(..),SourceRange)
import           Text.PrettyPrint

type family Annot a
type family TVar  a


data Spec a = Spec
  { specAnnot :: !(Annot a)
  , specDecls :: ![Decl a]
  }

data Name = Name { nameRange :: SourceRange, nameText :: Text }
              deriving Show

instance HasRange Name where
  range = nameRange

instance Eq Name where
  x == y = nameText x == nameText y

instance Ord Name where
  compare x y = compare (nameText x) (nameText y)

data Decl a = DClass      !(ClassDecl a)
            | DType       !(TypeDecl a)
            | DNamespace  !(NamespaceDecl a)
            | DValDecl    !(ValDecl a)

data ClassDecl a = ClassDecl
  { classAnnot   :: !(Annot a)
  , className    :: !Name
  , classExtends :: ![Name]
  , classMembers :: ![ValDecl a]
  }

data ValDecl a = ValDecl
  { valAnnot   :: !(Annot a)
  , valName    :: !Name
  , valVars    :: [Name]
  , valMutable :: !Bool
  , valType    :: !(Type a)
  }

data TypeDecl a = TypeDecl
  { typeDeclAnnot   :: !(Annot a)
  , typeDeclName    :: !Name
  , typeDeclDef     :: !(Type a)
  , typeDeclVars    :: ![Name]
  }

data NamespaceDecl a = NamespaceDecl
  { namespaceAnnot   :: !(Annot a)
  , namespaceName    :: !Name
  , namespaceNested  :: ![NamespaceDecl a]
  , namespaceMembers :: ![ValDecl a]
  }

data Type a = TCon !(Annot a) !TCon ![Type a]
            | TVar (TVar a)


data TCon   = TNil
            | TStringLit Text
            | TBoolean
            | TString
            | TInteger
            | TNumber
            | TDynamic
            | TArray
            | TMap
            | TTuple Int
            | TMaybe
            | TMany
            | TFun
            | TMutable Bool
            | TUser Name
            | TUnion
              deriving Eq





--------------------------------------------------------------------------------
-- Pretty Printing
--------------------------------------------------------------------------------


class Pretty t where
  pretty :: t -> Doc

prettyBlock :: Doc -> [Doc] -> Doc
prettyBlock d xs = d $$ nest 2 (vcat xs)


instance Pretty Name where
  pretty x = text (Text.unpack (nameText x))

instance Pretty (Spec a) where
  pretty Spec { .. } = vcat $ intersperse "" $ map pretty specDecls

instance Pretty (Decl a) where
  pretty d =
    case d of
      DClass x      -> pretty x
      DType x       -> pretty x
      DNamespace x  -> pretty x
      DValDecl x    -> pretty x

instance Pretty (ClassDecl a) where
  pretty ClassDecl { .. } =
    prettyBlock ("class" <+> pretty className)
                (map ppExt classExtends ++ map pretty classMembers)
    where
    ppExt x = "extends" <+> pretty x

instance Pretty (ValDecl a) where
  pretty ValDecl { .. } =
    mut <+> pretty valName <> ppGenericList valVars <> colon <+>
    pretty valType
    where mut
            | valMutable = "mutable"
            | otherwise  = empty

ppGenericList :: [Name] -> Doc
ppGenericList vars
  | null vars = empty
  | otherwise = "<" <> foldr1 (\x y -> x <> "," <+> y) (map pretty vars) <> ">"

instance Pretty (TypeDecl a) where
  pretty TypeDecl { .. } =
    "type" <+> pretty typeDeclName <> ppGenericList typeDeclVars <+> "=" <+>
    pretty typeDeclDef

instance Pretty (NamespaceDecl a) where
  pretty NamespaceDecl { .. } =
    prettyBlock ("namespace" <+> pretty namespaceName)
      (map pretty namespaceMembers ++ map pretty namespaceNested)


prettyType :: Int -> Type a -> Doc
prettyType p (TCon _ tc ts) = prettyTypeParts p tc ts

prettyTypeParts :: Int -> TCon -> [Type a] -> Doc
prettyTypeParts prec typeCon typeParams =
  case typeCon of
    TNil          -> ar0 "nil"
    TBoolean      -> ar0 "boolean"
    TString       -> ar0 "string"
    TInteger      -> ar0 "integer"
    TNumber       -> ar0 "number"
    TDynamic      -> ar0 "dynamic"
    TNil          -> ar0 "nil"
    TMutable b    -> ar0 (if b then "mutable"
                               else if prec > 0 then empty else "immutable")
    TArray        -> ar2 $ \m t -> pp 1 m <+> braces (pp 0 t)
    TMap          -> ar3 $ \m s t -> pp 1 m <+>
                                      braces (pp 0 s <+> ":" <+> pp 0 t)
    TTuple n      -> arN n (parens . hsep . punctuate comma . map (pp 0))
    TMaybe        -> ar1 $ \t   -> wrap 3 (pp 2 t <> text "?")
    TMany         -> ar1 $ \t   -> wrap 3 (pp 2 t <> text "*")
    TFun          -> ar2 $ \s t -> wrap 1 (pp 1 s <+> text "->" <+> pp 0 t)
    TUser x       -> ar0 (pretty x)
    TUnion        -> ar2 $ \s t -> wrap 2 (pp 2 s <+> text "|" <+> pp 2 t)
  where
  ar0 f   = prettyTypeApp 0 typeParams $ \_                 -> f
  ar1 f   = prettyTypeApp 1 typeParams $ \ ~(x : _)         -> f x
  ar2 f   = prettyTypeApp 2 typeParams $ \ ~(x : y : _)     -> f x y
  ar3 f   = prettyTypeApp 3 typeParams $ \ ~(x : y : z : _) -> f x y z
  arN n f = prettyTypeApp n typeParams f

  wrap n  = if prec < n then id else parens

  pp _ (Left x)  = x
  pp n (Right t) = prettyType n t



instance Pretty (Type a) where
  pretty = prettyType 0

instance Pretty TCon where
  pretty tc = prettyTypeParts 0 tc []


prettyTypeApp :: Int -> [Type a] -> ([Either Doc (Type a)] -> Doc) -> Doc
prettyTypeApp n xs f
  | null bs   = ty
  | otherwise = parens (ty <+> hsep (map err bs))
  where
  (as,bs)    = splitAt n xs
  ts         = take n (map Right as ++ repeat prettyWild)
  ty         = f ts

  err b      = text "!" <> prettyType 10 b

  prettyWild = Left "_"



