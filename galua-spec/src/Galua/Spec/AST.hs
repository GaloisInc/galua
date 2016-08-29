{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Galua.Spec.AST
  ( Name(..)
  , Spec(..)
  , Decl(..)
  , ClassDecl(..)
  , ValDecl(..)
  , TypeDecl(..)
  , NamespaceDecl(..)
  , Type(..)
  , TCon(..)
  , Annotated(..)
  , Pretty(..)
  ) where

import           Data.Text(Text)
import qualified Data.Text as Text
import           Data.List(intersperse)

import           AlexTools(HasRange(..),SourceRange)
import           Text.PrettyPrint

data Spec a = Spec
  { specAnnot :: !a
  , specDecls :: ![Decl a]
  } deriving Show

data Name a = Name
  { nameAnnot :: !a
  , nameText  :: !Text
  } deriving Show

data Decl a = DClass      !(ClassDecl a)
            | DType       !(TypeDecl a)
            | DNamespace  !(NamespaceDecl a)
            | DValDecl    !(ValDecl a)
              deriving Show

data ClassDecl a = ClassDecl
  { classAnnot   :: !a
  , className    :: !(Name a)
  , classExtends :: ![Name a]
  , classMembers :: ![ValDecl a]
  } deriving Show

data ValDecl a = ValDecl
  { valAnnot   :: !a
  , valName    :: !(Name a)
  , valMutable :: !Bool
  , valType    :: !(Type a)
  } deriving Show

data TypeDecl a = TypeDecl
  { typeDeclAnnot   :: !a
  , typeDeclName    :: !(Name a)
  , typeDeclDef     :: !(Type a)
  } deriving Show

data NamespaceDecl a = NamespaceDecl
  { namespaceAnnot   :: !a
  , namespaceName    :: !(Name a)
  , namespaceNested  :: ![NamespaceDecl a]
  , namespaceMembers :: ![ValDecl a]
  } deriving Show

data Type a = Type
  { typeAnnot   :: !a
  , typeCon     :: !(TCon a)
  , typeParams  :: ![Type a]
  } deriving Show

data TCon a = TNil a
            | TBoolean a
            | TString a
            | TInteger a
            | TNumber a
            | TArray a
            | TTuple Int a
            | TMaybe a
            | TMany a
            | TFun a
            | TUser (Name a)
              deriving Show



--------------------------------------------------------------------------------

class Annotated t a where
  getAnnot :: t -> a
  setAnnot :: a -> t -> t

instance Annotated (Spec a) a where
  getAnnot      = specAnnot
  setAnnot a t  = t { specAnnot = a }

instance Annotated (Name a) a where
  getAnnot      = nameAnnot
  setAnnot a t  = t { nameAnnot = a }

instance Annotated (Decl a) a where
  getAnnot d =
    case d of
      DClass x     -> getAnnot x
      DType x      -> getAnnot x
      DNamespace x -> getAnnot x
      DValDecl x   -> getAnnot x

  setAnnot a d =
    case d of
      DClass x      -> DClass     (setAnnot a x)
      DType x       -> DType      (setAnnot a x)
      DNamespace x  -> DNamespace (setAnnot a x)
      DValDecl x    -> DValDecl   (setAnnot a x)

instance Annotated (ClassDecl a) a where
  getAnnot      = classAnnot
  setAnnot a d  = d { classAnnot = a }

instance Annotated (ValDecl a) a where
  getAnnot      = valAnnot
  setAnnot a v  = v { valAnnot = a }

instance Annotated (TypeDecl a) a where
  getAnnot      = typeDeclAnnot
  setAnnot a t  = t { typeDeclAnnot = a }

instance Annotated (NamespaceDecl a) a where
  getAnnot      = namespaceAnnot
  setAnnot a n  = n { namespaceAnnot = a }

instance Annotated (Type a) a where
  getAnnot      = typeAnnot
  setAnnot a t  = t { typeAnnot = a }

instance Annotated (TCon a) a where
  getAnnot tc =
    case tc of
      TNil a      -> a
      TBoolean a  -> a
      TString a   -> a
      TInteger a  -> a
      TNumber a   -> a
      TArray a    -> a
      TTuple _ a  -> a
      TMaybe a    -> a
      TMany a     -> a
      TFun a      -> a
      TUser a     -> getAnnot a

  setAnnot a tc =
     case tc of
      TNil _      -> TNil a
      TBoolean _  -> TBoolean a
      TString _   -> TString a
      TInteger _  -> TInteger a
      TNumber _   -> TNumber a
      TArray _    -> TArray a
      TTuple n _  -> TTuple n a
      TMaybe _    -> TMaybe a
      TMany _     -> TMany a
      TFun _      -> TFun a
      TUser n     -> TUser (setAnnot a n)


instance HasRange (Type SourceRange) where
  range = getAnnot

instance HasRange (Name SourceRange) where
  range = getAnnot

--------------------------------------------------------------------------------

class Pretty t where
  pretty :: t -> Doc

prettyBlock :: Doc -> [Doc] -> Doc
prettyBlock d xs = d $$ nest 2 (vcat xs)


instance Pretty (Name a) where
  pretty Name { .. } = text (Text.unpack nameText)

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
  pretty ValDecl { .. } = mut <+> pretty valName <> colon <+> pretty valType
    where mut = if valMutable then "mutable" else empty

instance Pretty (TypeDecl a) where
  pretty TypeDecl { .. } =
    "type" <+> pretty typeDeclName <+> "=" <+> pretty typeDeclDef

instance Pretty (NamespaceDecl a) where
  pretty NamespaceDecl { .. } =
    prettyBlock ("namespace" <+> pretty namespaceName)
      (map pretty namespaceMembers ++ map pretty namespaceNested)

prettyType :: Int -> Type a -> Doc
prettyType prec Type { .. } =
  case typeCon of
    TNil _      -> ar0 "nil"
    TBoolean _  -> ar0 "boolean"
    TString _   -> ar0 "string"
    TInteger _  -> ar0 "integer"
    TNumber _   -> ar0 "number"
    TArray _    -> ar1 (braces . pretty)
    TTuple n _  -> arN n (parens . hsep . punctuate comma . map pretty)
    TMaybe _    -> ar1 $ \t   -> wrap 2 (prettyType 1 t <> text "?")
    TMany _     -> ar1 $ \t   -> wrap 2 (prettyType 1 t <> text "*")
    TFun _      -> ar2 $ \s t -> wrap 1 (prettyType 1 s <+> text "->" <+>
                                         prettyType 0 t)
    TUser x     -> ar0 (pretty x)
  where
  ar0 f   = prettyTypeApp typeAnnot 0 typeParams $ \_             -> f
  ar1 f   = prettyTypeApp typeAnnot 1 typeParams $ \ ~(x : _)     -> f x
  ar2 f   = prettyTypeApp typeAnnot 2 typeParams $ \ ~(x : y : _) -> f x y
  arN n f = prettyTypeApp typeAnnot n typeParams f

  wrap n  = if prec < n then id else parens


instance Pretty (Type a) where
  pretty = prettyType 0

instance Pretty (TCon a) where
  pretty tc = pretty t
    where t = Type { typeAnnot = getAnnot tc, typeCon = tc, typeParams = [] }

prettyTypeApp :: a -> Int -> [Type a] -> ([Type a] -> Doc) -> Doc
prettyTypeApp a n xs f
  | null bs   = ty
  | otherwise = parens (ty <+> hsep (map err bs))
  where
  (as,bs)    = splitAt n xs
  ts         = take n (as ++ repeat prettyWild)
  ty         = f ts

  err b      = text "!" <> prettyType 10 b

  prettyWild = Type { typeAnnot   = a
                    , typeCon     = TUser Name { nameAnnot = a, nameText = "_" }
                    , typeParams  = []
                    }





