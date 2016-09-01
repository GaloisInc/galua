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
  , prettyType
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

{- XXX: This is required so that we can compare user-defined types
for equality.  Perhaps we should distinguish between parsed
names (which should not be compared) and the actual resolved names
that we use during type inference. -}
instance Eq (Name a) where
  x == y = nameText x == nameText y

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

data Type a = TCon !a !TCon ![Type a]
              deriving Show



data TCon   = TNil
            | TBoolean
            | TString
            | TInteger
            | TNumber
            | TArray
            | TMap
            | TTuple Int
            | TMaybe
            | TMany
            | TFun
            | TMutable Bool
            | TUser (Name ())
              deriving (Show,Eq)





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
  getAnnot ty = case ty of
                  TCon a _ _ -> a

  setAnnot a ty = case ty of
                    TCon _ tc ps -> TCon a tc ps


instance HasRange (Type SourceRange) where
  range = getAnnot

instance HasRange (Name SourceRange) where
  range = getAnnot

--------------------------------------------------------------------------------
-- Pretty Printing
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
prettyType prec (TCon ta typeCon typeParams) =
  case typeCon of
    TNil          -> ar0 "nil"
    TBoolean      -> ar0 "boolean"
    TString       -> ar0 "string"
    TInteger      -> ar0 "integer"
    TNumber       -> ar0 "number"
    TMutable b    -> ar0 (if b then "mutable"
                               else if prec > 0 then empty else "immutable")
    TArray        -> ar2 $ \m t -> pp 1 m <+> braces (pp 0 t)
    TMap          -> ar3 $ \m s t -> pp 1 m <+>
                                      braces (pp 0 s <+> ":" <+> pp 0 t)
    TTuple n      -> arN n (parens . hsep . punctuate comma . map (pp 0))
    TMaybe        -> ar1 $ \t   -> wrap 2 (pp 1 t <> text "?")
    TMany         -> ar1 $ \t   -> wrap 2 (pp 1 t <> text "*")
    TFun          -> ar2 $ \s t -> wrap 1 (pp 1 s <+> text "->" <+> pp 0 t)
    TUser x       -> ar0 (pretty x)
  where
  ar0 f   = prettyTypeApp ta 0 typeParams $ \_                 -> f
  ar1 f   = prettyTypeApp ta 1 typeParams $ \ ~(x : _)         -> f x
  ar2 f   = prettyTypeApp ta 2 typeParams $ \ ~(x : y : _)     -> f x y
  ar3 f   = prettyTypeApp ta 3 typeParams $ \ ~(x : y : z : _) -> f x y z
  arN n f = prettyTypeApp ta n typeParams f

  wrap n  = if prec < n then id else parens

  pp _ (Left x)  = x
  pp n (Right t) = prettyType n t



instance Pretty (Type a) where
  pretty = prettyType 0

instance Pretty TCon where
  pretty tc = pretty (TCon () tc [])


prettyTypeApp :: a -> Int -> [Type a] -> ([Either Doc (Type a)] -> Doc) -> Doc
prettyTypeApp a n xs f
  | null bs   = ty
  | otherwise = parens (ty <+> hsep (map err bs))
  where
  (as,bs)    = splitAt n xs
  ts         = take n (map Right as ++ repeat prettyWild)
  ty         = f ts

  err b      = text "!" <> prettyType 10 b

  prettyWild = Left "_"



