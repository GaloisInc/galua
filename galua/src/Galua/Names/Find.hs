{-# LANGUAGE FlexibleInstances#-}
-- | Identify expressions that refer to interesting names of things.
module Galua.Names.Find
  ( chunkLocations
  , LocatedExprName(..)
  , ExprName(..)
  , ppExprName
  , strictPrefix
  ) where

import qualified Language.Lua.Syntax as Lua
import Language.Lua.Annotated.Syntax
import Language.Lua.Annotated.Lexer (SourceRange(..))
import Language.Lua.Annotated.Simplify
import Language.Lua.StringLiteral(interpretStringLiteral)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import Data.Text.Encoding(encodeUtf8)
import Control.Applicative
import Data.Foldable

import Galua.Number

import Language.Lua.Annotated.Parser

{-
import Text.Show.Pretty(pPrint)
import Debug.Trace

test = do res <- parseFile "test.lua"
          case res of
            Left x -> fail (show x)
            Right a -> pPrint (chunkLocations a)
-}

chunkLocations :: Block SourceRange -> [LocatedExprName]
chunkLocations b = case resolve b of
                     M _ out -> out []



data ExprName = EIdent    Lua.Name
              | ESelectFrom ExprName ExprIx -- value, index
              | EVarArg

              -- These appear only in the indexes of a selection
              | EString   ByteString
              | ENumber   Number
              | EBool     Bool
              | EUni Lua.Unop ExprName
                deriving (Show,Eq)

strictPrefix :: ExprName -> ExprName -> Bool
strictPrefix e (ESelectFrom e1 ix) = e == e1 || strictPrefix e e1
strictPrefix _ _                   = False




type ExprIx = ExprName

ppExprName :: ExprName -> String
ppExprName x =
  case x of
    EIdent (Lua.Name x) -> Text.unpack x
    EString x           -> show x
    ENumber (Int x)     -> show x
    ENumber (Double x)  -> show x
    EBool x             -> show x
    EUni op e           -> opTxt ++ ppExprName e
      where opTxt = case op of
                      Lua.Neg        -> "-"
                      Lua.Not        -> "not"
                      Lua.Len        -> "#"
                      Lua.Complement -> "~"
    EVarArg             -> "..."
    ESelectFrom a b     -> ppExprName a ++ "[" ++ ppExprName b ++ "]"


data LocatedExprName = LocatedExprName { exprPos  :: SourceRange
                                       , exprName :: ExprName
                                       } deriving (Show,Eq)


data M a = M (Maybe a) ([LocatedExprName] -> [LocatedExprName])

instance Functor M where
  fmap = liftA

instance Applicative M where
  pure a                  = M (Just a) id
  M f out1 <*> M x out2   = M (f <*> x) (out1 . out2)

emit :: SourceRange -> M ExprName -> M ExprName
emit src (M mb out) =
  case  mb of
    Nothing -> M Nothing out
    Just e  ->
      let le = LocatedExprName { exprPos = src, exprName = e }
      in M (Just e) ((le :) . out)


ignore :: M a
ignore = M Nothing id



class Resolve a where
  resolve :: a -> M ExprName

instance Resolve a => Resolve [a] where
  resolve xs = ignore <* traverse_ resolve xs

instance Resolve a => Resolve (Maybe a) where
  resolve xs = ignore <* traverse_ resolve xs

instance (Resolve a, Resolve b) => Resolve (a,b) where
  resolve (x,y) = ignore <* resolve x <* resolve y


instance Resolve (Block SourceRange) where
  resolve (Block _ stats mbReturn) = resolve (stats, mbReturn)

instance Resolve (Stat SourceRange) where
  resolve s =
    case s of
      Assign _ vs xs        -> resolve (vs,xs)
      FunCall _ fc          -> resolve fc
      Label {}              -> ignore
      Break{}               -> ignore
      Goto{}                -> ignore
      Do _ b                -> resolve b
      While _  l r          -> resolve (l,r)
      Repeat _ l r          -> resolve (l,r)
      If _ xs ys            -> resolve (xs,ys)
      ForRange _ i x y z b  -> resolve (i,(x,(y,(z,b))))
      ForIn _ is xs b       -> resolve (is,(xs,b))
      FunAssign _ _ x       -> resolve x
      LocalFunAssign _ _ b  -> resolve b
      LocalAssign _ xs b    -> resolve (xs,b)
      EmptyStat{}           -> ignore

instance Resolve (Exp SourceRange) where
  resolve expr =
    case expr of
      Nil{}          -> ignore
      Bool _ b       -> pure (EBool b)
      Number _ num   -> case parseNumber (Text.unpack num) of
                          Just n -> pure (ENumber n)
                          Nothing -> ignore
      String _ txt   -> case interpretStringLiteral (Text.unpack txt) of
                          Just ok -> pure (EString (LBS.toStrict ok))
                          Nothing -> ignore
      Vararg a       -> ignore <* emit a (pure EVarArg)
      EFunDef _ b    -> resolve b
      PrefixExp _ p  -> resolve p
      TableConst _ t -> resolve t
      Binop _ _ l r  -> resolve (l,r)
      Unop _ i e     -> EUni (sUnop i) <$> resolve e

instance Resolve (FunDef SourceRange) where
  resolve (FunDef _ x) = resolve x

instance Resolve (FunBody SourceRange) where
  resolve (FunBody _ xs hasVa block) = resolve block


instance Resolve (Table SourceRange) where
  resolve (Table _ xs) = resolve xs

instance Resolve (TableField SourceRange) where
  resolve field =
    case field of
      ExpField _ l r   -> resolve (l,r)
      NamedField _ _ r -> ignore <* resolve r
      Field _ r        -> ignore <* resolve r

instance Resolve (PrefixExp SourceRange) where
  resolve p =
    case p of
      PEVar _ v      -> resolve v
      PEFunCall _ fc -> resolve fc
      Paren _ x      -> resolve x

instance Resolve (FunCall SourceRange) where
  resolve fc =
    case fc of
      NormalFunCall _ f x -> resolve (f,x)
      MethodCall _ obj (Name a meth) arg ->
              emit a (sel <$> resolve obj) <* resolve arg
        where sel o = ESelectFrom o (EString (encodeUtf8 meth))

instance Resolve (FunArg SourceRange) where
  resolve fa =
    case fa of
      Args _ xs     -> resolve xs
      TableArg _ t  -> resolve t
      StringArg _ _ -> ignore

instance Resolve (Var SourceRange) where
  resolve v =
    case v of
      VarName _ x      -> resolve x
      Select a l r     -> emit a (ESelectFrom <$> resolve l <*> resolve r)
      SelectName _ l (Name a x) -> emit a (sel <$> resolve l)
        where sel o = ESelectFrom o (EString (encodeUtf8 x))

instance Resolve (Name SourceRange) where
  resolve (Name a x) = emit a (pure (EIdent (Lua.Name x)))

