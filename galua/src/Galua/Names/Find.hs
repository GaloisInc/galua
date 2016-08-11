{-# LANGUAGE FlexibleInstances#-}
-- | Identify expressions that refer to interesting names of things.
module Galua.Names.Find (chunkLocations) where

import qualified Language.Lua.Syntax as Lua
import Language.Lua.Annotated.Syntax
import Language.Lua.Annotated.Lexer (SourcePos(..))
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
import Text.Show.Pretty(pPrint)

test = do res <- parseFile "test.lua"
          case res of
            Left x -> fail (show x)
            Right a -> pPrint (chunkLocations a)


chunkLocations :: Block SourcePos -> [LocatedExprName]
chunkLocations b = case resolve b of
                     M _ out -> out []



data ExprName = EIdent    Lua.Name
              | EString   ByteString
              | ENumber   Number
              | EBool     Bool
              | EUni Lua.Unop ExprName
              | EVarArg
              | ESelectFrom ExprName ExprName  -- value, index
                deriving (Show,Eq)

data LocatedExprName = LocatedExprName { exprPos :: SourcePos
                                       , exprName :: ExprName
                                       } deriving (Show,Eq)


data M a = M (Maybe a) ([LocatedExprName] -> [LocatedExprName])

instance Functor M where
  fmap = liftA

instance Applicative M where
  pure a                  = M (Just a) id
  M f out1 <*> M x out2   = M (f <*> x) (out1 . out2)

emit :: SourcePos -> M ExprName -> M ExprName
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


instance Resolve (Block SourcePos) where
  resolve (Block _ stats mbReturn) = resolve (stats, mbReturn)

instance Resolve (Stat SourcePos) where
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
      FunAssign{}           -> ignore
      LocalFunAssign{}      -> ignore
      LocalAssign _ xs b    -> resolve (xs,b)
      EmptyStat{}           -> ignore

instance Resolve (Exp SourcePos) where
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
      EFunDef{}      -> ignore  -- XXX: Maybe we can do something about this?
      PrefixExp _ p  -> resolve p
      TableConst _ t -> resolve t
      Binop _ _ l r  -> resolve (l,r)
      Unop _ i e     -> EUni (sUnop i) <$> resolve e

instance Resolve (Table SourcePos) where
  resolve (Table _ xs) = resolve xs

instance Resolve (TableField SourcePos) where
  resolve field =
    case field of
      ExpField _ l r   -> resolve (l,r)
      NamedField _ _ r -> ignore <* resolve r
      Field _ r        -> ignore <* resolve r

instance Resolve (PrefixExp SourcePos) where
  resolve p =
    case p of
      PEVar _ v      -> resolve v
      PEFunCall _ fc -> resolve fc
      Paren _ x      -> resolve x

instance Resolve (FunCall SourcePos) where
  resolve fc =
    case fc of
      NormalFunCall _ f x -> resolve (f,x)
      MethodCall _ obj (Name a meth) arg ->
              emit a (sel <$> resolve obj) <* resolve arg
        where sel o = ESelectFrom o (EString (encodeUtf8 meth))

instance Resolve (FunArg SourcePos) where
  resolve fa =
    case fa of
      Args _ xs     -> resolve xs
      TableArg _ t  -> resolve t
      StringArg _ _ -> ignore

instance Resolve (Var SourcePos) where
  resolve v =
    case v of
      VarName _ x      -> resolve x
      Select a l r     -> emit a (ESelectFrom <$> resolve l <*> resolve r)
      SelectName _ l (Name a x) -> emit a (sel <$> resolve l)
        where sel o = ESelectFrom o (EString (encodeUtf8 x))

instance Resolve (Name SourcePos) where
  resolve (Name a x) = emit a (pure (EIdent (Lua.Name x)))

