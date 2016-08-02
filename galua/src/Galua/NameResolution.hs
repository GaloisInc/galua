module Galua.NameResolution (chunkLocations) where

import Language.Lua.Annotated.Syntax
import Language.Lua.PrettyPrinter
import Language.Lua.Annotated.Lexer (SourcePos(..))
import Language.Lua.Annotated.Parser
import Language.Lua.Annotated.Simplify
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad
import Control.Applicative
import Data.Monoid
import Data.Foldable
import qualified Data.Set as Set

chunkLocations :: Block SourcePos -> [Exp SourcePos]
chunkLocations = resolveLocations

class Annotated f => Resolve f where
  resolveLocations :: Alternative m => f SourcePos -> m (Exp SourcePos)

asumMap :: (Alternative f, Foldable t) => (a -> f b) -> t a -> f b
asumMap f = getAlt . foldMap (Alt . f)

instance Resolve Block where
  resolveLocations (Block sp stats mbReturn) =
        asumMap resolveLocations stats
    <|> asumMap (asumMap resolveLocations) mbReturn

instance Resolve Stat where
  resolveLocations s =
    case s of
      Assign _ vs xs ->
                    asumMap resolveLocations vs
                <|> asumMap resolveLocations xs
      FunCall a fc -> resolveLocations fc
      Label {} -> empty
      Break{} -> empty
      Goto{} -> empty
      Do _ b -> resolveLocations b
      While _  l r -> resolveLocations l <|> resolveLocations r
      Repeat _ l r -> resolveLocations l <|> resolveLocations r
      If _ xs ys ->
          asum
          [ z | (x,y) <- xs, z <- [resolveLocations x, resolveLocations y] ]
        <|> asumMap resolveLocations ys
      ForRange _ _ x y z b ->
          resolveLocations x <|>
          resolveLocations y <|>
          asumMap resolveLocations z <|>
          resolveLocations b
      ForIn _ _ xs b ->
             asumMap resolveLocations xs <|>
             resolveLocations b
      FunAssign{} -> empty
      LocalFunAssign{} -> empty
      LocalAssign _ _ b -> asumMap (asumMap resolveLocations) b
      EmptyStat{} -> empty


instance Resolve Exp where
  resolveLocations expr =
    case expr of
      Nil{}          -> pure expr
      Bool{}         -> pure expr
      Number{}       -> pure expr
      String{}       -> pure expr
      Vararg{}       -> pure expr
      EFunDef{}      -> pure expr
      PrefixExp a p  -> resolveLocations p
      TableConst a t -> resolveLocations t
      Binop _ _ l r  -> resolveLocations l <|> resolveLocations r
      Unop _ _ e     -> resolveLocations e

instance Resolve Table where
  resolveLocations (Table _ xs) =
    asumMap resolveLocations xs

instance Resolve TableField where
  resolveLocations field =
    case field of
      ExpField _ l r   -> resolveLocations l <|> resolveLocations r
      NamedField _ _ r -> resolveLocations r
      Field _ r        -> resolveLocations r

instance Resolve PrefixExp where
  resolveLocations p =
    case p of
      PEVar a v      -> pure (PrefixExp a p) <|> resolveLocations v
      PEFunCall a fc -> pure (PrefixExp a p) <|> resolveLocations fc
      Paren a x      -> pure (PrefixExp a p) <|> resolveLocations x

instance Resolve FunCall where
  resolveLocations fc =
    case fc of
      NormalFunCall _ f x -> resolveLocations f <|> resolveLocations x
      MethodCall _ f _ x  -> resolveLocations f <|> resolveLocations x

instance Resolve FunArg where
  resolveLocations fa =
    case fa of
      Args _ xs       -> asumMap resolveLocations xs
      TableArg _ t    -> resolveLocations t
      StringArg a txt -> pure (String a txt)

instance Resolve Var where
  resolveLocations v =
    case v of
      VarName{}        -> empty
      Select _ l r     -> resolveLocations l <|> resolveLocations r
      SelectName _ l _ -> resolveLocations l
