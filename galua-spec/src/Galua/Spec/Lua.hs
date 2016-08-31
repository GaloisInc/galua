module Galua.Spec.Lua where

import Language.Lua.Annotated.Syntax

import Galua.Spec.AST
import Galua.Spec.Parser(SourceRange(..))

type Parsed f = f SourceRange
type TC f     = f [SourceRange]

tPrim :: TCon -> a -> Type a
tPrim tc a = Type { typeAnnot = a, typeCon = tc, typeParams = [] }

tNil :: a -> Type a
tNil = tPrim TNil

tBoolean :: a -> Type a
tBoolean = tPrim TBoolean

tString :: a -> Type a
tString = tPrim TString

tInteger :: a -> Type a
tInteger = tPrim TInteger

tNumber :: a -> Type a
tNumber = tPrim TNumber




type InferM = IO -- XXX


inferExpr :: Parsed Exp -> InferM (TC Type)
inferExpr expr =
  case expr of
    Nil r       -> return (tNil [r])
    Bool r _    -> return (tBoolean [r])
    Number r nt _ ->
      case nt of
        FloatNum -> return (tNumber [r])
        IntNum   -> return (tInteger [r])
          -- XXX: this is OK if `integer` is a subtype of `number`.
          -- If not, we could have this be `integer | number`.
          -- Alternatively, we could use a constraint: `a` where `Num a`

