{-# LANGUAGE OverloadedStrings #-}
module Galua.Spec.Lua where

import Prelude hiding (EQ,GT,LT)
import Language.Lua.Annotated.Syntax
import Text.PrettyPrint
import qualified Data.Text as Text
import Control.Monad(liftM,ap)

import Galua.Spec.AST (Pretty(..),prettyType)
import qualified Galua.Spec.AST as Spec
import Galua.Spec.AST (Type(..), TCon(..))
import Galua.Spec.Parser(SourceRange(..))

type Parsed f = f SourceRange
type TC f     = f [SourceRange]

tPrim :: TCon -> a -> Type a
tPrim tc a = TCon a tc []

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

tTuple :: a -> [Type a] -> Type a
tTuple a ts = TCon a (TTuple (length ts)) ts

--------------------------------------------------------------------------------

data Constraint = Constraint SourceRange Ctr [TC Type]
data Ctr  = C_Add
          | C_Sub
          | C_Mul
          | C_Div
          | C_Exp
          | C_Mod
          | C_Concat
          | C_IDiv
          | C_ShiftL
          | C_ShiftR
          | C_BAnd
          | C_BOr
          | C_BXor

          | C_LT
          | C_LTE
          | C_GT
          | C_GTE
          | C_EQ
          | C_NEQ

          | C_Len
          | C_Neg
          | C_Complement

          | C_Select
          | C_Project (Name SourceRange)

          | C_Call

          | C_EqType

c2 :: SourceRange -> Ctr -> TC Type -> TC Type -> Constraint
c2 r c t1 t2 = Constraint r c [t1,t2]

c3 :: SourceRange -> Ctr -> TC Type -> TC Type -> TC Type -> Constraint
c3 r c t1 t2 t3 = Constraint r c [t1,t2,t3]

cEqType :: SourceRange -> TC Type -> TC Type -> Constraint
cEqType r = c2 r C_EqType

instance Pretty Constraint where
  pretty (Constraint _ tc ts) =
    case (tc, map (prettyType 1) ts) of
      (C_Add, [a,b,c])    -> infix3 "+" a b c
      (C_Sub, [a,b,c])    -> infix3 "-" a b c
      (C_Mul, [a,b,c])    -> infix3 "*" a b c
      (C_Div, [a,b,c])    -> infix3 "/" a b c
      (C_Exp, [a,b,c])    -> infix3 "^" a b c
      (C_Mod, [a,b,c])    -> infix3 "%" a b c
      (C_Concat, [a,b,c]) -> infix3 ".." a b c
      (C_IDiv, [a,b,c])   -> infix3 "//" a b c
      (C_ShiftL, [a,b,c]) -> infix3 "<<" a b c
      (C_ShiftR, [a,b,c]) -> infix3 ">>" a b c
      (C_BAnd, [a,b,c])   -> infix3 "&" a b c
      (C_BOr, [a,b,c])    -> infix3 "|" a b c
      (C_BXor, [a,b,c])   -> infix3 "~" a b c

      (C_LT, [a,b])       -> infix2 "<" a b
      (C_LTE, [a,b])      -> infix2 "<=" a b
      (C_GT, [a,b])       -> infix2 ">" a b
      (C_GTE, [a,b])      -> infix2 ">=" a b
      (C_EQ, [a,b])       -> infix2 "==" a b
      (C_NEQ, [a,b])      -> infix2 "~=" a b

      (C_Len,[a])         -> prefix1 "#" a
      (C_Neg,[a])         -> prefix1 "-" a
      (C_Complement ,[a]) -> prefix1 "~" a

      (C_Select, [a,_,c]) -> returns (a <> brackets (pretty (ts !! 1))) c
      (C_Project x, [a,b])-> returns (a <> "." <> pretty x) b

      (C_Call, [a,_,c])   -> returns (a <> parens (pretty (ts !! 1))) c

      (C_EqType,[a,b])    -> a <+> "=" <+> b

    where
    returns a b = a <+> "=" <+> b

    infix3 x a b c = returns (a <+> x <+> b) c
    infix2 x a b   = a <+> x <+> b
    prefix1 x a    = x <> a

instance Pretty (Name a) where
  pretty (Name _ x) = text (Text.unpack x)

--------------------------------------------------------------------------------

class InferExpr e where
  inferExpr :: Parsed e -> InferM (TC Type)

instance InferExpr Exp where
  inferExpr expr =
    case expr of
      Nil r       -> return (tNil [r])
      Bool r _    -> return (tBoolean [r])
      Number r nt _ ->
        case nt of
          FloatNum -> return (tNumber [r])
          IntNum   -> return (tInteger [r])
      String r _ -> return (tString [r])

      Vararg r          -> lookupVarArg r

      EFunDef _ f       -> error "XXX: function"

      PrefixExp _ pe    -> inferExpr pe
      TableConst _ t    -> inferExpr t
      Binop r op e1 e2  ->
        do t1 <- inferExpr e1
           t2 <- inferExpr e2
           let bool = tBoolean [r]

               overload c = do res <- newTVar r
                               constraint (c3 r c t1 t2 res)
                               return res


               relOp c  = do constraint (c2 r c t1 t2)
                             return bool

               boolOp   = do constraint (cEqType r t1 bool)
                             constraint (cEqType r t2 bool)
                             return bool

           case op of
             Add _    -> overload C_Add
             Sub _    -> overload C_Sub
             Mul _    -> overload C_Mul
             Div _    -> overload C_Div
             Exp _    -> overload C_Exp
             Mod _    -> overload C_Mod
             Concat _ -> overload C_Concat
             IDiv _   -> overload C_IDiv
             ShiftL _ -> overload C_ShiftL
             ShiftR _ -> overload C_ShiftR
             BAnd _   -> overload C_BAnd
             BOr _    -> overload C_BOr
             BXor _   -> overload C_BXor

             LT _     -> relOp C_LT
             LTE _    -> relOp C_LTE
             GT _     -> relOp C_GT
             GTE _    -> relOp C_GTE
             EQ _     -> relOp C_EQ
             NEQ _    -> relOp C_NEQ

             And _    -> boolOp
             Or _     -> boolOp


      Unop r op e ->
        do t <- inferExpr e
           let overload c = do do res <- newTVar r
                                  constraint (c2 r c t res)
                                  return res
           case op of
             Neg _          -> overload C_Neg
             Not _          -> do let bool = tBoolean [r]
                                  constraint (cEqType r t bool)
                                  return bool
             Len _          -> overload C_Len
             Complement _   -> overload C_Complement


instance InferExpr PrefixExp where
  inferExpr expr =
    case expr of
      PEVar _ x      -> inferExpr x
      PEFunCall _ fc -> inferExpr fc
      Paren _ e      -> inferExpr e

instance InferExpr Var where
  inferExpr expr =
    case expr of
      VarName r x -> lookupVar r x

      Select r e i ->
        do te  <- inferExpr e
           ti  <- inferExpr i
           res <- newTVar r
           constraint (c3 r C_Select te ti res)
           return res

      SelectName r e l ->
        do te <- inferExpr e
           res <- newTVar r
           constraint (c2 r (C_Project l) te res)
           return res

instance InferExpr FunCall where
  inferExpr expr =
    case expr of
      NormalFunCall r pe as ->
        do t   <- inferExpr pe
           ts  <- inferArgs as
           res <- newTVar r
           constraint (c3 r C_Call t (tTuple [ann as] ts) res)
           return res

      MethodCall r pe m as ->
        do objT <- inferExpr pe
           ts   <- inferArgs as
           res  <- newTVar r
           fun  <- newTVar (ann m)
           constraint (c2 r (C_Project m) objT fun)
           constraint (c3 r C_Call fun (tTuple [ann as] (objT : ts)) res)
           return res

instance InferExpr Table where
  inferExpr (Table r fs) = error "XXX: Table"


inferArgs :: Parsed FunArg -> InferM [TC Type]
inferArgs args =
  case args of
    Args _ es     -> mapM inferExpr es
    TableArg _ ta -> do t <- inferExpr ta
                        return [t]
    StringArg r _ -> return [ tString [r] ]

--------------------------------------------------------------------------------

newtype InferM a = IM (RW -> Either TypeError (a,RW))

type TypeError = Doc

instance Functor InferM where
  fmap = liftM

instance Applicative InferM where
  pure a = IM (\s -> Right (a,s))
  (<*>)  = ap

instance Monad InferM where
  IM m >>= f = IM (\s -> case m s of
                           Left err -> Left err
                           Right (a,s1) -> let IM m1 = f a
                                           in m1 s1)



data RW = RW
  { rwSubst       :: !Subst
  , rwConstraints :: ![Constraint]
  , rwNextVar     :: !Int
  }

type Subst = Int -- XXX


-- | Generate a fresh type variable, with the given suggested name.
newTVar :: SourceRange -> InferM (TC Type)
newTVar nm = undefined {-IM (\s -> let i  = rwNextVar s
                           s1 = s { rwNextVar = i + 1 }
                           x  = TVar (TV i nm)
                       in s1 `seq` Right (x, s1))-}


constraint :: Constraint -> InferM ()
constraint = undefined

lookupVar :: SourceRange -> Parsed Name -> InferM (TC Type)
lookupVar = undefined

lookupVarArg :: SourceRange -> InferM (TC Type)
lookupVarArg = undefined


