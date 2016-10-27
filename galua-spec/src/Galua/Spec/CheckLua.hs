{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Galua.Spec.CheckLua where

import Text.PrettyPrint
import MonadLib
import MonadLib.Derive
import qualified Language.Lua.Annotated.Syntax as Lua
import qualified Language.Lua.PrettyPrinter as Lua
import qualified Language.Lua.Annotated.Simplify as Lua

import Galua.Spec.Parser(SourceRange(..))

import qualified Galua.Spec.AST as AST
import Galua.Spec.AST hiding (Type,Name)
import Galua.Spec.CFG


--------------------------------------------------------------------------------
-- Types and Constraints
--------------------------------------------------------------------------------

data TypeChecked

type instance Annot TypeChecked = SourceRange
type instance TVar TypeChecked  = TV
type Type                       = AST.Type TypeChecked

data TV = TV !SourceRange !Int

instance Eq TV where
  TV _ x == TV _ y = x == y

instance Ord TV where
  compare (TV _ x) (TV _ y) = compare x y

tPrim :: TCon -> SourceRange -> Type
tPrim tc a = TCon a tc []

tBoolean :: SourceRange -> Type
tBoolean = tPrim TBoolean

tString :: SourceRange -> Type
tString = tPrim TString

tInteger :: SourceRange -> Type
tInteger = tPrim TInteger

tNumber :: SourceRange -> Type
tNumber = tPrim TNumber

-- | This is for function inputs and outputs
tTuple :: SourceRange -> [Type] -> Type
tTuple a ts = TCon a (TTuple (length ts)) ts

tMaybe :: SourceRange -> Type -> Type
tMaybe r t = TCon r TMaybe [t]


data Constraint = Constraint SourceRange Ctr [Type]

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
          | C_Project Selector

          | C_Call

          | C_Number

          | C_Get !Int -- acces a particular function result or argument

          | C_EqType

c1 :: SourceRange -> Ctr -> Type -> Constraint
c1 r c t = Constraint r c [t]

c2 :: SourceRange -> Ctr -> Type -> Type -> Constraint
c2 r c t1 t2 = Constraint r c [t1,t2]

c3 :: SourceRange -> Ctr -> Type -> Type -> Type -> Constraint
c3 r c t1 t2 t3 = Constraint r c [t1,t2,t3]

cEqType :: SourceRange -> Type -> Type -> Constraint
cEqType r = c2 r C_EqType


--------------------------------------------------------------------------------
-- Pretty Printing
--------------------------------------------------------------------------------

instance Pretty Constraint where
  pretty (Constraint _ tc ts) =
    case (tc, prettyType 1 <$> ts) of
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

      (C_Number, [a,b])  -> returns ("to_number" <+> a) b

      (C_Get n, [a,b])   -> returns ("get_" <> int n <+> a) b

      (C_EqType,[a,b])   -> a <+> "=" <+> b

    where
    returns a b = a <+> "=" <+> b

    infix3 x a b c = returns (a <+> x <+> b) c
    infix2 x a b   = a <+> x <+> b
    prefix1 x a    = x <> a

instance Pretty Selector where
  pretty = text . show . Lua.pprint . Lua.sName

instance Pretty TV where
  pretty (TV _ x) = "$" <> int x

--------------------------------------------------------------------------------

newtype InferM a = IM {unIM :: ReaderT    RO (
                               StateT     RW (
                               ExceptionT TypeError
                               Id)) a }

data RO = RO
  {
  }

data RW = RW
  { rwNextVar     :: !Int
  , rwConstraints :: ![Constraint]
  }

data TypeError = TypeError


instance Functor InferM where
  fmap = liftM

instance Applicative InferM where
  pure  = derive_pure (Iso IM unIM)
  (<*>) = ap

instance Monad InferM where
  (>>=) = derive_bind (Iso IM unIM)

-- | Generate a fresh type variable.  The range identifies the location
-- of the entity whose type this variable stands for.
newTVar :: SourceRange -> InferM Type
newTVar r = IM $ sets $ \s -> let i  = rwNextVar s
                                  x  = TVar (TV r i)
                              in (x, s { rwNextVar = i + 1 })

-- | Add a constraint.
constraint :: Constraint -> InferM ()
-- constraint (Constraint r C_EqType [t1,t2]) = unify r t1 t2
constraint c = delay c

-- | Store constraint to try solve later as we don't have enough
-- information as to how to do it just yet.
delay :: Constraint -> InferM ()
delay c = IM $ sets_ $ \s -> s { rwConstraints = c : rwConstraints s }

-- | Lookup a name
lookupVar :: Name -> InferM Type
lookupVar = undefined





--------------------------------------------------------------------------------
class InferExpr a where
  inferExpr :: a -> InferM Type

class InferStmt a where
  inferStmt :: a -> InferM ()

{-
instance InferStmt Stat where
  inferStmt stmt =
    case stmt of
    Assign          Annot [Var]   [Exp]
  | LocalAssign     Annot [Name]  (Maybe [Exp])
  | FunAssign       Annot FunName FunBody
  | FunCallStat FunCall
  | AssertIsNumber Exp
  | AssumeIsNumber Name
-}


instance InferExpr Exp where
  inferExpr expr =
    case expr of

      Nil r       -> do a <- newTVar r
                        return (tMaybe r a)

      Bool r _    -> return (tBoolean r)

      Number r nt _ ->
        case nt of
          FloatNum -> return (tNumber r)
          IntNum   -> return (tInteger r)

      String r _ -> return (tString r)

{-
      Vararg r          -> lookupVarArg r

      EFunDef _ f       -> xxxTODO "XXX: function"

      PrefixExp _ pe    -> inferExpr pe
      TableConst _ t    -> inferExpr t
-}

      Binop r op e1 e2  ->
        do t1 <- inferExpr e1
           t2 <- inferExpr e2
           let bool = tBoolean r

               overload c = do res <- newTVar r
                               constraint (c3 r c t1 t2 res)
                               return res


               relOp c  = do constraint (c2 r c t1 t2)
                             return bool

               boolOp   = do constraint (c2 r C_EqType t1 t2)
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

             Lua.LT _ -> relOp C_LT
             LTE _    -> relOp C_LTE
             Lua.GT _ -> relOp C_GT
             GTE _    -> relOp C_GTE
             Lua.EQ _ -> relOp C_EQ
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
             Not _          -> return (tBoolean r)
             Len _          -> overload C_Len
             Complement _   -> overload C_Complement



instance InferExpr PrefixExp where
  inferExpr expr =
    case expr of
      PEVar x      -> inferExpr x
      PEFunCall fc -> inferExpr fc
      Paren e      -> inferExpr e

instance InferExpr Var where
  inferExpr expr =
    case expr of
      VarName x -> lookupVar x

      -- Arrays / maps
      Select r e i ->
        do te  <- inferExpr e
           ti  <- inferExpr i
           res <- newTVar r
           constraint (c3 r C_Select te ti res)
           return res

      -- Records / modules
      SelectName r e l ->
        do te <- inferExpr e
           res <- newTVar r
           constraint (c2 r (C_Project l) te res)
           return res

instance InferExpr FunCall where
  inferExpr (FunCall r pe mb es) =
    case mb of
      Nothing ->
        do t   <- inferExpr pe
           ts  <- mapM inferExpr es
           res <- newTVar r
           constraint (c3 r C_Call t (tTuple rng ts) res)
           return res

      Just m ->
        do objT <- inferExpr pe
           ts   <- mapM inferExpr es
           res  <- newTVar r
           fun  <- newTVar (Lua.ann m)
           constraint (c2 r (C_Project m) objT fun)
           constraint (c3 r C_Call fun (tTuple rng (objT : ts)) res)
           return res

    where
    rng = case es of
            [] -> r
            _  -> let e1 = annot (head es)
                      e2 = annot (last es)
                  in SourceRange { sourceFrom = sourceFrom e1
                                 , sourceTo = sourceTo e2
                                 }





