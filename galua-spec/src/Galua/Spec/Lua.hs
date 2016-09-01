{-# LANGUAGE OverloadedStrings #-}
module Galua.Spec.Lua where

import           Prelude hiding (EQ,GT,LT)
import           Language.Lua.Annotated.Syntax
import           Text.PrettyPrint
import qualified Data.Text as Text
import           Control.Monad(liftM,ap)
import           Data.Map ( Map )
import qualified Data.Map as Map

import Galua.Spec.AST (Pretty(..))
import Galua.Spec.AST (TCon(..))
import Galua.Spec.Parser(SourceRange(..))


xxxTODO = error

--------------------------------------------------------------------------------
-- Types and Constraints
--------------------------------------------------------------------------------

data Type   = TCon !SourceRange !TCon ![Type]
            | TVar !TVar
              deriving Show

data TVar   = TV !SourceRange !Int
              deriving (Show)

instance Eq TVar where
  TV _ x == TV _ y = x == y

instance Ord TVar where
  compare (TV _ x) (TV _ y) = compare x y


type Parsed f = f SourceRange


tPrim :: TCon -> SourceRange -> Type
tPrim tc a = TCon a tc []

tNil :: SourceRange -> Type
tNil = tPrim TNil

tBoolean :: SourceRange -> Type
tBoolean = tPrim TBoolean

tString :: SourceRange -> Type
tString = tPrim TString

tInteger :: SourceRange -> Type
tInteger = tPrim TInteger

tNumber :: SourceRange -> Type
tNumber = tPrim TNumber

tTuple :: SourceRange -> [Type] -> Type
tTuple a ts = TCon a (TTuple (length ts)) ts


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
          | C_Project (Name SourceRange)

          | C_Call

          | C_EqType

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

      (C_EqType,[a,b])    -> a <+> "=" <+> b

    where
    returns a b = a <+> "=" <+> b

    infix3 x a b c = returns (a <+> x <+> b) c
    infix2 x a b   = a <+> x <+> b
    prefix1 x a    = x <> a

instance Pretty (Name a) where
  pretty (Name _ x) = text (Text.unpack x)

instance Pretty Type where
  pretty = prettyType 0

instance Pretty TVar where
  pretty (TV _ x) = "$" <> int x

prettyType :: Int -> Type -> Doc
prettyType _ (TVar x) = pretty x
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

prettyTypeApp :: a -> Int -> [Type] -> ([Either Doc Type] -> Doc) -> Doc
prettyTypeApp a n xs f
  | null bs   = ty
  | otherwise = parens (ty <+> hsep (err <$> bs))
  where
  (as,bs)    = splitAt n xs
  ts         = take n (map Right as ++ repeat prettyWild)
  ty         = f ts

  err b      = text "!" <> prettyType 10 b

  prettyWild = Left "_"




--------------------------------------------------------------------------------
-- Inference
--------------------------------------------------------------------------------

class InferExpr e where
  inferExpr :: Parsed e -> InferM Type

instance InferExpr Exp where
  inferExpr expr =
    case expr of
      Nil r       -> return (tNil r)
      Bool r _    -> return (tBoolean r)
      Number r nt _ ->
        case nt of
          FloatNum -> return (tNumber r)
          IntNum   -> return (tInteger r)
      String r _ -> return (tString r)

      Vararg r          -> lookupVarArg r

      EFunDef _ f       -> xxxTODO "XXX: function"

      PrefixExp _ pe    -> inferExpr pe
      TableConst _ t    -> inferExpr t
      Binop r op e1 e2  ->
        do t1 <- inferExpr e1
           t2 <- inferExpr e2
           let bool = tBoolean r

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
             Not _          -> do let bool = tBoolean r
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
           constraint (c3 r C_Call t (tTuple (ann as) ts) res)
           return res

      MethodCall r pe m as ->
        do objT <- inferExpr pe
           ts   <- inferArgs as
           res  <- newTVar r
           fun  <- newTVar (ann m)
           constraint (c2 r (C_Project m) objT fun)
           constraint (c3 r C_Call fun (tTuple (ann as) (objT : ts)) res)
           return res

instance InferExpr Table where
  inferExpr (Table r fs) = xxxTODO "XXX: Table"


inferArgs :: Parsed FunArg -> InferM [Type]
inferArgs args =
  case args of
    Args _ es     -> mapM inferExpr es
    TableArg _ ta -> do t <- inferExpr ta
                        return [t]
    StringArg r _ -> return [ tString r ]





--------------------------------------------------------------------------------
-- Inference Monad
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



--------------------------------------------------------------------------------

-- | Generate a fresh type variable, with the given suggested name.
newTVar :: SourceRange -> InferM Type
newTVar r = IM (\s -> let i  = rwNextVar s
                          s1 = s { rwNextVar = i + 1 }
                          x  = TVar (TV r i)
                      in s1 `seq` Right (x, s1))

reportError :: TypeError -> InferM a
reportError e = IM (\_ -> Left e)


-- | Add a constraint.
constraint :: Constraint -> InferM ()
constraint (Constraint r C_EqType [t1,t2]) = unify r t1 t2
constraint c = delay c

-- | Store constraint to try solve later as we don't have enough
-- information as to how to do it just yet.
delay :: Constraint -> InferM ()
delay c = IM (\s -> let s1 = s { rwConstraints = c : rwConstraints s1 }
                    in s1 `seq` Right ((),s1))

lookupVar :: SourceRange -> Parsed Name -> InferM Type
lookupVar = undefined

lookupVarArg :: SourceRange -> InferM Type
lookupVarArg = undefined

-- | Apply the accumulated substitution to sometihng.
zonk :: ApSubst t => t -> InferM t
zonk t = IM (\s -> let t1 = apSubst (rwSubst s) t
                   in t1 `seq` Right (t1, s))


-- | make two types the same.  The location points to the place
-- in the source code that forced the types to be the same.
unify :: SourceRange -> Type -> Type -> InferM ()
unify r t1' t2' =
  do z <- zonk (t1',t2')
     case z of
       (TVar x, t) -> bindVar r x t
       (t, TVar x) -> bindVar r x t
       (TCon _ c1 ts1, TCon _ c2 ts2)
          | c1 == c2  -> unifyMany r ts1 ts2
          | otherwise -> reportError errMsg
  where
  errMsg = "Type mismatch" -- XXX

unifyMany :: SourceRange -> [Type] -> [Type] -> InferM ()
unifyMany _ [] []             = return ()
unifyMany r (x : xs) (y : ys) = unify r x y >> unifyMany r xs ys
unifyMany _ _ _ = reportError "Arity mismatch" -- XXX

-- | Replace a variable by a type.  Assumes that the substitution
-- has already been applied to both.
bindVar :: SourceRange -> TVar -> Type -> InferM ()
bindVar r x t = IM (\s -> case suExtend r x t (rwSubst s) of
                            Just su ->
                              let s1 = s { rwSubst = su }
                              in s1 `seq` Right ((),s1)
                            Nothing -> Left "Recursive type" -- XXX
                 )

--------------------------------------------------------------------------------
-- Substitutions
--------------------------------------------------------------------------------

-- | The source range is the location in the source code that cause the
-- variable to be matched with the type.
newtype Subst = Subst (Map TVar (SourceRange, Type))

suEmpty :: Subst
suEmpty = Subst Map.empty

suExtend :: SourceRange -> TVar -> Type -> Subst -> Maybe Subst
suExtend _ x (TVar y) su | x == y = Just su
suExtend _ x t _ | occursIn t = Nothing
  where occursIn (TVar y)      = x == y
        occursIn (TCon _ _ ts) = occursIn `any` ts
suExtend r x t (Subst mp) = Just (Subst (Map.insert x new mp1))
  where
  new   = (r,t)
  small = Subst (Map.singleton x new)
  mp1   = (\(r,t) -> (r, apSubst small t)) <$> mp


class ApSubst t where
  apSubst :: Subst -> t -> t

instance ApSubst a => ApSubst [a] where
  apSubst su = map (apSubst su)

instance (ApSubst a, ApSubst b) => ApSubst (a,b) where
  apSubst su (a,b) = (apSubst su a, apSubst su b)

instance ApSubst Type where
  apSubst su@(Subst mp) ty =
    case ty of
      TCon r tc ts -> TCon r tc (apSubst su ts)
      TVar x -> case Map.lookup x mp of
                  Nothing     -> ty
                  Just (_,t1) -> t1

instance ApSubst Constraint where
  apSubst su (Constraint r c ts) = Constraint r c (apSubst su ts)

