{-# LANGUAGE OverloadedStrings, FlexibleInstances, TypeFamilies #-}
module Galua.Spec.Lua where

import           Prelude hiding (EQ,GT,LT)
import           Language.Lua.Annotated.Syntax hiding (Name)
import qualified Language.Lua.Annotated.Syntax as Lua
import           Text.PrettyPrint
import qualified Data.Text as Text
import           Control.Monad(liftM,ap,unless,zipWithM,forM)
import           Data.Map ( Map )
import qualified Data.Map as Map
import           Data.List(foldl')
import           Data.Maybe(fromMaybe)
import           Data.Text(Text)

import Galua.Spec.AST hiding (Type)
import qualified Galua.Spec.AST as AST
import Galua.Spec.Parser(Parsed, SourceRange(..))
import Galua.Spec.Parser.Lexer((<->))


xxxTODO = error

--------------------------------------------------------------------------------
-- Types and Constraints
--------------------------------------------------------------------------------

data TypeChecked

type instance Annot TypeChecked = SourceRange
type instance TVar TypeChecked  = TV
type Type                       = AST.Type TypeChecked
type LuaName                    = Lua.Name SourceRange

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
          | C_Project Name

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

instance Pretty TV where
  pretty (TV _ x) = "$" <> int x


--------------------------------------------------------------------------------
-- Inference: Statments
--------------------------------------------------------------------------------

-- Track scoping local variables
-- Track scoping of labels
--    A label is in scope in the block where it is defined
--    - it may be shadowed in a sub-block
--    - it is not in scope in nested functions.
-- Types of local variables may change,
-- but not types of globals or free variables (up values)

class InferStmt t where
  inferStmt :: t -> InferM ()


-- local x,y,z = f ()
-- local x,y,z = 1,2
-- local x,y,z = 1, f ()

-- local x,y,z = 1,2



instance InferStmt (Stat SourceRange) where
  inferStmt stmt =
    case stmt of
      Assign r xs es        -> xxxTODO "ASSIGN"

        -- Types of locals may change as long as they are not shared
        -- (i.e. captured by a closure)


      LocalAssign r xs mbEs ->
        do vs <- check xs (fromMaybe [] mbEs)
           mapM_ (uncurry newLocal) vs

        where
        check [] (_ : _) = reportError "Too many expressions in assignment"
                            -- XXX

        check xs [e] =
          do t  <- inferExpr e
             let getV i v =
                   do a <- newTVar (ann v)
                      constraint (c2 (ann e) (C_Get i) t a)
                      return (v,a)
             zipWithM getV [ 0 .. ] xs


        check xs [] = forM xs $ \v -> do a <- newTVar (ann v)
                                         return (v, tMaybe r a)

        check (x:xs) (e:es) =
          do t <- inferExpr e
             a <- newTVar (ann e)
             constraint (c2 r (C_Get 0) t a)
             more <- check xs es
             return ((x,t):more)

      EmptyStat _    -> return ()

      FunCall _ fc   -> inferStmt fc
      Label _ _      -> return ()
      Break r        -> inLoop r
      Goto _ l       -> hasLabel l
      Do r block     -> inferBlock block
      While r e b    -> do t <- inferExpr e
                           loopBody (inferBlock b)
      Repeat r b e   -> do startBlock
                           loopBody (doInferBlock b)
                           t <- inferExpr e
                           endBlock

      -- XXX: Support for "Maybe" via checking for null
      If r alts mb   -> do mapM_ alt alts
                           mapM_ inferBlock mb
        where alt (e,b) = do t <- inferExpr e
                             inferBlock b

      ForRange r x e1 e2 me3 b ->
        do t1 <- numExpr e1
           t2 <- numExpr e2
           t3 <- case me3 of
                   Nothing -> return (tInteger r)
                   Just e3 -> numExpr e3
           a  <- newTVar (ann x)
           constraint (c3 r C_Add t1 t3 a)
           loopBody $ do startBlock
                         newLocal x a
                         doInferBlock b
                         endBlock
        where
        numExpr e = do t <- inferExpr e
                       v <- newTVar (ann e)
                       constraint (c2 r C_Number t v)
                       return v

      ForIn r xs es b -> xxxTODO "ForIn"

      FunAssign r fn fb ->
        do let (sel,isMeth) = funNameToSel fn
           if not isMeth
             then do let r' = ann fb
                     inferStmt (Assign r [sel] [EFunDef r' (FunDef r' fb)])
             else xxxTODO "method declaration"
             -- A method declaratoin should match its declaration
             -- in a class!

             -- this one is a bit tricky because we need to add the
             -- implicit "self" parameter, which has the same type as the
             -- object.
             --
             -- Also, we have to decide if we should allow definitions of new
             -- globals or not: perhaps only at the top-level?
             -- otherwise, we'd have to keep track of what new
             -- functions were defined by a function call!

      LocalFunAssign r x fb ->
        do let r' = ann fb
           t <- inferExpr (EFunDef r' (FunDef r' fb))
           newLocal x t

funNameToSel :: FunName SourceRange -> (Var SourceRange, Bool)
funNameToSel (FunName r0 x s mb) = case mb of
                                     Nothing -> (part1, False)
                                     Just y  -> (gt part1 y, True)
  where
  part1   = foldl' gt (VarName (ann x) x) s
  gt nm f = let r = r0 <-> ann f
            in SelectName r (PEVar r nm) f


instance InferStmt (FunCall SourceRange) where
  inferStmt fc = do _ <- inferExpr fc
                    return ()

inferBlock :: Block SourceRange -> InferM ()
inferBlock b = startBlock >> doInferBlock b >> endBlock

-- | Assumes that start and end block are done by calelr
doInferBlock :: Block SourceRange -> InferM ()
doInferBlock = xxxTODO "XXX"





--------------------------------------------------------------------------------
-- Inference: Expressions
--------------------------------------------------------------------------------

class InferExpr e where
  inferExpr :: e SourceRange -> InferM Type

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

      Vararg r          -> lookupVarArg r

      EFunDef _ f       -> xxxTODO "XXX: function"

      PrefixExp _ pe    -> inferExpr pe
      TableConst _ t    -> inferExpr t

      -- XXX: special cases for:  x /= nil `And` stuff?
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
             Not _          -> return (tBoolean r)
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
           constraint (c2 r (C_Project (importName l)) te res)
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
           constraint (c2 r (C_Project (importName m)) objT fun)
           constraint (c3 r C_Call fun (tTuple (ann as) (objT : ts)) res)
           return res

instance InferExpr Table where
  inferExpr (Table r fs) = xxxTODO "XXX: Table"


inferArgs :: FunArg SourceRange -> InferM [Type]
inferArgs args =
  case args of
    Args _ es     -> mapM inferExpr es
    TableArg _ ta -> do t <- inferExpr ta
                        return [t]
    StringArg r _ -> return [ tString r ]


importName :: LuaName -> Name
importName (Lua.Name x y) = Name { nameRange = x, nameText = y }




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
  , theSpec       :: !(Spec TypeChecked)
  }



runInferM :: Spec TypeChecked -> InferM () -> Either TypeError [Constraint]
runInferM globs (IM f) =
  case f rw of
    Left err    -> Left err
    Right (a,s) -> Right (map (apSubst (rwSubst s)) (rwConstraints s))
  where
  rw = RW { rwSubst       = suEmpty
          , rwConstraints = []
          , rwNextVar     = 0
          , theSpec       = globs
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

lookupVar :: SourceRange -> LuaName -> InferM Type
lookupVar = xxxTODO "XXX"

lookupVarArg :: SourceRange -> InferM Type
lookupVarArg = xxxTODO "XXX"

inLoop :: SourceRange -> InferM ()
inLoop = xxxTODO "XXX"

loopBody :: InferM a -> InferM a
loopBody = xxxTODO "XXX"

hasLabel :: LuaName -> InferM ()
hasLabel = xxxTODO "XXX"

withLabels :: [LuaName] -> InferM a -> InferM a
withLabels = xxxTODO "XXX"

-- | Declare a new local
newLocal :: LuaName -> Type -> InferM ()
newLocal = xxxTODO "XXX"

startBlock :: InferM ()
startBlock = xxxTODO "XXX"

endBlock :: InferM ()
endBlock = xxxTODO "XXX"

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
bindVar :: SourceRange -> TV -> Type -> InferM ()
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
newtype Subst = Subst (Map TV (SourceRange, Type))

suEmpty :: Subst
suEmpty = Subst Map.empty

suExtend :: SourceRange -> TV -> Type -> Subst -> Maybe Subst
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

