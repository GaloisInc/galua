{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Galua.Spec.CFG where

import           Language.Lua.Annotated.Parser(SourceRange)
import qualified Language.Lua.Annotated.Syntax as Lua
import           Data.Map ( Map )
import qualified Data.Map as Map
import           Data.Vector ( Vector )
import qualified Data.Vector as Vector
import           MonadLib hiding (Label())
import           MonadLib.Derive
import           Data.Text ( Text )
import qualified Data.Text as Text

type Annot      = SourceRange

data Name       = Name { nameId   :: !Int
                          -- ^ Unique identifier (XXX, maybe more structure)
                       , nameType :: !NameT -- ^ What sort of name is this
                       , nameOrig :: !(Lua.Name Annot) -- ^ Original name
                       }

instance Eq Name where
  x == y = nameId x == nameId y

instance Ord Name where
  compare x y = compare (nameId x) (nameId y)

data NameT      = LocalName | UpvalueName | GloabalName

type Selector   = Lua.Name Annot

data FunName    = FunName Annot Name [Selector] (Maybe Selector)

data Var        = VarName Name                        -- ^ variable
                | Select Annot PrefixExp Exp          -- ^ table[exp]
                | SelectName Annot PrefixExp Selector -- ^ table.variable

data PrefixExp  = PEVar Var
                | PEFunCall FunCall
                | Paren Exp

data Exp        = Nil     Annot
                | Bool    Annot Bool
                | Number  Annot Lua.NumberType Text -- XXX: Parse?
                | String  Annot Text
                | Vararg  Annot
                | Binop   Annot (Lua.Binop Annot) Exp Exp
                | Unop    Annot (Lua.Unop Annot) Exp

                | EFunDef     FunBody
                | PrefixExp   PrefixExp
                | TableConst  Table

eVar :: Name -> Exp
eVar x = PrefixExp (PEVar (VarName x))

data Table      = Table Annot [TableField]

data TableField = ExpField   Annot Exp Exp
                | NamedField Annot Selector Exp
                | Field      Annot Exp

data FunCall    = FunCall Annot PrefixExp (Maybe Selector) [Exp]

--------------------------------------------------------------------------------

class GetAnnot t where
  annot :: t -> Annot

instance GetAnnot Exp where
  annot e =
    case e of
      Nil a -> a
      Bool a _ -> a
      Number a _ _ -> a
      String a _ -> a
      Vararg a   -> a
      Binop a _ _ _ -> a
      Unop a _ _ -> a
      EFunDef x -> annot x
      PrefixExp x -> annot x
      TableConst x -> annot x

instance GetAnnot FunBody where
  annot (FunBody a _ _ _) = a

instance GetAnnot PrefixExp where
  annot e =
    case e of
      PEVar x -> annot x
      PEFunCall x -> annot x
      Paren x -> annot x

instance GetAnnot Table where
  annot (Table a _) = a

instance GetAnnot FunCall where
  annot (FunCall a _ _ _) = a

instance GetAnnot Var where
  annot e =
    case e of
      VarName x -> annot x
      Select a _ _ -> a
      SelectName a _ _ -> a

instance GetAnnot Name where
  annot = Lua.ann . nameOrig


--------------------------------------------------------------------------------







type Label    = Int

data CFG = CFG
  { cfgBlocks :: Map Label BasicBlock
  , cfgEntry  :: Label
  }

data BasicBlock = BasicBlock
  { bbStmts   :: Vector Stat
  , bbExit    :: EndStat
  }

data Stat =
    Assign          Annot [Var]   [Exp]
  | LocalAssign     Annot [Name]  (Maybe [Exp])
  | FunAssign       Annot FunName FunBody
  | LocalFunAssign  Annot Name    FunBody
  | FunCallStat FunCall
  | AssertIsNumber Exp
  | AssertIsBool Exp
  | AssumeIsNumber Name
  | AssumeNotNil Name

data EndStat =
    Return [Exp]
  | Goto [Label]
  | ForIn [Name] [Exp] Label Label

data FunBody = FunBody Annot [Name] (Maybe Annot) CFG

--------------------------------------------------------------------------------

newtype M a = M { unM :: StateT RW Lift a }


data RW = RW
  {
  }

instance Functor M where
  fmap = liftM
instance Applicative M where
  pure  = derive_pure (Iso M unM)
  (<*>) = ap
instance Monad M where
  (>>=) = derive_bind (Iso M unM)


emit :: Stat -> M ()
emit = undefined

endCurrentBlock :: EndStat -> M ()
endCurrentBlock = undefined

setCurrentBlock :: Label -> M ()
setCurrentBlock = undefined

newBlock :: M Label
newBlock = undefined

assicateLabel :: Name -> Label -> M ()
assicateLabel = undefined

lookupLabel :: Name -> M Label
lookupLabel = undefined

pushScope :: M ()
pushScope = undefined

popScope :: M ()
popScope = undefined

cvtNameUse :: Lua.Name Annot -> M Name
cvtNameUse = undefined

cvtNameNew :: Lua.Name Annot -> M Name
cvtNameNew = undefined



--------------------------------------------------------------------------------

class CvtStat t where
  cvtStat :: t Annot -> M ()

class CvtExpr t s | t -> s, s -> t where
  cvtExpr :: t Annot -> M s

instance CvtStat Lua.Stat where
  cvtStat stmt =
    case stmt of

      Lua.Assign a xs es ->
        do es' <- mapM cvtExpr es
           xs' <- mapM cvtExpr xs
           emit (Assign a xs' es')

      Lua.FunCall _ f ->
        do f' <- cvtExpr f
           emit (FunCallStat f')

      Lua.Label _ l ->
        do l' <- cvtNameNew l
           x <- newBlock
           assicateLabel l' x
           endCurrentBlock (Goto [x])
           setCurrentBlock x

      Lua.Goto _ l ->
        do l' <- cvtNameUse l
           x <- lookupLabel l'
           endCurrentBlock (Goto [x])

      Lua.Do _ b ->
        do pushScope
           cvtStat b
           popScope

      Lua.While _ e b ->
        do body <- newBlock
           next <- newBlock
           e'   <- cvtExpr e
           emit (AssertIsBool e')
           endCurrentBlock (Goto [body,next])
           setCurrentBlock body
           pushScope
           cvtStat b
           popScope
           setCurrentBlock next

      Lua.Repeat _ b e ->
        do body <- newBlock
           next <- newBlock
           endCurrentBlock (Goto [body])
           setCurrentBlock body
           pushScope
           cvtStat b
           e' <- cvtExpr e
           emit (AssertIsBool e')
           popScope
           endCurrentBlock (Goto [next,body])
           setCurrentBlock next

      Lua.ForRange a x e1 e2 mbE3 b ->
        do emit =<< (AssertIsNumber <$> cvtExpr e1)
           emit =<< (AssertIsNumber <$> cvtExpr e2)
           case mbE3 of
             Nothing -> return ()
             Just e3 -> emit =<< (AssertIsNumber <$> cvtExpr e3)

           body <- newBlock
           next <- newBlock
           endCurrentBlock (Goto [body,next])

           setCurrentBlock body
           pushScope
           x' <- cvtNameNew x
           emit (AssumeIsNumber x')
           cvtStat b
           popScope
           endCurrentBlock (Goto [body,next])

           setCurrentBlock next


      Lua.ForIn a xs es b ->
        do es'  <- mapM cvtExpr es
           body <- newBlock
           next <- newBlock

           pushScope
           xs' <- mapM cvtNameNew xs
           endCurrentBlock (ForIn xs' es' body next)

           setCurrentBlock body
           emit (AssumeNotNil (head xs'))
           cvtStat b
           popScope
           endCurrentBlock (Goto [ body, next ])

      -- Lua.FunAssign a x b
      -- Lua.LocaFunAssign

      Lua.LocalAssign a xs mb ->
        do e'  <- traverse (traverse cvtExpr) mb
           xs' <- mapM cvtNameNew xs
           emit (LocalAssign a xs' e')

      Lua.EmptyStat _ -> return ()


instance CvtStat Lua.Block where
  cvtStat (Lua.Block _ xs mbRet) =
    do mapM_ cvtStat xs
       case mbRet of
         Nothing -> return ()
         Just es -> do es' <- mapM cvtExpr es
                       endCurrentBlock (Return es')

instance CvtExpr Lua.Exp Exp where
 cvtExpr expr =
   case expr of
     Lua.Nil a             -> return (Nil a)
     Lua.Bool a b          -> return (Bool a b)
     Lua.Number a t n      -> return (Number a t n)
     Lua.String a t        -> return (String a t)
     Lua.Vararg a          -> return (Vararg a)
     -- Lua.EFunDef a d       -> error "XXX"
     Lua.PrefixExp _ e     -> PrefixExp <$> cvtExpr e
     Lua.TableConst _ t    -> TableConst <$> cvtExpr t
     Lua.Binop a op e1 e2  -> do e1' <- cvtExpr e1
                                 e2' <- cvtExpr e2
                                 return (Binop a op e1' e2')
     Lua.Unop a op e       -> do e' <- cvtExpr e
                                 return (Unop a op e')


instance CvtExpr Lua.FunCall FunCall where
  cvtExpr (Lua.NormalFunCall a p arg) =
    do p'   <- cvtExpr p
       arg' <- cvtExpr arg
       return (FunCall a p' Nothing arg')

  cvtExpr (Lua.MethodCall a p x arg) =
    do p'   <- cvtExpr p
       arg' <- cvtExpr arg
       return (FunCall a p' (Just x) arg')

instance CvtExpr Lua.FunArg [Exp] where
  cvtExpr arg =
    case arg of
      Lua.Args _ es     -> mapM cvtExpr es
      Lua.TableArg _ t  -> do t' <- cvtExpr t
                              return [ TableConst t' ]
      Lua.StringArg a t -> return [ String a t ]

instance CvtExpr Lua.Var Var where
  cvtExpr v =
    case v of
      Lua.VarName _ x       -> VarName  <$> cvtNameUse x
      Lua.Select a p e      -> Select a <$> cvtExpr p <*> cvtExpr e
      Lua.SelectName a p x  -> do p' <- cvtExpr p
                                  return (SelectName a p' x)

instance CvtExpr Lua.PrefixExp PrefixExp where
  cvtExpr pe =
    case pe of
      Lua.PEVar _ v     -> PEVar     <$> cvtExpr v
      Lua.PEFunCall _ f -> PEFunCall <$> cvtExpr f
      Lua.Paren _ e     -> Paren     <$> cvtExpr e

instance CvtExpr Lua.Table Table where
  cvtExpr (Lua.Table a fs) = Table a <$> mapM cvtExpr fs


instance CvtExpr Lua.TableField TableField where
  cvtExpr f =
    case f of
      Lua.ExpField a e1 e2 -> do e1' <- cvtExpr e1
                                 e2' <- cvtExpr e2
                                 return (ExpField a e1' e2')
      Lua.NamedField a x e  -> do e' <- cvtExpr e
                                  return (NamedField a x e')
      Lua.Field a e         -> do e' <- cvtExpr e
                                  return (Field a e')



