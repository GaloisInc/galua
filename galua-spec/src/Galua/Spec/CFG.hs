{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module Galua.Spec.CFG
  ( Label
  , CFG(..)
  , BasicBlock(..)
  , Stat(..)
  , EndStat(..)
  , FunBody(..)
  , Name(..)
  , NameT(..)
  , Selector
  , Var(..)
  , PrefixExp(..)
  , Exp(..)
  , Table(..)
  , TableField(..)
  , FunCall(..)
  , Lua.NumberType(..)
  , Lua.Unop(..)
  , Lua.Binop(..)

  , topLevel
  , Error(..)
  , GetAnnot(..)
  ) where

import           Language.Lua.Annotated.Parser(SourceRange(..))
import qualified Language.Lua.Annotated.Syntax as Lua
import qualified Language.Lua.PrettyPrinter as Lua
import qualified Language.Lua.Annotated.Simplify as Lua
import           Data.Map ( Map )
import qualified Data.Map as Map
import           Data.Vector ( Vector )
import qualified Data.Vector as Vector
import           MonadLib hiding (Label())
import           MonadLib.Derive
import           Data.Text ( Text )
import           Data.Function (on)
import           Text.Show.Pretty(PrettyVal(..))
import qualified Text.Show.Pretty as PP
import           GHC.Generics(Generic)

type Annot = SourceRange

type Label = Int

data CFG = CFG
  { cfgEntry     :: !Label
  , cfgBlocks    :: !(Vector BasicBlock)
  } deriving (Generic,Show)

data BasicBlock = BasicBlock
  { bbStmts   :: Vector Stat
  , bbExit    :: EndStat
  } deriving (Generic,Show)

data Stat =
    Assign          Annot [Var]   [Exp]
  | LocalAssign     Annot [Name]  (Maybe [Exp])
  | SetMethod Annot Name [Selector] Selector FunBody
  | FunCallStat FunCall
  | AssertIsNumber Exp
  | AssumeIsNumber Name
    deriving (Generic,Show)

data EndStat =
    Return [Exp]
  | Goto [Label]
  | If Exp Label Label
  | ForIn [Name] [Exp] Label Label
    deriving (Generic,Show)

data FunBody = FunBody Annot [Name] (Maybe Annot) CFG
    deriving (Generic,Show)



data Name       = Name { nameId   :: !Int
                       , nameType :: !NameT -- ^ What sort of name is this
                       , nameOrig :: !(Lua.Name Annot) -- ^ Original name
                       } deriving Show

data NameT      = LocalName
                | UpvalueName
                | GloabalName !Int
                  deriving (Show,Eq,Ord)

type Selector   = Lua.Name Annot

data Var        = VarName Name                        -- ^ variable
                | Select Annot PrefixExp Exp          -- ^ table[exp]
                | SelectName Annot PrefixExp Selector -- ^ table.variable
                  deriving (Generic,Show)

data PrefixExp  = PEVar Var
                | PEFunCall FunCall
                | Paren Exp
                  deriving (Generic,Show)

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
                  deriving (Generic,Show)

data Table      = Table Annot [TableField]
                  deriving (Generic,Show)

data TableField = ExpField   Annot Exp Exp
                | NamedField Annot Selector Exp
                | Field      Annot Exp
                  deriving (Generic,Show)

data FunCall    = FunCall Annot PrefixExp (Maybe Selector) [Exp]
                  deriving (Generic,Show)



--------------------------------------------------------------------------------
nameUID :: Name -> (Int,NameT)
nameUID Name { .. } = (nameId, nameType)

instance Eq Name where
  (==) = (==) `on` nameUID

instance Ord Name where
  compare = compare `on` nameUID




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








--------------------------------------------------------------------------------

newtype M a = M { unM :: ReaderT RO (StateT RW (ExceptionT Error Id)) a }

run :: Int -> M a -> Either Error a
run cid (M m) =
  case final m of
    Left err -> Left err
    Right (a,rw) ->
      case curBlock rw of
        Nothing -> Right a
        Just (l,_) -> panic ("run: Unterminated block " ++ show l)

  where
  final = runId . runExceptionT . runStateT rwBlank . runReaderT ro
  ro = RO { chunkID = cid }

topLevel :: Int -> Lua.Block Annot -> Either Error CFG
topLevel cid m = run cid (cvtBody m)

data Error = UndefinedLabel (Lua.Name Annot)
           | MultipleLabelDefinitionsFor (Lua.Name Annot)
           | BreakOutsideLooop Annot
             deriving Show

-- XXX: We use association lists because `Lua.Name` is not in `Ord`.
data Scope = Scope
  { scopeVars   :: [ (Lua.Name Annot, Name) ]
  , scopeLabels :: [ (Lua.Name Annot, Label) ]
  }

emptyScope :: Scope
emptyScope = Scope { scopeVars = [], scopeLabels = [] }


data RO = RO
  { chunkID :: !Int
  }

data RW = RW
  { prevScopes      :: ![Scope]
  , globals         :: ![(Lua.Name Annot, Name)]
  , upvalues        :: ![(Lua.Name Annot, Name)]
  , curScope        :: !Scope
  , curBlock        :: !(Maybe (Label,[Stat]))  -- ^ statements are reversed
  , finishedBlocks  :: !(Map Label BasicBlock)
  , nextBlockId     :: !Int
  , nextNameId      :: !Int
  , loopStack       :: ![Label]
  }

rwBlank :: RW
rwBlank = RW
  { prevScopes     = []
  , curScope       = emptyScope
  , upvalues       = []
  , globals        = []
  , curBlock       = Nothing
  , finishedBlocks = Map.empty
  , nextBlockId    = 0
  , nextNameId     = 0
  , loopStack      = []
  }

instance Functor M where
  fmap = liftM
instance Applicative M where
  pure  = derive_pure (Iso M unM)
  (<*>) = ap
instance Monad M where
  (>>=) = derive_bind (Iso M unM)


panic :: String -> a
panic x = error ("[bug] " ++ x)

reportError :: Error -> M a
reportError err = M $ raise err

subFun :: M a -> M a
subFun (M m) = M $
  do s <- get
     let ups = [ (x,n { nameType = UpvalueName })
                  | Scope { .. } <- curScope s : prevScopes s
                  , (x,n) <- scopeVars
               ]
     set rwBlank { globals = globals s, upvalues = ups }
     a <- m
     sets_ $ \s1 -> s { globals = globals s1 }
     return a


emit :: Stat -> M ()
emit s = M $ sets_ $ \RW { .. } ->
  case curBlock of
    Nothing     -> panic "emit: Nothing"
    Just (l,bs) -> RW { curBlock = Just (l, s : bs), .. }

hasCurrentBlock :: M Bool
hasCurrentBlock =
  do RW { .. } <- M get
     case curBlock of
       Nothing -> return False
       Just _  -> return True

getFinished :: M (Vector BasicBlock)
getFinished =
  do RW { .. } <- M get
     return (Vector.fromList (Map.elems finishedBlocks))

endCurrentBlock :: EndStat -> M ()
endCurrentBlock e = M $ sets_ $ \RW { .. } ->
  case curBlock of
    Nothing -> panic "endCurrentBlock: Nothing"
    Just (l,bs) ->
      let bb = BasicBlock
                { bbStmts = Vector.fromList (reverse bs)
                , bbExit  = e
                }
      in RW { curBlock       = Nothing
            , finishedBlocks = Map.insert l bb finishedBlocks
            , .. }

setCurrentBlock :: Label -> M ()
setCurrentBlock l = M $ sets_ $ \RW { .. } ->
  case curBlock of
    Just (l',_) -> panic ("setCurrentBlock: unterminated block " ++ show l')
    Nothing -> RW { curBlock = Just (l,[]), .. }


newBlock :: M Label
newBlock = M $ sets $ \RW { .. } ->
  (nextBlockId, RW { nextBlockId = 1 + nextBlockId, .. })

cvtLuaLabelNew :: Lua.Name Annot -> Label -> M ()
cvtLuaLabelNew l bl =
  do RW { .. } <- M get
     when (l `elem` map fst (scopeLabels curScope)) $
       reportError (MultipleLabelDefinitionsFor l)
     M $ set RW { curScope = curScope
                               { scopeLabels = (l,bl) : scopeLabels curScope }
                , .. }

cvtLuaLabelUse :: Lua.Name Annot -> M Label
cvtLuaLabelUse l =
  do RW { .. } <- M get
     let check Scope { .. } = lookup l scopeLabels
     case msum (map check (curScope : prevScopes)) of
       Nothing -> reportError (UndefinedLabel l)
       Just x  -> return x


pushScope :: M ()
pushScope = M $ sets_ $ \RW { .. } ->
                         RW { curScope = emptyScope
                            , prevScopes = curScope : prevScopes
                            , .. }

popScope :: M ()
popScope = M $ sets_ $ \RW { .. } ->
                        case prevScopes of
                          s : ss -> RW { curScope = s, prevScopes = ss, .. }
                          []     -> panic "popScope: []"

cvtNameNew :: Lua.Name Annot -> M Name
cvtNameNew l = M $ sets $ \RW { curScope = Scope { .. }, .. } ->
  let name = Name { nameId   = nextNameId
                  , nameType = LocalName
                  , nameOrig = l
                  }
  in ( name
     , RW { curScope   = Scope { scopeVars = (l,name) : scopeVars, .. }
          , nextNameId = 1 + nextNameId
          , ..
          }
     )

cvtNameUse :: Lua.Name Annot -> M Name
cvtNameUse l =
  do RW { .. } <- M get
     let check Scope { .. } = lookup l scopeVars
     case msum (map check (curScope : prevScopes) ++
                  [ lookup l upvalues, lookup l globals ]) of
       Nothing ->
         do RO { .. } <- M ask
            let n = Name { nameId   = nextNameId
                         , nameType = GloabalName chunkID
                         , nameOrig = l
                         }
            M $ set RW { globals = (l,n) : globals, .. }
            return n
       Just n -> return n

startLoop :: M ()
startLoop = M $ sets_ $ \RW { .. } ->
  case curBlock of
    Nothing    -> panic "startLoop: Nothing"
    Just (l,_) -> RW { loopStack = l : loopStack, .. }

endLoop :: M ()
endLoop = M $ sets_ $ \RW { .. } ->
  case loopStack of
    []     -> panic "endLoop: []"
    _ : ls -> RW { loopStack = ls, .. }


getLoop :: Annot -> M Label
getLoop a =
  do RW { .. } <- M get
     case loopStack of
       []    -> reportError (BreakOutsideLooop a)
       l : _ -> return l

--------------------------------------------------------------------------------

class CvtStat t where
  cvtStat :: t Annot -> M ()

class CvtExpr t s | t -> s, s -> t where
  cvtExpr :: t Annot -> M s

cvtBody :: (Lua.Block Annot) -> M CFG
cvtBody b =
  do l <- newBlock
     setCurrentBlock l
     cvtStat b
     needsEnd <- hasCurrentBlock
     when needsEnd (endCurrentBlock (Return []))
     bs <- getFinished
     return CFG { cfgEntry = l, cfgBlocks = bs }

cvtFunction :: Lua.FunBody Annot -> M FunBody
cvtFunction (Lua.FunBody a xs va b) = subFun $
  do xs' <- mapM cvtNameNew xs
     cfg <- cvtBody b
     return (FunBody a xs' va cfg)




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
        do x  <- newBlock
           cvtLuaLabelNew l x
           endCurrentBlock (Goto [x])
           setCurrentBlock x

      Lua.Goto _ l ->
        do x <- cvtLuaLabelUse l
           endCurrentBlock (Goto [x])


      Lua.Do _ b ->
        do pushScope
           cvtStat b
           popScope

      Lua.If _ bs d ->
        do after <- newBlock
           chain bs after
           setCurrentBlock after
        where
        block l b after = do setCurrentBlock l
                             pushScope
                             cvtStat b
                             popScope
                             y <- hasCurrentBlock
                             when y (endCurrentBlock (Goto [after]))

        chain [] _ = panic "Lua.If: []"

        chain [(e,b)] after =
          do e'   <- cvtExpr e
             l    <- newBlock
             (next,defB) <- case d of
                              Nothing -> return (after, return ())
                              Just db  ->
                                do next <- newBlock
                                   return (next, block next db after)
             endCurrentBlock (If e' l next)
             block l b after
             defB

        chain ((e,b) : more) after =
          do e'   <- cvtExpr e
             l    <- newBlock
             next <- newBlock
             endCurrentBlock (If e' l next)
             block l b after
             chain more after

      Lua.While _ e b ->
        do body <- newBlock
           next <- newBlock
           e'   <- cvtExpr e
           endCurrentBlock (If e' body next)
           setCurrentBlock body
           pushScope
           startLoop
           cvtStat b
           y <- hasCurrentBlock
           when y (endCurrentBlock (Goto [body]))
           endLoop
           popScope
           setCurrentBlock next

      Lua.Repeat _ b e ->
        do body <- newBlock
           next <- newBlock
           endCurrentBlock (Goto [body])
           setCurrentBlock body
           pushScope
           startLoop
           cvtStat b
           e' <- cvtExpr e
           endLoop
           popScope
           endCurrentBlock (If e' next body)
           setCurrentBlock next

      Lua.ForRange _ x e1 e2 mbE3 b ->
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
           startLoop
           x' <- cvtNameNew x
           emit (AssumeIsNumber x')
           cvtStat b
           endLoop
           popScope
           y <- hasCurrentBlock
           when y (endCurrentBlock (Goto [body,next]))

           setCurrentBlock next


      Lua.ForIn _ xs es b ->
        do es'  <- mapM cvtExpr es
           body <- newBlock
           next <- newBlock

           pushScope
           xs' <- mapM cvtNameNew xs
           endCurrentBlock (ForIn xs' es' body next)

           setCurrentBlock body
           startLoop
           cvtStat b
           endLoop
           popScope
           y <- hasCurrentBlock
           when y (endCurrentBlock (Goto [ body, next ]))


      Lua.Break a ->
        do l <- getLoop a
           endCurrentBlock (Goto [l])

      Lua.LocalFunAssign a x f ->
        do x' <- cvtNameNew x
           f' <- cvtFunction f
           emit (LocalAssign a [x'] (Just [EFunDef f']))

      Lua.FunAssign a (Lua.FunName _ x xs (Just m)) b ->
        do x' <- cvtNameUse x
           b' <- cvtFunction b
           emit (SetMethod a x' xs m b')

      Lua.FunAssign a (Lua.FunName _ x xs Nothing) b ->
        do x' <- cvtNameUse x
           b' <- cvtFunction b
           let var = foldl sel (VarName x') xs
               sel p s = let an = SourceRange
                                    { sourceFrom = sourceFrom (annot p)
                                    , sourceTo = sourceTo (Lua.ann s)
                                    }
                         in SelectName an (PEVar p) s
           emit (Assign a [var] [EFunDef b'])

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
     Lua.EFunDef _ (Lua.FunDef _ d) -> EFunDef <$> cvtFunction d
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



--------------------------------------------------------------------------------

instance PrettyVal Name where
  prettyVal Name { .. } = prettyVal nameOrig

instance PrettyVal (Lua.Name Annot) where
  prettyVal x = PP.String (show (Lua.pprint (Lua.sName x)))

instance PrettyVal Annot where
  prettyVal _ = PP.String ""

instance PrettyVal (Lua.Unop Annot) where
  prettyVal = PP.String . show . Lua.pprint . Lua.sUnop

instance PrettyVal (Lua.Binop Annot) where
  prettyVal = PP.String . show . Lua.pprint . Lua.sBinop

instance PrettyVal Stat
instance PrettyVal Lua.NumberType
instance PrettyVal Exp
instance PrettyVal PrefixExp
instance PrettyVal FunCall
instance PrettyVal Var
instance PrettyVal Table
instance PrettyVal TableField
instance PrettyVal CFG
instance PrettyVal EndStat
instance PrettyVal BasicBlock
instance PrettyVal FunBody
instance PrettyVal Text where
  prettyVal = prettyVal . show

instance PrettyVal a => PrettyVal (Vector a) where
  prettyVal = prettyVal . Vector.toList



