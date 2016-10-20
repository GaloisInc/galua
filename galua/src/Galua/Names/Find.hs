{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
-- | Identify expressions that refer to interesting names of things.
module Galua.Names.Find
  ( chunkLocations
  , LocatedExprName(..)
  , ExprName(..)
  , LocalName(..)
  , ppExprName
  , ppLocatedExprName
  ) where

import           Control.Applicative
import           Control.Monad(when)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Foldable
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Encoding(encodeUtf8,decodeUtf8)
import           Data.Char(isAlphaNum,isAlpha,isAscii)
import           Language.Lua.Annotated.Lexer (SourceRange(..),showRange)
import           Language.Lua.Annotated.Simplify
import           Language.Lua.Annotated.Syntax
import           Language.Lua.StringLiteral(interpretStringLiteral)
import qualified Language.Lua.Syntax as Lua
import           Language.Lua.Bytecode(Reg(..),plusReg)
import           Language.Lua.Bytecode.FunId(rootFun, subFun, FunId(..))

import Galua.Number


chunkLocations ::
  Int {- ^ chunk ID -} ->
  Block SourceRange ->
  ([LocatedExprName], [FunId])
chunkLocations i b =
  (computedNames rw, reverse (computedFunctions rw))
  where
    M m    = resolve b
    (_,rw) = m (rootFun i) emptyRW



data LocalName = LocalName
  { localName   :: !ByteString  -- ^ Our name (mostly for debugging)
  , localReg    :: !Reg         -- ^ What register is this var in
  , localNumber :: !Int         -- ^ What local number are we?
  } deriving (Eq,Show)



data ExprName = ELocal !LocalName
              | ENonLocal !ByteString
              | ESelectFrom ExprName ExprIx -- value, index
              | EVarArg

              -- These appear only in the indexes of a selection
              | EString   ByteString
              | ENumber   Number
              | EBool     Bool
              | EUni Lua.Unop ExprName
                deriving (Show,Eq)

type ExprIx = ExprName

ppExprName :: ExprName -> String
ppExprName y =
  case y of
    ELocal x            -> Text.unpack (decodeUtf8 (localName x))
    ENonLocal x         -> Text.unpack (decodeUtf8 x)
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
    ESelectFrom a (EString k)
      | isValid bs -> ppExprName a ++ "." ++ bs
      where
      bs = Text.unpack $ decodeUtf8 k

      isValid (x : xs) = validFirst x && all validRest xs
      isValid [] = False

      validFirst x = isAscii x && (x == '_' || isAlpha x)
      validRest  x = isAscii x && (x == '_' || isAlphaNum x)

    ESelectFrom a b     -> ppExprName a ++ "[" ++ ppExprName b ++ "]"


data LocatedExprName = LocatedExprName
  { exprPos  :: SourceRange
  , exprName :: ExprName
  } deriving (Show,Eq)

ppLocatedExprName :: LocatedExprName -> String
ppLocatedExprName x =
  ppExprName (exprName x) ++ " " ++ showRange (exprPos x)


newtype M a = M (FunId -> RW -> (Maybe a, RW))

data RW = RW
  { computedNames :: [LocatedExprName]
  , computedFunctions :: [FunId]
  , nextLocal     :: !Int                       -- ^ Local counter
  , nextFunction  :: !Int                       -- ^ Local counter
  , nextReg       :: !Reg
  , nameMap       :: Map ByteString LocalName
  }

emptyRW :: RW
emptyRW = RW
  { computedNames = []
  , computedFunctions = []
  , nextLocal     = 0
  , nextFunction  = 0
  , nextReg       = Reg 0
  , nameMap       = Map.empty
  }

instance Functor M where
  fmap = liftA

instance Applicative M where
  pure a        = M (\_ rw -> (Just a, rw))
  M mf <*> M mx = M (\funId rw ->
                      let (f,rw1) = mf funId rw
                          (x,rw2) = mx funId rw1
                      in (f <*> x, rw2))


emit :: SourceRange -> M ExprName -> M ExprName
emit src (M f) = M $ \funId rw ->
  let (res,rw1) = f funId rw
  in case res of
       Nothing -> (Nothing, rw1)
       Just e  ->
         let le = LocatedExprName { exprPos = src, exprName = e }
         in (Just e, rw1 { computedNames = le : computedNames rw1 })

ignore :: M a
ignore = M (\_ rw -> (Nothing,rw))


declareLocal :: Text -> M ()
declareLocal x = M (\_ rw ->
  ( Just ()
  , let l   = nextLocal rw
        nm  = encodeUtf8 x
        r   = nextReg rw
        lnm = LocalName { localName   = nm
                        , localNumber = l
                        , localReg    = r
                        }
    in rw { nextLocal = l + 1
          , nextReg   = plusReg r 1
          , nameMap   = Map.insert nm lnm (nameMap rw)
          }
  ))

declareInvisible :: Int -> M ()
declareInvisible n = M $ \_ rw ->
  (Just (), rw { nextLocal = n + nextLocal rw
               , nextReg = plusReg (nextReg rw) n
               })

newScope :: M a -> M a
newScope (M f) = M $ \funId rw ->
  let curMap = nameMap rw
      r      = nextReg rw
  in case f funId rw of
       (mb,rw1) -> (mb, rw1 { nameMap = curMap, nextReg = r })

newFun :: M a -> M a
newFun (M f) = M $ \funId rw ->
  let newFunId = subFun funId (nextFunction rw) in
  case f newFunId
         emptyRW { computedNames = computedNames rw
                 , nextFunction  = 0
                 , computedFunctions = newFunId : computedFunctions rw
                 } of
    (a,rw1) -> (a, rw { computedNames     = computedNames rw1
                      , computedFunctions = computedFunctions rw1
                      , nextFunction      = nextFunction rw + 1
                      })

useName :: Name SourceRange -> M ExprName
useName (Name r x) = emit r $ M $ \_ rw ->
  ( Just $ case Map.lookup nm (nameMap rw) of
             Just vi -> ELocal vi
             Nothing -> ENonLocal nm
  , rw
  )
  where
  nm = encodeUtf8 x


declareAndUse :: Name SourceRange -> M ExprName
declareAndUse x@(Name _ t) = declareLocal t *> useName x




class Resolve a where
  resolve :: a -> M ExprName

instance Resolve a => Resolve [a] where
  resolve xs = ignore <* traverse_ resolve xs

instance Resolve a => Resolve (Maybe a) where
  resolve xs = ignore <* traverse_ resolve xs

instance (Resolve a, Resolve b) => Resolve (a,b) where
  resolve (x,y) = ignore <* resolve x <* resolve y


instance Resolve (Block SourceRange) where
  resolve (Block _ stats mbReturn) = newScope $ resolve (stats, mbReturn)

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
      ForRange _ i x y z b  -> resolve (x,(y,z)) *>
                               newScope (declareInvisible 3 *>
                                         declareAndUse i    *> resolve b)
      ForIn _ is xs b       -> resolve xs *>
                               newScope (declareInvisible 3 *>
                                         traverse_ declareAndUse is *>
                                         resolve b)
      FunAssign _ n x       -> resolve n *>
                               declareFun (isMethod n) x
      LocalFunAssign _ n b  -> declareAndUse n *>
                               declareFun False b
      LocalAssign _ xs b    -> resolve b <*   -- Note the order here!
                               traverse_ declareAndUse xs
      EmptyStat{}           -> ignore

isMethod :: FunName a -> Bool
isMethod (FunName _ _ _ (Just _)) = True
isMethod _ = False

declareFun :: Bool -> FunBody SourceRange -> M ExprName
declareFun meth (FunBody _ xs _ block) =
  newFun $
    when meth (declareLocal "self") *>
    traverse_ declareAndUse xs *>
    resolve block



instance Resolve (FunName SourceRange) where
  --name.field.field.field:method
  resolve (FunName _ name@(Name nameA _) fields method) =
    case method of
      Nothing -> declareParams fields
      Just m  -> declareParams (fields ++ [m])

    where
    step thing (Name a x) = emit (mkRange a)
                            $ (\e -> ESelectFrom e (label x)) <$> thing

    label x = EString (encodeUtf8 x)

    declareParams ps = foldl step (emit nameA (useName name)) ps


    mkRange s =
      SourceRange
        { sourceFrom = sourceFrom nameA
        , sourceTo   = sourceTo s
        }

instance Resolve (Exp SourceRange) where
  resolve expr =
    case expr of
      Nil{}          -> ignore
      Bool _ b       -> pure (EBool b)
      Number a _ num -> emit a
                      $ case parseNumber (Text.unpack num) of
                          Just n -> pure (ENumber n)
                          Nothing -> ignore
      String a txt   -> emit a
                      $ case interpretStringLiteral (Text.unpack txt) of
                          Just ok -> pure (EString (LBS.toStrict ok))
                          Nothing -> ignore
      Vararg a       -> ignore <* emit a (pure EVarArg)
      EFunDef _ (FunDef _ b) -> declareFun False b
      PrefixExp _ p  -> resolve p
      TableConst _ t -> resolve t
      Binop _ _ l r  -> resolve (l,r)
      Unop _ i e     -> EUni (sUnop i) <$> resolve e

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
      MethodCall a obj (Name b meth) arg ->
              emit r (sel <$> resolve obj) <* resolve arg
        where sel o = ESelectFrom o (EString (encodeUtf8 meth))
              r = SourceRange { sourceFrom = sourceFrom a
                              , sourceTo   = sourceTo b }

instance Resolve (FunArg SourceRange) where
  resolve fa =
    case fa of
      Args _ xs     -> resolve xs
      TableArg _ t  -> resolve t
      StringArg _ _ -> ignore

instance Resolve (Var SourceRange) where
  resolve v =
    case v of
      VarName a x      -> emit a (useName x)
      Select a l r     -> emit a (ESelectFrom <$> resolve l <*> resolve r)
      SelectName a l (Name _ x) -> emit a (sel <$> resolve l)
        where sel o = ESelectFrom o (EString (encodeUtf8 x))


