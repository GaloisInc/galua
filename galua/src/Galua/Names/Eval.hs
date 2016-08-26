{-# Language OverloadedStrings #-}
module Galua.Names.Eval
  ( exprToValue, NameResolveEnv(..)
  , NotFound(..), nameResolveException
  , Flavor(..)
  , nameInScope
  ) where

import           Language.Lua.Syntax(Unop(..))
import           Language.Lua.Bytecode.Pretty(PP,pp,blankPPInfo)
import           Language.Lua.Bytecode
                    (UpIx(..),Reg(..),DebugInfo(..),VarInfo(..),Function(..))
import           Data.Vector(Vector)
import qualified Data.Vector as Vector
import           Data.Maybe(fromMaybe)
import           Data.ByteString(ByteString)
import           Data.IORef(IORef,readIORef)
import           Data.Text.Encoding(decodeUtf8)
import qualified Data.Text as Text
import           Data.Bits(complement)
import           Control.Exception(Exception,throwIO)

import Galua.Value(Value(..),valueBool)
import Galua.Overloading(valueMetamethod,withMetatables)
import Galua.Reference(Reference,readRef)
import Galua.Util.Table(Table,getTableRaw,tableLen)
import Galua.Util.SizedVector(SizedVector,getMaybe)
import Galua.LuaString(unsafeFromByteString,fromByteString,luaStringLen)
import Galua.Number(Number(..),numberToInt)
import Galua.Names.Find
import Galua.Mach (TypeMetatables)

data NameResolveEnv = NameResolveEnv
  { nrFunction :: Function
  , nrStack    :: SizedVector (IORef Value)
  , nrUpvals   :: Vector (IORef Value)
  , nrMetas    :: TypeMetatables
  }


data NotFound = NotFound String
                deriving Show

instance Exception NotFound

nameResolveException :: String -> IO a
nameResolveException = bad

bad :: String -> IO a
bad msg = throwIO (NotFound msg)


data Flavor = Local | UpValue | Global


-- | Throws 'NotFound'
exprToValue :: NameResolveEnv {-^ execution frame -}  ->
               Maybe PC       {-^ cur pc -}           ->
               ExprName                               ->
               IO Value
exprToValue eenv pc expr =
  case expr of
    ELocal x -> localVar eenv pc x
    ENonLocal x ->
      case getUpValue (nrFunction eenv) x of
        Just u -> readUpValue eenv u
        Nothing -> readGlobal eenv x
    ESelectFrom x y ->
      do v <- exprToValue eenv pc x
         case v of
           Table tr ->
             do i <- exprToValue eenv pc y
                lookupInTable eenv i tr

           _ -> do i <- exprToValue eenv pc y
                   indexFromMetaTable eenv True v i

    EVarArg   -> bad "XXX: lookup of var args is not yet implemented."

    EString x -> String <$> fromByteString x
    EBool x   -> return (Bool x)
    ENumber n -> return (Number n)
    EUni op e ->
      do v <- exprToValue eenv pc e
         case op of
           Neg        ->
             case v of
               Number n -> return $ Number $ negate n
               _        -> bad "Negation of a non-number."
           Complement ->
             case v of
               Number n
                 | Just i <- numberToInt n ->
                    return $ Number $ Int $ complement i
               _ -> bad "Complement of a non-integer."
           Len  ->
             case v of
               String s -> return $ Number $ Int $ luaStringLen s
               Table r  -> do t <- readRef r
                              (Number . Int) <$> tableLen t
                  -- XXX: Check that the `length` is not overwritten!
               _ -> bad "Length of a value which is not a string or a table."
           Not  -> return $ Bool $ not $ valueBool v


localNameInScope :: Function -> Maybe PC -> LocalName -> Bool
localNameInScope func mbPc name =
  fromMaybe False $
  do pc <- mbPc
     vi <-  dbg Vector.!? localNumber name
     return (varInfoStart vi <= pc && pc < varInfoEnd vi)
  where
  dbg = debugInfoVars $ funcDebug func

nameInScope :: Function -> Maybe PC -> ExprName -> Bool
nameInScope func mbPc name =
  case name of
    ELocal x        -> localNameInScope func mbPc x
    ENonLocal _     -> True
    ESelectFrom x y -> nameInScope func mbPc x && nameInScope func mbPc y
    EVarArg         -> funcIsVararg func

    EString _       -> True
    ENumber _       -> True
    EBool _         -> True
    EUni _ e        -> nameInScope func mbPc e




localVar :: NameResolveEnv {-^ execution frame -}      ->
            Maybe PC       {-^ cur pc -}               ->
            LocalName      {-^ info about the local -} ->
            IO Value
localVar eenv pc name
  | localNameInScope (nrFunction eenv) pc name = readReg eenv (localReg name)
  | otherwise = bad ("Variable " ++ Text.unpack (decodeUtf8 (localName name))
                                                        ++ " is not in scope.")


type PC   = Int



-- | If we know that the name is not local, then is it an up-value?
getUpValue :: Function -> ByteString -> Maybe UpIx
getUpValue fun nm = fmap UpIx
                  $ Vector.findIndex (nm ==)
                  $ debugInfoUpvalues
                  $ funcDebug fun

-- We could check that its name is _ENV
getGlobalEnv :: NameResolveEnv -> IO (Reference (Table Value))
getGlobalEnv eenv =
  case Vector.elemIndex
         "_ENV"
         (debugInfoUpvalues (funcDebug (nrFunction eenv))) of
    Just i ->
      case nrUpvals eenv Vector.!? i of
        Just ref ->
          do val <- readIORef ref
             case val of
               Table t -> return t
               _       -> bad "Global environment is not a table."
        Nothing -> bad "getGlobalEnv: Panic, upvalues don't match debug info."
    Nothing -> bad "Missing global environment."

lookupInTable :: NameResolveEnv -> Value -> Reference (Table Value) -> IO Value
lookupInTable eenv key ref =
  do tab <- readRef ref
     res <- getTableRaw tab key
     case res of
       Nil -> indexFromMetaTable eenv False (Table ref) key
       v   -> return v

indexFromMetaTable :: NameResolveEnv -> Bool -> Value -> Value -> IO Value
indexFromMetaTable eenv failOnNil v key =
  do method <- withMetatables tabs (valueMetamethod v "__index")
     case method of
       Nil | failOnNil -> bad "Missing field."
           | otherwise -> return Nil
       Table r   -> lookupInTable eenv key r
       Closure _ ->
         bad "Accessing the value requires executing the meta-method."
       _ -> indexFromMetaTable eenv True method key

  where
  tabs = nrMetas eenv




readGlobal :: NameResolveEnv -> ByteString -> IO Value
readGlobal eenv nm = lookupInTable eenv key =<< getGlobalEnv eenv
  where key = String (unsafeFromByteString nm)

readUpValue :: NameResolveEnv -> UpIx -> IO Value
readUpValue eenv (UpIx n) =
  case nrUpvals eenv Vector.!? n of
    Just ref -> readIORef ref
    Nothing  -> bad $ "Invalid upvalue: " ++ sh (UpIx n)

readReg ::  NameResolveEnv -> Reg -> IO Value
readReg eenv (Reg n) =
  do mb <- getMaybe (nrStack eenv) n
     case mb of
       Just ref -> readIORef ref
       Nothing  -> bad $ "Invalid register: " ++ sh (Reg n)

sh :: PP a => a -> String
sh = show . pp blankPPInfo


