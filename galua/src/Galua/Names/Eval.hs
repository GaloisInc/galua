module Galua.Names.Eval
  ( exprToValue, NameResolveEnv(..)
  , NotFound(..), nameResolveException
  ) where

import           Language.Lua.Syntax(Unop(..))
import           Language.Lua.Bytecode.Pretty(PP,pp,blankPPInfo)
import           Language.Lua.Bytecode
                    (UpIx(..),Reg(..),DebugInfo(..),VarInfo(..),Function(..))
import           Data.Vector(Vector)
import qualified Data.Vector as Vector
import           Data.ByteString(ByteString)
import           Data.IORef(IORef,readIORef)
import           Data.Text.Encoding(decodeUtf8)
import qualified Data.Text as Text
import           Data.Bits(complement)
import           Control.Exception(Exception,throwIO)

import Galua.Value(Value(..),valueBool)
import Galua.Reference(Reference,readRef)
import Galua.Util.Table(Table,getTableMeta,getTableRaw,tableLen)
import Galua.Util.SizedVector(SizedVector,getMaybe)
import Galua.LuaString(unsafeFromByteString,fromByteString,luaStringLen)
import Galua.Number(Number(..),numberToInt)
import Galua.Names.Find

data NameResolveEnv = NameResolveEnv
  { nrFunction :: Function
  , nrStack    :: SizedVector (IORef Value)
  , nrUpvals   :: Vector (IORef Value)
  }


data NotFound = NotFound String
                deriving Show

instance Exception NotFound

nameResolveException :: String -> IO a
nameResolveException = bad

bad :: String -> IO a
bad msg = throwIO (NotFound msg)


-- | Throws 'NotFound'
exprToValue :: NameResolveEnv {-^ execution frame -}  ->
               PC             {-^ cur pc -}           ->
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
                lookupInTable i tr
           _ -> bad "Selection from a non-table."


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




localVar :: NameResolveEnv {-^ execution frame -}      ->
            PC             {-^ cur pc -}               ->
            LocalName      {-^ info about the local -} ->
            IO Value
localVar eenv pc name =
  case dbg Vector.!? localNumber name of
    Nothing -> bad "Invalid local name."
    Just vi
      | varInfoStart vi <= pc && pc < varInfoEnd vi ->
        readReg eenv (localReg name)
      | otherwise ->
        bad ("Variable " ++ Text.unpack (decodeUtf8 (localName name))
                                                      ++ " is not in scope.")
  where
  dbg = debugInfoVars $ funcDebug $ nrFunction eenv


type PC   = Int



-- | If we know that the name is not local, then is it an up-value?
getUpValue :: Function -> ByteString -> Maybe UpIx
getUpValue fun nm = fmap UpIx
                  $ Vector.findIndex (nm ==)
                  $ debugInfoUpvalues
                  $ funcDebug fun

-- XXX: For now we assume that the global environment is the first up value.
-- We could check that its name is _ENV
getGlobalEnv :: NameResolveEnv -> IO (Reference (Table Value))
getGlobalEnv eenv =
  case nrUpvals eenv Vector.!? 0 of
    Just ref ->
      do val <- readIORef ref
         case val of
           Table t -> return t
           _       -> bad "Global environment is not a table."
    Nothing -> bad "Missing global environment."

lookupInTable :: Value -> Reference (Table Value) -> IO Value
lookupInTable key ref =
  do tab <- readRef ref
     res <- getTableRaw tab key
     case res of
       Nil -> do m <- getTableMeta tab
                 case m of
                   Table newRef -> lookupInTable key newRef
                   _ -> bad "Encountered a meta-table, which is not a table."
       v   -> return v

readGlobal :: NameResolveEnv -> ByteString -> IO Value
readGlobal eenv nm = lookupInTable key =<< getGlobalEnv eenv
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


