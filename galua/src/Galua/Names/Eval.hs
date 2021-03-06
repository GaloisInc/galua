{-# Language OverloadedStrings #-}
module Galua.Names.Eval
  ( exprToValue, NameResolveEnv(..)
  , NotFound(..), nameResolveException
  , Flavor(..)
  , nameInScope
  , globalInfo
  ) where

import           Language.Lua.Syntax(Unop(..))
import           Data.Vector(Vector)
import qualified Data.Vector as Vector
import           Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as IOVector
import           Data.Maybe(fromMaybe)
import           Data.ByteString(ByteString)
import           Data.IORef(IORef,readIORef)
import           Data.Text.Encoding(decodeUtf8)
import qualified Data.Text as Text
import           Data.Bits(complement)
import           Control.Exception(Exception,throwIO)


import Galua.Code
import Galua.Value(Reference, referenceVal, Value(..),valueBool)
import Galua.Overloading(get_m__index)
import Galua.Util.Table(Table,getTableRaw,tableLen)
import Galua.LuaString(toByteString,
                        unsafeFromByteString,fromByteString,luaStringLen)
import Galua.Number(Number(..),numberToInt)
import Galua.Names.Find
import Galua.Mach (TypeMetatables)
import Galua.Pretty(Pretty(pp))

data NameResolveEnv = NameResolveEnv
  { nrFunction :: Function
  , nrStack    :: IOVector (IORef Value)
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

globalInfo ::
  NameResolveEnv {-^ execution frame -} ->
  Maybe PC       {-^ cur pc          -} ->
  ExprName                              ->
  IO (Maybe (ByteString,[ByteString]))
globalInfo eenv pc expr =

  case expr of

    ENonLocal x ->
      case getUpValue (nrFunction eenv) x of
        Just _  -> return Nothing
        Nothing -> return (Just (x,[]))

    ESelectFrom x y ->
      do mb <- globalInfo eenv pc x
         case mb of
           Just (g,sels) ->
             do i <- exprToValue eenv pc y
                case i of
                  String s -> return (Just (g, toByteString s : sels))
                  _        -> return Nothing
           Nothing -> return Nothing

    _ -> return Nothing





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
               Table r  -> Number . Int <$> tableLen (referenceVal r)
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
  do res <- getTableRaw (referenceVal ref) key
     case res of
       Nil -> indexFromMetaTable eenv False (Table ref) key
       v   -> return v

indexFromMetaTable :: NameResolveEnv -> Bool -> Value -> Value -> IO Value
indexFromMetaTable eenv failOnNil v key =
  do method <- get_m__index (nrMetas eenv) v
     case method of
       Nil | failOnNil -> bad "Missing field."
           | otherwise -> return Nil
       Table r   -> lookupInTable eenv key r
       Closure _ ->
         bad "Accessing the value requires executing the meta-method."
       _ -> indexFromMetaTable eenv True method key



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
  do let sz = IOVector.length (nrStack eenv)
     if n >= sz
        then bad $ "Invalid register: " ++ sh (Reg n)
        else readIORef =<< IOVector.unsafeRead (nrStack eenv) n

sh :: Pretty a => a -> String
sh = show . pp
