module Galua.Names.Eval
  ( exprToValue, NameResolveEnv(..)
  , NotFound(..), nameResolveException
  ) where

import           Language.Lua.Syntax(Unop(..))
import           Language.Lua.Bytecode.Pretty(PP,pp,blankPPInfo)
import           Language.Lua.Bytecode
                    (UpIx(..),Reg(..),DebugInfo(..),VarInfo(..),Function(..))
import           Language.Lua.Bytecode.Debug
import           Data.Vector(Vector)
import qualified Data.Vector as Vector
import           Data.ByteString(ByteString)
import           Data.IORef(IORef,readIORef)
import           Data.Text.Encoding(decodeUtf8)
import qualified Data.Text as Text
import           Data.Bits(complement)
import           Control.Exception(Exception,throwIO)

import Galua.Value(Value(..),FunctionValue(..),valueBool)
import Galua.Reference(Reference,readRef)
import Galua.Util.Table(Table,getTableMeta,getTableRaw,tableLen)
import Galua.Util.SizedVector(SizedVector,getMaybe)
import Galua.Mach
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
               PC             {-^ cur pc -}            ->
               PC             {-^ op-code for name -} ->
               ExprName       {-^ expression to evaluate -} ->
               IO Value
exprToValue eenv pc name_pc expr =
  case expr of
    EIdent x  -> nameToValue eenv pc name_pc x
    ESelectFrom x y ->
      do v <- exprToValue eenv pc name_pc x
         case v of
           Table tr ->
             do i <- exprToValue eenv pc name_pc y
                lookupInTable i tr
           _ -> bad "Selection from a non-table."


    EVarArg   -> bad "XXX: lookup of var args is not yet implemented."

    EString x -> String <$> fromByteString x
    EBool x   -> return (Bool x)
    ENumber n -> return (Number n)
    EUni op e ->
      do v <- exprToValue eenv pc name_pc e
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





nameToValue :: NameResolveEnv {-^ execution frame -}  ->
               PC             {-^ cur pc -}            ->
               PC             {-^ op-code for name -} ->
               ByteString     {-^ an identifier -} ->
               IO Value
nameToValue eenv pc name_pc name =
  case getRegister regs pc name of
    LocalNotInScope -> bad ("Variable " ++ Text.unpack (decodeUtf8 name)
                                    ++ " is not in scope.")
    LocalInReg r -> readReg eenv r
    NotLocal ->
      case getUpValue func name of
        Just ix -> readUpValue eenv ix
        Nothing -> readGlobal eenv name
  where
  func = nrFunction eenv
  regs = Vector.reverse (getRegistersAt func name_pc)



-- reversed
type Regs = Vector VarInfo
type PC   = Int

data RegInfo = NotLocal | LocalInReg Reg | LocalNotInScope


-- | Is this a local variable, and if so is it ready for reading.
getRegister :: Regs         {-^ registers in scope where the name was -} ->
               PC           {-^ PC for current execution -}              ->
               ByteString   {-^ Name of interest -}                      ->
               RegInfo      {-^ Register where to lookup the value -}
getRegister regs pc nm =
  case Vector.findIndex ((nm ==) . varInfoName) regs of
    Nothing -> NotLocal
    Just ix ->
       let vi = regs Vector.! ix
       in if varInfoStart vi <= pc && pc < varInfoEnd vi
             then LocalInReg (Reg (Vector.length regs - ix - 1))
             else LocalNotInScope


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


