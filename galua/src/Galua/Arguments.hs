module Galua.Arguments where

import           Control.Monad.IO.Class
import           Control.Monad (when)
import           Data.IORef
import           Data.Foldable (for_)
import qualified Data.Vector.Mutable as IOVector

import           Galua.Mach
import           Galua.Number
import           Galua.Value
import           Galua.LuaString
import qualified Galua.Util.SizedVector as SV
import qualified Galua.Util.IOVector as IOVector

maxstack :: Int
maxstack = 1000000

registryIndex :: Int
registryIndex = -maxstack - 1000

isPseudo :: Int -> Bool
isPseudo x = x <= registryIndex

assign :: Int -> Value -> SV.SizedVector (IORef Value) -> Mach ()
assign i x stack
  | i == registryIndex = luaError "assign: Can not replace registry"

  | i <  registryIndex = do v <- getsExecEnv execUpvals
                            let i' = registryIndex - i - 1
                            liftIO $
                              do mb <- IOVector.readMaybe v i'
                                 for_ mb $ \ref ->
                                   writeIORef ref x

  | otherwise = liftIO $
      case compare i 0 of
        EQ -> return () -- XXX: what should we do here?
        LT -> do n  <- SV.size stack
                 mb <- SV.getMaybe stack (n+i)
                 for_ mb $ \ref -> writeIORef ref x
        GT -> do mb <- SV.getMaybe stack (i-1)
                 for_ mb $ \ref -> writeIORef ref x

select :: Int -> SV.SizedVector (IORef Value) -> Mach (Maybe Value)
select i stack
  | i == registryIndex = do r <- getsMachEnv machRegistry
                            return (Just (Table r))

  | i <  registryIndex = do v <- getsExecEnv execUpvals
                            let i' = registryIndex - i - 1
                            liftIO $ do mbRef <- IOVector.readMaybe v i'
                                        traverse readIORef mbRef

  | otherwise = liftIO $
      case compare i 0 of
        EQ -> return Nothing
        LT -> do n  <- SV.size stack
                 mb <- SV.getMaybe stack (n+i)
                 traverse readIORef mb
        GT -> do mb <- SV.getMaybe stack (i-1)
                 traverse readIORef mb

-- | Upvalues are 1 based. This computes an upvalue index, to an index
-- suitable for passing to select, where very negative numbers are upvalues.
upvalue :: Int -> Int
upvalue x = registryIndex - x

valueArgument :: Int -> SV.SizedVector (IORef Value) -> Mach Value
valueArgument i args =
  do mb <- select i args
     case mb of
       Just x -> return x
       Nothing -> return Nil

valueArgumentOpt :: Int -> SV.SizedVector (IORef Value) -> Mach (Maybe Value)
valueArgumentOpt = select


stringArgument :: Int -> SV.SizedVector (IORef Value) -> Mach LuaString
stringArgument i args =
  do mb <- select i args
     case mb of
       Just x -> checkStringArgument i x
       Nothing -> badArgument i "string expected, got no value"

stringArgumentOpt :: Int -> SV.SizedVector (IORef Value) -> Mach (Maybe LuaString)
stringArgumentOpt i args =
  do mb <- select i args
     case mb of
       Just Nil -> return Nothing
       Nothing -> return Nothing
       Just x -> Just <$> checkStringArgument i x


checkStringArgument :: Int -> Value -> Mach LuaString
checkStringArgument i v =
  case v of
    Number x -> liftIO (fromByteString (packUtf8 (numberToString x)))
    String x -> return x
    x        -> typeError i "string" x




intArgument :: Int -> SV.SizedVector (IORef Value) -> Mach Int
intArgument i args =
  do mb <- select i args
     case mb of
       Just x -> checkIntArgument i x
       Nothing -> badArgument i "string expected, got no value"

doubleArgument :: Int -> SV.SizedVector (IORef Value) -> Mach Double
doubleArgument i args =
  do mb <- select i args
     case mb of
       Just x -> checkDoubleArgument i x
       Nothing -> badArgument i "number expected, got no value"

checkDoubleArgument :: Int -> Value -> Mach Double
checkDoubleArgument i v =
  case fmap numberToDouble (valueNumber v) of
    Nothing -> typeError i "number" v
    Just x  -> return x

checkIntArgument :: Int -> Value -> Mach Int
checkIntArgument i v =
  case v of
    Number n ->
      case numberToInt n of
        Nothing -> badArgument i "number has no integer representation"
        Just x -> return x
    String s
      | Just n <- parseNumber (unpackUtf8 (toByteString s)) ->
          case numberToInt n of
            Nothing -> badArgument i "number has no integer representation"
            Just x -> return x
    _ -> typeError i "number" v

intArgumentOpt :: Int -> SV.SizedVector (IORef Value) -> Mach (Maybe Int)
intArgumentOpt i args =
  do mb <- select i args
     case mb of
       Just Nil -> return Nothing
       Just x -> Just <$> checkIntArgument i x
       Nothing -> return Nothing




boolArgument :: Int -> SV.SizedVector (IORef Value) -> Mach Bool
boolArgument i args =
  do mb <- select i args
     case mb of
       Just x -> return (valueBool x)
       Nothing -> return False






tableArgument :: Int -> SV.SizedVector (IORef Value) -> Mach (Reference Table)
tableArgument i args =
  do mb <- select i args
     case mb of
       Just x -> checkTableArgument i x
       Nothing -> badArgument i "table expected, got no value"

tableArgumentOpt :: Int -> SV.SizedVector (IORef Value) -> Mach (Maybe (Reference Table))
tableArgumentOpt i args =
  do mb <- select i args
     case mb of
       Nothing -> return Nothing
       Just Nil -> return Nothing
       Just x -> Just <$> checkTableArgument i x

checkTableArgument :: Int -> Value -> Mach (Reference Table)
checkTableArgument i v =
  case v of
    Table t -> return t
    x       -> typeError i "table" x





functionArgument :: Int -> SV.SizedVector (IORef Value) -> Mach (Reference Closure)
functionArgument i args =
  do mb <- select i args
     case mb of
       Just x -> checkFunctionArgument i x
       Nothing -> badArgument i "function expected, got no value"

checkFunctionArgument :: Int -> Value -> Mach (Reference Closure)
checkFunctionArgument i v =
  case v of
    Closure c -> return c
    _ -> typeError i "function" v



threadArgument :: Int -> SV.SizedVector (IORef Value) -> Mach (Reference Thread)
threadArgument i args =
  do mb <- select i args
     case mb of
       Just x  -> checkThreadArgument i x
       Nothing -> badArgument i "thread expected, got no value"

threadArgumentOpt :: Int -> SV.SizedVector (IORef Value) -> Mach (Maybe (Reference Thread))
threadArgumentOpt i args =
  do mb <- select i args
     case mb of
       Nothing  -> return Nothing
       Just Nil -> return Nothing
       Just x   -> Just <$> checkThreadArgument i x

checkThreadArgument :: Int -> Value -> Mach (Reference Thread)
checkThreadArgument i v =
  case v of
    Thread t -> return t
    x        -> typeError i "thread" x




--------------------------------------------------------------------------------
-- Errors

badArgument :: Int -> String -> Mach a
badArgument n extra = luaError ("bad argument #" ++ show n ++
                                                          " (" ++ extra ++ ")")

typeError :: Int -> String -> Value -> Mach a
typeError n expect got =
  badArgument n (expect ++ " expected, got " ++ prettyValueType (valueType got))
