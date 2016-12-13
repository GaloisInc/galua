module Galua.Arguments where

import           Control.Exception
import           Data.IORef
import           Data.Foldable (for_)

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

assign :: VM -> Int -> Value -> SV.SizedVector (IORef Value) -> IO ()
assign vm i x stack
  | i == registryIndex = throwIO (LuaX "assign: Can not replace registry")

  | i <  registryIndex =
      do let v = execUpvals (vmCurExecEnv vm)
             i' = registryIndex - i - 1
         mb <- IOVector.readMaybe v i'
         for_ mb $ \ref -> writeIORef ref x

  | otherwise =
      case compare i 0 of
        EQ -> return () -- XXX: what should we do here?
        LT -> do n  <- SV.size stack
                 mb <- SV.getMaybe stack (n+i)
                 for_ mb $ \ref -> writeIORef ref x
        GT -> do mb <- SV.getMaybe stack (i-1)
                 for_ mb $ \ref -> writeIORef ref x


select :: VM -> Int -> SV.SizedVector (IORef Value) -> IO (Maybe Value)
select vm i stack
  | i == registryIndex = do let r = machRegistry (vmMachineEnv vm)
                            return $! Just $! Table r

  | i <  registryIndex = do let v  = execUpvals (vmCurExecEnv vm)
                                i' = registryIndex - i - 1
                            mbRef <- IOVector.readMaybe v i'
                            traverse readIORef mbRef

  | otherwise =
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


valueArgument :: VM -> Int -> SV.SizedVector (IORef Value) -> IO Value
valueArgument vm i args =
  do mb <- select vm i args
     case mb of
       Just x -> return x
       Nothing -> return Nil

valueArgumentOpt :: VM -> Int -> SV.SizedVector (IORef Value) -> IO (Maybe Value)
valueArgumentOpt = select


stringArgument :: VM -> Int -> SV.SizedVector (IORef Value) -> IO LuaString
stringArgument vm i args =
  do mb <- select vm i args
     case mb of
       Just x -> checkStringArgument i x
       Nothing -> badArgument i "string expected, got no value"

stringArgumentOpt :: VM -> Int -> SV.SizedVector (IORef Value) -> IO (Maybe LuaString)
stringArgumentOpt vm i args =
  do mb <- select vm i args
     case mb of
       Just Nil -> return Nothing
       Nothing -> return Nothing
       Just x -> Just <$> checkStringArgument i x


checkStringArgument :: Int -> Value -> IO LuaString
checkStringArgument i v =
  case v of
    Number x -> fromByteString (packUtf8 (numberToString x))
    String x -> return x
    x        -> typeError i "string" x




intArgument :: VM -> Int -> SV.SizedVector (IORef Value) -> IO Int
intArgument vm i args =
  do mb <- select vm i args
     case mb of
       Just x -> checkIntArgument i x
       Nothing -> badArgument i "string expected, got no value"

doubleArgument :: VM -> Int -> SV.SizedVector (IORef Value) -> IO Double
doubleArgument vm i args =
  do mb <- select vm i args
     case mb of
       Just x -> checkDoubleArgument i x
       Nothing -> badArgument i "number expected, got no value"

checkDoubleArgument :: Int -> Value -> IO Double
checkDoubleArgument i v =
  case fmap numberToDouble (valueNumber v) of
    Nothing -> typeError i "number" v
    Just x  -> return x

checkIntArgument :: Int -> Value -> IO Int
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

intArgumentOpt :: VM -> Int -> SV.SizedVector (IORef Value) -> IO (Maybe Int)
intArgumentOpt vm i args =
  do mb <- select vm i args
     case mb of
       Just Nil -> return Nothing
       Just x -> Just <$> checkIntArgument i x
       Nothing -> return Nothing




boolArgument :: VM -> Int -> SV.SizedVector (IORef Value) -> IO Bool
boolArgument vm i args =
  do mb <- select vm i args
     case mb of
       Just x -> return (valueBool x)
       Nothing -> return False






tableArgument :: VM -> Int -> SV.SizedVector (IORef Value) -> IO (Reference Table)
tableArgument vm i args =
  do mb <- select vm i args
     case mb of
       Just x -> checkTableArgument i x
       Nothing -> badArgument i "table expected, got no value"

tableArgumentOpt :: VM -> Int -> SV.SizedVector (IORef Value) -> IO (Maybe (Reference Table))
tableArgumentOpt vm i args =
  do mb <- select vm i args
     case mb of
       Nothing -> return Nothing
       Just Nil -> return Nothing
       Just x -> Just <$> checkTableArgument i x

checkTableArgument :: Int -> Value -> IO (Reference Table)
checkTableArgument i v =
  case v of
    Table t -> return t
    x       -> typeError i "table" x





functionArgument :: VM -> Int -> SV.SizedVector (IORef Value) -> IO (Reference Closure)
functionArgument vm i args =
  do mb <- select vm i args
     case mb of
       Just x -> checkFunctionArgument i x
       Nothing -> badArgument i "function expected, got no value"

checkFunctionArgument :: Int -> Value -> IO (Reference Closure)
checkFunctionArgument i v =
  case v of
    Closure c -> return c
    _ -> typeError i "function" v



threadArgument :: VM -> Int -> SV.SizedVector (IORef Value) -> IO (Reference Thread)
threadArgument vm i args =
  do mb <- select vm i args
     case mb of
       Just x  -> checkThreadArgument i x
       Nothing -> badArgument i "thread expected, got no value"

threadArgumentOpt :: VM -> Int -> SV.SizedVector (IORef Value) -> IO (Maybe (Reference Thread))
threadArgumentOpt vm i args =
  do mb <- select vm i args
     case mb of
       Nothing  -> return Nothing
       Just Nil -> return Nothing
       Just x   -> Just <$> checkThreadArgument i x

checkThreadArgument :: Int -> Value -> IO (Reference Thread)
checkThreadArgument i v =
  case v of
    Thread t -> return t
    x        -> typeError i "thread" x




--------------------------------------------------------------------------------
-- Errors

newtype LuaX = LuaX String
                    deriving Show

instance Exception LuaX

badArgument :: Int -> String -> IO a
badArgument n extra =
  throwIO (LuaX ("bad argument #" ++ show n ++ " (" ++ extra ++ ")"))

typeError :: Int -> String -> Value -> IO a
typeError n expect got =
  badArgument n (expect ++ " expected, got " ++ prettyValueType (valueType got))


