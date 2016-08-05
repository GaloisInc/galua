{-# LANGUAGE RecordWildCards, FlexibleInstances, MultiWayIf #-}
module Galua.Micro.Eval where

import           Data.IORef(IORef,newIORef,modifyIORef',modifyIORef,readIORef,
                            writeIORef)
import           Data.Vector(Vector)
import qualified Data.Vector as Vector
import           Data.Vector.Mutable(IOVector)
import qualified Data.Vector.Mutable as IOVector
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Bits(complement,(.&.),(.|.),xor)
import qualified Data.ByteString as BS
import           Control.Monad.IO.Class(MonadIO,liftIO)
import           Control.Monad(zipWithM_,(<=<))

import qualified Language.Lua.Bytecode as OP
import           Language.Lua.Bytecode.FunId

import           Galua.Reference(Reference,NameM,readRef,RefLoc(..),CodeLoc(..))
import           Galua.Value
import           Galua.Number(Number(..),wordshiftL,wordshiftR,nummod,
                              numberPow,numberDiv)
import           Galua.LuaString
                   (LuaString,toByteString,fromByteString,luaStringLen)
import           Galua.Micro.AST
import           Galua.Micro.Translate(translate)

data V = VRef {-# UNPACK #-} !(IORef Value)
       | VVal Value


type MetaTables = IORef (Map ValueType (Reference Table))

data Frame = Frame { regs         :: !(IOVector V)
                   , regsTMP      :: !(IORef (Map (Int,Int) V))
                   , argRegRef    :: !(IORef [Value])
                   , listRegRef   :: !(IORef [Value])
                   , upvals       :: !(Vector (IORef Value))
                   , metaTables   :: !MetaTables
                   , ourFID       :: !FunId
                   , subFuns      :: !(Vector OP.Function)
                   , ourCode      :: !(Map BlockName (Vector BlockStmt))
                   , ourCaller    :: !CodeLoc
                   }

data Next = EnterBlock BlockName
          | RaiseError Value
          | Continue
          | MakeCall (Reference Closure)
          | MakeTailCall (Reference Closure)
          | ReturnWith [Value]

class IsValue t where
  toValue :: t -> V

instance IsValue V where
  toValue = id

instance IsValue (IORef Value) where
  toValue = VRef

instance IsValue Value where
  toValue = VVal

instance IsValue Bool where
  toValue = toValue . Bool

instance IsValue Int where
  toValue = toValue . Int

instance IsValue Double where
  toValue = toValue . Double

instance IsValue Number where
  toValue = toValue . Number

instance IsValue LuaString where
  toValue = toValue . String

instance IsValue () where
  toValue _ = toValue Nil

instance IsValue (Reference Table) where
  toValue = toValue . Table

instance IsValue (Reference Closure) where
  toValue = toValue . Closure

instance IsValue (Reference UserData) where
  toValue = toValue . UserData

setListReg :: MonadIO m => Frame -> [Value] -> m ()
setListReg Frame{..} = liftIO . writeIORef listRegRef

getListReg :: MonadIO m => Frame -> m [Value]
getListReg Frame{..} = liftIO (readIORef listRegRef)

setReg :: (IsValue v, MonadIO m) => Frame -> Reg -> v -> m ()
setReg Frame { .. } reg v0 =
  liftIO $
  case reg of
    Reg (OP.Reg r) -> IOVector.write regs r val
    TMP a b        -> modifyIORef' regsTMP (Map.insert (a,b) val)

  where
  val = toValue v0


getReg :: MonadIO m => Frame -> Reg -> m V
getReg Frame { .. } reg =
  liftIO $
  case reg of
    Reg (OP.Reg r) -> IOVector.read regs r
    TMP a b -> do m <- readIORef regsTMP
                  case Map.lookup (a,b) m of
                    Just v  -> return v
                    Nothing -> crash ("Read from bad reg: " ++ show (pp' reg))


getExpr :: MonadIO m => Frame -> Expr -> m V
getExpr f expr =
  case expr of
    EReg r -> getReg f r
    EUp (OP.UpIx x)  ->
      case upvals f Vector.!? x of
        Just r  -> return (VRef r)
        Nothing -> crash ("Read from bad upval: " ++ show (pp' expr))
    ELit l ->
      case l of
        KNil           -> return (VVal Nil)
        KBool b        -> return (VVal (Bool b))
        KNum d         -> return (VVal (Number (Double d)))
        KInt n         -> return (VVal (Number (Int n)))
        KString bs     -> liftIO $ (VVal . String) <$> fromByteString bs
        KLongString bs -> liftIO $ (VVal . String) <$> fromByteString bs

getProp :: MonadIO m => Frame -> Prop -> m Bool
getProp f (Prop pre es) =
  do vs <- mapM (getExpr f) es
     let unary cvt f' = case vs of
                          [v] -> f' <$> cvt v
                          _   -> typeError "exactly 1 parameter"
         binary cvt f' = case vs of
                           [v1,v2] -> do x <- cvt v1
                                         y <- cvt v2
                                         return (f' x y)
                           _ -> typeError "exactly 2 parameters"

     case pre of
       IsNone -> case vs of
                   [VVal (Bool b)] -> return b
                   [VVal Nil]    -> return False
                   [_]      -> return True
                   _        -> typeError "exactly 1 parameter"
       IsInteger -> unary isNumber $ \n -> case n of
                                             Int _ -> True
                                             _     -> False

       IsNaN -> unary isNumber $ \n -> case n of
                                         Int _    -> False
                                         Double d -> isNaN d

       Equals     -> binary isValue  (==)
       NumberLT   -> binary isNumber (<)
       NumberLEQ  -> binary isNumber (<=)
       StringLT   -> binary isString (<)
       StringLEQ  -> binary isString (<=)



typeError :: MonadIO m => String -> m a
typeError what = crash ("Type error: expected a " ++ what ++ ".")

crash :: MonadIO m => String -> m a
crash msg = liftIO (fail msg)


isTable :: MonadIO m => V -> m (Reference Table)
isTable val =
  case val of
    VVal (Table t) -> return t
    _              -> typeError "table"

isFunction :: MonadIO m => V -> m (Reference Closure)
isFunction val =
  case val of
    VVal (Closure c) -> return c
    _                -> typeError "function"



isValue :: MonadIO m => V -> m Value
isValue val =
  case val of
    VVal v -> return v
    _      -> typeError "Lua value"

isReference :: MonadIO m => V -> m (IORef Value)
isReference val =
    case val of
      VRef r -> return r
      _      -> typeError "reference"


isNumber :: MonadIO m => V -> m Number
isNumber val =
  case val of
    VVal (Number n) -> return n
    _ -> typeError "number"

isInt :: MonadIO m => V -> m Int
isInt val =
  case val of
    VVal (Number (Int n)) -> return n
    _ -> typeError "integer"

isDouble :: MonadIO m => V -> m Double
isDouble val =
  case val of
    VVal (Number (Double d)) -> return d
    _ -> typeError "double"

isString :: MonadIO m => V -> m LuaString
isString val =
  case val of
    VVal (String s) -> return s
    _ -> typeError "string"

isBool :: MonadIO m => V -> m Bool
isBool val =
  case val of
    VVal (Bool b) -> return b
    _ -> typeError "boolean"



runStmt :: NameM m => Frame -> Int -> BlockStmt -> m Next
runStmt f@Frame { .. } pc stmt =
  case stmtCode stmt of
    Assign r e    -> do liftIO (setReg f r =<< getExpr f e)
                        return Continue

    SetUpVal _ _  -> crash "SetUpVal in phase 2"


    -- Tables
    NewTable r  ->
      do let refLoc = RefLoc { refLocSite   = InLua ourFID pc
                             , refLocCaller = ourCaller
                             }
         (liftIO . setReg f r) =<< newTable refLoc 0 0
         return Continue

    LookupTable res tab ix ->
      do t <- isTable =<< getReg f tab
         v <- isValue =<< getExpr f ix
         setReg f res =<< getTableRaw t v
         return Continue

    SetTable tab ix val ->
      do t <- isTable =<< getReg f tab
         i <- isValue =<< getExpr f ix
         v <- isValue =<< getExpr f val
         setTableRaw t i v
         return Continue

    SetTableList tab ix ->
      do t  <- isTable =<< getReg f tab
         xs <- getListReg f
         zipWithM_ (setTableRaw t) (map (Number . Int) [ ix .. ]) xs
         return Continue

    GetMeta r e ->
      do v <- isValue =<< getExpr f e
         let setMb mb = do case mb of
                             Nothing -> setReg f r ()
                             Just t  -> setReg f r t
                           return Continue
         tabs <- liftIO (readIORef metaTables)
         let tyMeta t = setMb (Map.lookup t tabs)

         case v of

           Table tr         -> setMb =<< getTableMeta tr
           UserData ur      -> setMb . userDataMeta =<< readRef ur
           _                -> tyMeta (valueType v)

    Raise e ->
      do v <- isValue =<< getExpr f e
         return (RaiseError v)

    Case e as d ->
      do v <- isValue =<< getExpr f e
         case lookup (valueType v) as of
           Just b  -> return (EnterBlock b)
           Nothing ->
             case d of
               Nothing -> crash "Stuck case."
               Just b  -> return (EnterBlock b)

    If p tr fa ->
      do b <- getProp f p
         return (EnterBlock (if b then tr else fa))

    Goto b -> return (EnterBlock b)

    Call clo ->
      do fun <- isFunction =<< getReg f clo
         return (MakeCall fun)

    TailCall clo ->
      do fun <- isFunction =<< getReg f clo
         return (MakeTailCall fun)

    Return ->
      do vs <- getListReg f
         return (ReturnWith vs)

    CloseStack _ -> crash "CloseStack in phase 2"

    NewClosure res proto us ->
      do rs <- mapM (isReference <=< getExpr f) us
         fun <- case subFuns Vector.!? proto of
                  Just fn -> return (LuaFunction (subFun ourFID proto) fn)
                  Nothing -> crash ("Missing function: " ++ show proto)
         let refLoc = RefLoc { refLocSite   = InLua ourFID pc
                             , refLocCaller = ourCaller
                             }
         clo <- newClosure refLoc fun (Vector.fromList rs)
         setReg f res clo
         return Continue


    Drop ArgReg n ->
      do liftIO (modifyIORef' argRegRef (drop n))
         return Continue

    Drop ListReg n ->
      do liftIO (modifyIORef' listRegRef (drop n))
         return Continue

    SetList res es ->
      do vs <- mapM (isValue <=< getExpr f) es
         let ref = case res of
                     ArgReg -> argRegRef
                     ListReg -> listRegRef
         liftIO (writeIORef ref vs)
         return Continue

    Append res es ->
      do vs <- mapM (isValue <=< getExpr f) es
         let ref = case res of
                     ArgReg -> argRegRef
                     ListReg -> listRegRef
         liftIO (modifyIORef ref (vs ++))
         return Continue

    IndexList res lst ix ->
      do vs <- liftIO $ readIORef $ case lst of
                 ArgReg  -> argRegRef
                 ListReg -> listRegRef
         case splitAt ix vs of
           (_,b:_) -> setReg f res b
           _       -> setReg f res ()
         return Continue

    NewRef res val ->
      do setReg f res =<< liftIO . newIORef =<< isValue =<< getExpr f val
         return Continue

    ReadRef res ref ->
      do r <- isReference =<< getExpr f ref
         setReg f res =<< liftIO (readIORef r)
         return Continue

    WriteRef ref val ->
      do r <- isReference =<< getExpr f ref
         v <- isValue =<< getExpr f val
         liftIO (writeIORef r v)
         return Continue

    Arith1 res op e ->
      do vv <- getExpr f e
         case op of

           ToNumber ->
             do v <- isValue vv
                case valueNumber v of
                  Just n  -> setReg f res n
                  Nothing -> setReg f res ()

           ToInt ->
             do v <- isValue vv
                case valueInt v of
                 Just n  -> setReg f res n
                 Nothing -> setReg f res ()

           IntToDouble ->
             do n <- isInt vv
                setReg f res (fromIntegral n :: Double)

           ToString ->
             do n <- isValue vv
                case valueString n of
                  Just s  -> setReg f res =<< liftIO (fromByteString s)
                  Nothing -> setReg f res ()

           ToBoolean ->
             do n <- isValue vv
                case valueString n of
                  Just s  -> setReg f res =<< liftIO (fromByteString s)
                  Nothing -> setReg f res ()

           StringLen   -> setReg f res . luaStringLen =<< isString vv

           TableLen    -> setReg f res =<< tableLen =<< isTable vv

           NumberUnaryMinus ->
             do d <- isNumber vv
                setReg f res (negate d)

           Complement  ->
             do i <- isInt vv
                setReg f res (complement i)

           BoolNot ->
             do b <- isBool vv
                setReg f res (not b)


         return Continue


    Arith2 res op e1 e2 ->
      do vv1 <- getExpr f e1
         vv2 <- getExpr f e2
         let liftBin cvt sem =
               do x1 <- cvt vv1
                  x2 <- cvt vv2
                  setReg f res (sem x1 x2)

         case op of
           NumberAdd -> liftBin isNumber (+)
           NumberSub -> liftBin isNumber (-)
           NumberMul -> liftBin isNumber (*)

           NumberPow -> liftBin isNumber numberPow

           IMod      -> liftBin isInt    mod
           FMod      -> liftBin isDouble nummod

           IDiv      -> liftBin isInt    div
           NumberDiv -> liftBin isNumber numberDiv

           And       -> liftBin isInt (.&.)
           Or        -> liftBin isInt (.|.)
           Xor       -> liftBin isInt xor
           Shl       -> liftBin isInt wordshiftL
           Shr       -> liftBin isInt wordshiftR

           Concat ->
             do s1 <- isString vv1
                s2 <- isString vv2
                let x = toByteString s1
                    y = toByteString s2
                z <- if | BS.null x -> return s1
                        | BS.null y -> return s2
                        | otherwise -> liftIO (fromByteString (BS.append x y))
                setReg f res (String z)

         return Continue


    Comment _    -> return Continue


--------------------------------------------------------------------------------


runStmtAt :: NameM m => Frame -> Vector BlockStmt -> Int -> m Next
runStmtAt f curBlock pc =
  case curBlock Vector.!? pc of
    Just stmt -> runStmt f pc stmt
    Nothing   -> crash ("Invalid PC: " ++ show pc)


run :: NameM m => Frame -> Vector BlockStmt -> Int -> m (Either Value [Value])
run f curBlock pc =
  do next <- runStmtAt f curBlock pc
     case next of
       Continue -> run f curBlock (pc + 1)
       EnterBlock l ->
         case Map.lookup l (ourCode f) of
           Just b  -> run f b 0
           Nothing -> crash ("Missing block: " ++ show l)
       RaiseError e -> return (Left e)
       ReturnWith xs -> return (Right xs)
       MakeTailCall fun ->
         do vs <- getListReg f
            let loc = InLua (ourFID f) pc
            runFunction loc (metaTables f) fun vs
       MakeCall fun ->
         do vs <- getListReg f
            let loc = InLua (ourFID f) pc
            ans <- runFunction loc (metaTables f) fun vs
            case ans of
              Left err -> return (Left err)
              Right as ->
                do setListReg f as
                   run f curBlock (pc + 1)

runFunction ::
  NameM m => CodeLoc -> MetaTables -> Reference Closure -> [Value] ->
                                        m (Either Value [Value])
runFunction ourCaller metaTables r vs =
  do clo <- readRef r
     case cloFun clo of
       LuaFunction ourFID fun ->
         do let stackSize = OP.funcMaxStackSize fun
            regs <- liftIO (IOVector.replicate stackSize (VVal Nil))
            regsTMP <- liftIO (newIORef Map.empty)
            argRegRef <- liftIO (newIORef vs)
            listRegRef <- liftIO (newIORef [])
            let upvals  = cloUpvalues clo
                subFuns = OP.funcProtos fun
                ourCode = functionCode (translate fun)
                f = Frame { .. }
            case Map.lookup EntryBlock ourCode of
              Just b  -> run f b 0
              Nothing -> crash "Function entry point is missing."

       CFunction {} -> crash "We don't know how to call C functions (yet?)."



