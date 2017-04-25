{-# LANGUAGE RecordWildCards, FlexibleInstances, MultiWayIf #-}
module Galua.Micro.Eval (runStmtAt,Next(..)) where

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
import           Control.Monad(zipWithM_,(<=<))

import qualified Language.Lua.Bytecode as OP
import           Language.Lua.Bytecode.FunId

import           Galua.Value
import           Galua.Number(Number(..),wordshiftL,wordshiftR,nummod,
                              numberPow,numberDiv)
import           Galua.Reference(AllocRef)
import           Galua.LuaString
                   (LuaString,toByteString,fromByteString,luaStringLen)
import           Galua.Micro.AST

data V = VRef {-# UNPACK #-} !(IORef Value)
       | VVal Value


type MetaTables = IORef (Map ValueType (Reference Table))

data Frame = Frame { regs         :: !(IOVector V)
                   , regsTMP      :: !(IORef (Map (Int,Int) Value))
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
          | Continue
          | MakeCall (Reference Closure)
          | MakeTailCall (Reference Closure) [Value]
          | RaiseError Value
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

-- setListReg :: MonadIO m => Frame -> [Value] -> m ()
-- setListReg Frame{..} = writeIORef listRegRef

getListReg :: Frame -> IO [Value]
getListReg Frame{..} = readIORef listRegRef

setReg :: IsValue v => Frame -> Reg -> v -> IO ()
setReg Frame { .. } reg v0 =
  case reg of
    Reg (OP.Reg r) -> IOVector.write regs r val
    TMP a b        -> case toValue v0 of
                        VVal v -> modifyIORef' regsTMP (Map.insert (a,b) v)
                        VRef _ -> error "[bug] reference in a TMP register"

  where
  val = toValue v0


getReg :: Frame -> Reg -> IO V
getReg Frame { .. } reg =
  case reg of
    Reg (OP.Reg r) -> IOVector.read regs r
    TMP a b -> do m <- readIORef regsTMP
                  case Map.lookup (a,b) m of
                    Just v  -> return (VVal v)
                    Nothing -> crash ("Read from bad reg: " ++ show (pp' reg))


getExpr :: Frame -> Expr -> IO V
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
        KString bs     -> (VVal . String) <$> fromByteString bs
        KLongString bs -> (VVal . String) <$> fromByteString bs

getProp :: Frame -> Prop -> IO Bool
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



typeError :: String -> IO a
typeError what = crash ("Type error: expected a " ++ what ++ ".")

crash :: String -> IO a
crash msg = fail msg


isTable :: V -> IO (Reference Table)
isTable val =
  case val of
    VVal (Table t) -> return t
    _              -> typeError "table"

isFunction :: V -> IO (Reference Closure)
isFunction val =
  case val of
    VVal (Closure c) -> return c
    _                -> typeError "function"



isValue :: V -> IO Value
isValue val =
  case val of
    VVal v -> return v
    _      -> typeError "Lua value"

isReference :: V -> IO (IORef Value)
isReference val =
    case val of
      VRef r -> return r
      _      -> typeError "reference"


isNumber :: V -> IO Number
isNumber val =
  case val of
    VVal (Number n) -> return n
    _ -> typeError "number"

isInt :: V -> IO Int
isInt val =
  case val of
    VVal (Number (Int n)) -> return n
    _ -> typeError "integer"

isDouble :: V -> IO Double
isDouble val =
  case val of
    VVal (Number (Double d)) -> return d
    _ -> typeError "double"

isString :: V -> IO LuaString
isString val =
  case val of
    VVal (String s) -> return s
    _ -> typeError "string"

isBool :: V -> IO Bool
isBool val =
  case val of
    VVal (Bool b) -> return b
    _ -> typeError "boolean"



runStmt :: AllocRef -> Frame -> Int -> BlockStmt -> IO Next
runStmt aref f@Frame { .. } pc stmt =
  case stmtCode stmt of
    Assign r e    -> do setReg f r =<< getExpr f e
                        return Continue

    SetUpVal _ _  -> crash "SetUpVal in phase 2"


    -- Tables
    NewTable r  ->
      do let refLoc = RefLoc { refLocSite   = InLua ourFID pc
                             , refLocCaller = ourCaller
                             }
         setReg f r =<< newTable aref refLoc 0 0
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
         tabs <- readIORef metaTables
         let tyMeta t = setMb (Map.lookup t tabs)

         case v of

           Table tr         -> setMb =<< getTableMeta tr
           UserData ur      -> setMb =<< readIORef (userDataMeta (referenceVal ur))
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
         vs  <- getListReg f
         return (MakeTailCall fun vs)

    Return ->
      do vs <- getListReg f
         return (ReturnWith vs)

    CloseStack _ -> crash "CloseStack in phase 2"


{-
    NewClosure res ix us ->
      do rs <- mapM (isReference <=< getExpr f) us
         closureUpvals <- Vector.thaw vs
         let fid = ourFID
             f   = luaFunction (subFun fid ix) cloFunc

         fun <- case subFuns Vector.!? proto of
                  Just fn ->
                        return (luaFunction (subFun ourFID proto) fn)
                  Nothing -> crash ("Missing function: " ++ show proto)
         undefined
         let refLoc = RefLoc { refLocSite   = InLua ourFID pc
                             , refLocCaller = ourCaller
                             }
         clo <- newClosure aref refLoc fun =<< Vector.thaw (Vector.fromList rs)
         setReg f res clo
         return Continue
-}

    Drop ArgReg n ->
      do modifyIORef' argRegRef (drop n)
         return Continue

    Drop ListReg n ->
      do modifyIORef' listRegRef (drop n)
         return Continue

    SetList res es ->
      do vs <- mapM (isValue <=< getExpr f) es
         let ref = case res of
                     ArgReg -> argRegRef
                     ListReg -> listRegRef
         writeIORef ref vs
         return Continue

    Append res es ->
      do vs <- mapM (isValue <=< getExpr f) es
         let ref = case res of
                     ArgReg -> argRegRef
                     ListReg -> listRegRef
         modifyIORef ref (vs ++)
         return Continue

    IndexList res lst ix ->
      do vs <- readIORef $ case lst of
                             ArgReg  -> argRegRef
                             ListReg -> listRegRef
         case splitAt ix vs of
           (_,b:_) -> setReg f res b
           _       -> setReg f res ()
         return Continue

    NewRef res val ->
      do setReg f res =<< newIORef =<< isValue =<< getExpr f val
         return Continue

    ReadRef res ref ->
      do r <- isReference =<< getExpr f ref
         setReg f res =<< readIORef r
         return Continue

    WriteRef ref val ->
      do r <- isReference =<< getExpr f ref
         v <- isValue =<< getExpr f val
         writeIORef r v
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
                  Just s  -> setReg f res =<< fromByteString s
                  Nothing -> setReg f res ()

           ToBoolean ->
             do n <- isValue vv
                case valueString n of
                  Just s  -> setReg f res =<< fromByteString s
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
                        | otherwise -> fromByteString (BS.append x y)
                setReg f res (String z)

         return Continue


    Comment _    -> return Continue


runStmtAt :: AllocRef -> Frame -> Vector BlockStmt -> Int -> IO Next
runStmtAt aref f curBlock pc =
  case curBlock Vector.!? pc of
    Just stmt -> runStmt aref f pc stmt
    Nothing   -> crash ("Invalid PC: " ++ show pc)




