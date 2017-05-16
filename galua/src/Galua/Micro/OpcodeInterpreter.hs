{-# LANGUAGE RecordWildCards, FlexibleInstances, MultiWayIf #-}
module Galua.Micro.OpcodeInterpreter
  ( runStmtAt
  , Next(..)
  , crash
  , Frame(..)
  , setListReg
  ) where

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

import           Galua.Value
import           Galua.FunValue(luaFunction)
import           Galua.Number(Number(..),wordshiftL,wordshiftR,nummod,
                              numberPow,numberDiv)
import           Galua.Reference(AllocRef)
import           Galua.LuaString
                   (LuaString,toByteString,fromByteString,luaStringLen)
import           Galua.Micro.AST
import qualified Galua.Code as Code
import           Galua.Pretty(pp)
import           Galua.Mach(TypeMetatables)

data V = VRef {-# UNPACK #-} !(IORef Value)
       | VVal !Value


data Frame = Frame
  { regs         :: !(IOVector V)
    -- ^ Local values

  , regsTMP      :: !(IORef (Map (Int,Int) Value))
    -- ^ Additional--temporary--registers
    -- XXX: no need for a Map here, we can compile to a mutable vector
    -- just assign sequential numbers to TMP vars.

  , upvals       :: !(IOVector (IORef Value))
    -- ^ Upvalues available to this function

  , argRegRef    :: !(IORef [Value])
    -- ^ A special "list" register for storing function arguments

  , listRegRef   :: !(IORef [Value])
    -- ^ A special "list" register for storing results of functions

  , metaTables   :: !(IORef TypeMetatables)

  , ourCode      :: !(Map BlockName (Vector BlockStmt)) -- ^ Our CFG

  , ourFID       :: !Code.FunId                         -- ^ Our functoin ID
  , ourCaller    :: !CodeLoc                            -- ^ Who called us
  }

data Next = EnterBlock BlockName
          | Continue
          | MakeCall (Reference Closure) [Value]
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

setListReg :: Frame -> [Value] -> IO ()
setListReg Frame{..} vs = writeIORef listRegRef vs

getListReg :: Frame -> IO [Value]
getListReg Frame{..} = readIORef listRegRef

setReg :: IsValue v => Frame -> Reg -> v -> IO ()
setReg Frame { .. } reg v0 =
  case reg of
    Reg (Code.Reg r) -> IOVector.write regs r val
    TMP a b        -> case toValue v0 of
                        VVal v -> modifyIORef' regsTMP (Map.insert (a,b) v)
                        VRef _ -> error "[bug] reference in a TMP register"

  where
  val = toValue v0


getReg :: Frame -> Reg -> IO V
getReg Frame { .. } reg =
  case reg of
    Reg (Code.Reg r) -> IOVector.read regs r
    TMP a b -> do m <- readIORef regsTMP
                  case Map.lookup (a,b) m of
                    Just v  -> return (VVal v)
                    Nothing -> crash ("Read from bad reg: " ++ show (pp reg))


getExpr :: Frame -> Expr -> IO V
getExpr f expr =
  case expr of
    EReg r -> getReg f r
    EUp (Code.UpIx x)  -> VRef <$> IOVector.read (upvals f) x
    ELit l ->
      case l of
        LNil           -> return (VVal Nil)
        LBool b        -> return (VVal (Bool b))
        LNum d         -> return (VVal (Number (Double d)))
        LInt n         -> return (VVal (Number (Int n)))
        LStr bs        -> (VVal . String) <$> fromByteString bs

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

--------------------------------------------------------------------------------

performAssign :: Frame -> Reg -> Expr -> IO Next
performAssign f r e = do setReg f r =<< getExpr f e
                         return Continue


performNewTable :: AllocRef -> Frame -> Int -> Reg -> IO Next
performNewTable aref f pc r =
  do let refLoc = RefLoc { refLocSite   = InLua (ourFID f) pc
                         , refLocCaller = ourCaller f
                         }
     setReg f r =<< newTable aref refLoc 0 0
     return Continue


performLookupTable :: Frame -> Reg -> Reg -> Expr -> IO Next
performLookupTable f res tab ix =
  do t <- isTable =<< getReg f tab
     v <- isValue =<< getExpr f ix
     setReg f res =<< getTableRaw t v
     return Continue


performSetTable :: Frame -> Reg -> Expr -> Expr -> IO Next
performSetTable f tab ix val =
  do t <- isTable =<< getReg f tab
     i <- isValue =<< getExpr f ix
     v <- isValue =<< getExpr f val
     setTableRaw t i v
     return Continue


performSetTableList :: Frame -> Reg -> Int -> IO Next
performSetTableList f tab ix =
  do t  <- isTable =<< getReg f tab
     xs <- getListReg f
     zipWithM_ (setTableRaw t) (map (Number . Int) [ ix .. ]) xs
     return Continue


performGetMeta :: Frame -> Reg -> Expr -> IO Next
performGetMeta f r e =
  do v <- isValue =<< getExpr f e
     let setMb mb = do case mb of
                         Nothing -> setReg f r ()
                         Just t  -> setReg f r t
                       return Continue
     tabs <- readIORef (metaTables f)
     let tyMeta t = setMb (Map.lookup t tabs)

     case v of

       Table tr     -> setMb =<< getTableMeta tr
       UserData ur  -> setMb =<< readIORef (userDataMeta (referenceVal ur))
       _            -> tyMeta (valueType v)


performRaise :: Frame -> Expr -> IO Next
performRaise f e =
  do v <- isValue =<< getExpr f e
     return (RaiseError v)


performCase :: Frame ->
               Expr -> [(ValueType,BlockName)] -> Maybe BlockName -> IO Next
performCase f e as d =
  do v <- isValue =<< getExpr f e
     case lookup (valueType v) as of
       Just b  -> return (EnterBlock b)
       Nothing ->
         case d of
           Nothing -> crash "Stuck case."
           Just b  -> return (EnterBlock b)


performIf :: Frame -> Prop -> BlockName -> BlockName -> IO Next
performIf f p tr fa =
  do b <- getProp f p
     return (EnterBlock (if b then tr else fa))


performGoto :: BlockName -> IO Next
performGoto b = return (EnterBlock b)


performCall :: Frame -> Reg -> IO Next
performCall f clo =
  do fun <- isFunction =<< getReg f clo
     vs  <- getListReg f
     return (MakeCall fun vs)


performTailCall :: Frame -> Reg -> IO Next
performTailCall f clo =
  do fun <- isFunction =<< getReg f clo
     vs  <- getListReg f
     return (MakeTailCall fun vs)


performReturn :: Frame -> IO Next
performReturn f =
  do vs <- getListReg f
     return (ReturnWith vs)


performNewClosure :: AllocRef -> Frame -> Int ->
                                        Reg -> Int -> Code.Function -> IO Next
performNewClosure aref f pc res ix fun =
  do rs <- mapM (isReference <=< getExpr f) (funcUpvalExprs fun)

     let fid    = ourFID f
         fu     = luaFunction (Code.subFun fid ix) fun
         refLoc = RefLoc { refLocSite   = InLua fid pc
                         , refLocCaller = ourCaller f
                         }

     clo <- newClosure aref refLoc fu =<< Vector.thaw (Vector.fromList rs)
     setReg f res clo
     return Continue


lregRef :: Frame -> ListReg -> IORef [Value]
lregRef f r =
  case r of
    ArgReg  -> argRegRef f
    ListReg -> listRegRef f


performDrop :: Frame -> ListReg -> Int -> IO Next
performDrop f res n =
  do modifyIORef' (lregRef f res) (drop n)
     return Continue


performSetList :: Frame -> ListReg -> [Expr] -> IO Next
performSetList f res es =
  do vs <- mapM (isValue <=< getExpr f) es
     writeIORef (lregRef f res) vs
     return Continue


performAppend :: Frame -> ListReg -> [Expr] -> IO Next
performAppend f res es =
  do vs <- mapM (isValue <=< getExpr f) es
     modifyIORef (lregRef f res) (vs ++)
     return Continue


performIndexList :: Frame -> Reg -> ListReg -> Int -> IO Next
performIndexList f res lst ix =
  do vs <- readIORef (lregRef f lst)
     case splitAt ix vs of
       (_,b:_) -> setReg f res b
       _       -> setReg f res ()
     return Continue


performNewRef :: Frame -> Reg -> Expr -> IO Next
performNewRef f res val =
  do setReg f res =<< newIORef =<< isValue =<< getExpr f val
     return Continue


performReadRef :: Frame -> Reg -> Expr -> IO Next
performReadRef f res ref =
  do r <- isReference =<< getExpr f ref
     setReg f res =<< readIORef r
     return Continue


performWriteRef :: Frame -> Expr -> Expr -> IO Next
performWriteRef f ref val =
  do r <- isReference =<< getExpr f ref
     v <- isValue =<< getExpr f val
     writeIORef r v
     return Continue



performArith1 :: Frame -> Reg -> Op1 -> Expr -> IO Next
performArith1 f res op e =
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


performArith2 :: Frame -> Reg -> Op2 -> Expr -> Expr -> IO Next
performArith2 f res op e1 e2 =
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


runStmt :: AllocRef -> Frame -> Int -> BlockStmt -> IO Next
runStmt aref f pc stmt =
  case stmtCode stmt of
    Comment _               -> return Continue
    Assign r e              -> performAssign f r e

    -- Tables
    NewTable r              -> performNewTable aref f pc r
    LookupTable res tab ix  -> performLookupTable f res tab ix
    SetTable tab ix val     -> performSetTable f tab ix val
    SetTableList tab ix     -> performSetTableList f tab ix
    GetMeta r e             -> performGetMeta f r e

    -- Local control flow
    Case e as d             -> performCase f e as d
    If p tr fa              -> performIf f p tr fa
    Goto b                  -> performGoto b

    -- Functions
    NewClosure res ix fun   -> performNewClosure aref f pc res ix fun
    Call clo                -> performCall f clo
    TailCall clo            -> performTailCall f clo
    Return                  -> performReturn f
    Raise e                 -> performRaise f e

    -- Lists
    Drop r n                -> performDrop f r n
    SetList res es          -> performSetList f res es
    Append res es           -> performAppend f res es
    IndexList res lst ix    -> performIndexList f res lst ix

    -- References
    NewRef res val          -> performNewRef f res val
    ReadRef res ref         -> performReadRef f res ref
    WriteRef ref val        -> performWriteRef f ref val

    -- Arithmetic
    Arith1 res op e         -> performArith1 f res op e
    Arith2 res op e1 e2     -> performArith2 f res op e1 e2

    -- Misc

    -- Errors
    CloseStack _            -> crash "CloseStack in phase 2"
    SetUpVal _ _            -> crash "SetUpVal in phase 2"


runStmtAt :: AllocRef -> Frame -> Vector BlockStmt -> Int -> IO Next
runStmtAt aref f curBlock pc =
  case curBlock Vector.!? pc of
    Just stmt -> runStmt aref f pc stmt
    Nothing   -> crash ("Invalid PC: " ++ show pc)




