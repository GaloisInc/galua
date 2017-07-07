{-# LANGUAGE RecordWildCards, FlexibleInstances, MultiWayIf #-}
module Galua.Micro.OpcodeInterpreter
  ( runStmtAt
  , Next(..)
  , crash
  , setListReg
  ) where

import           Data.IORef(IORef,newIORef,modifyIORef',modifyIORef,readIORef,
                            writeIORef)
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as IOVector
import qualified Data.Map as Map
import           Data.Bits(complement,(.&.),(.|.),xor)
import qualified Data.ByteString as BS
import           Control.Monad(zipWithM_)

import           Galua.Value
import           Galua.FunValue(luaFunction)
import           Galua.Number(Number(..),wordshiftL,wordshiftR,nummod,
                              numberPow,numberDiv)
import           Galua.LuaString
                   (LuaString,toByteString,fromByteString,luaStringLen)
import           Galua.Micro.AST
import qualified Galua.Code as Code
import           Galua.Micro.ExecEnv(MLuaExecEnv(..))
import           Galua.Mach( VM(..)
                           , machNewClosure, machNewTable, MachineEnv(..) )

data Next = EnterBlock BlockName
          | Continue
          | MakeCall (Reference Closure) [Value]
          | MakeTailCall (Reference Closure) [Value]
          | RaiseError Value
          | ReturnWith [Value]

class IsValue t where
  toValue :: t -> Value

instance IsValue Value where
  toValue = id

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

setListReg :: MLuaExecEnv -> [Value] -> IO ()
setListReg MLuaExecEnv{..} vs = writeIORef mluaExecListReg vs

getListReg :: MLuaExecEnv -> IO [Value]
getListReg MLuaExecEnv{..} = readIORef mluaExecListReg

setReg :: IsValue v => MLuaExecEnv -> Reg -> v -> IO ()
setReg MLuaExecEnv { .. } reg v0 =
  case reg of
    Reg (Code.Reg r)  -> do IOVector.unsafeWrite mluaExecRegsValue r $! val
                            IOVector.unsafeWrite mluaExecRegsRefs r
                             (error "(do not use)") -- to avoid memory leads
    Ref {}            -> crash "setReg: value in a ref registers"
    TMP _renumbured b -> IOVector.unsafeWrite mluaExecRegsTMP b   $! val

  where
  val = toValue v0

getReg :: MLuaExecEnv -> Reg -> IO Value
getReg MLuaExecEnv { .. } reg =
  case reg of
    Reg (Code.Reg r)  -> IOVector.unsafeRead mluaExecRegsValue r
    Ref {}            -> crash "getReg: Ref register"
    TMP _renumbured b -> IOVector.unsafeRead mluaExecRegsTMP b


setRefReg :: MLuaExecEnv -> Reg -> IORef Value -> IO ()
setRefReg MLuaExecEnv { .. } reg v0 =
  case reg of
    Ref (Code.Reg r) -> do IOVector.unsafeWrite mluaExecRegsRefs r v0
                           IOVector.unsafeWrite mluaExecRegsValue r
                             (error "(do not use)") -- to avoid memory leads
    Reg {} -> crash "setRefReg: ordinary registers do not contain references"
    TMP {} -> crash "setRefReg: temporary registers do not contain references"

getRefReg :: MLuaExecEnv -> Reg -> IO (IORef Value)
getRefReg MLuaExecEnv { .. } reg =
  case reg of
    Ref (Code.Reg r) -> IOVector.unsafeRead mluaExecRegsRefs r
    Reg {}           -> crash "[bug] getRefReg: ordinary register"
    TMP {}           -> crash "[bug] getRefReg: not a reference"

getRefExpr :: MLuaExecEnv -> Expr -> IO (IORef Value)
getRefExpr f expr =
  case expr of
    EReg r             -> getRefReg f r
    EUp (Code.UpIx x)  -> IOVector.unsafeRead (mluaExecUpvals f) x
    ELit {}            -> crash "getRefExpr: literals are not references"

getExpr :: MLuaExecEnv -> Expr -> IO Value
getExpr f expr =
  case expr of
    EReg r -> getReg f r
    EUp {} -> crash "getExpr: upvalues are references"
    ELit l ->
      case l of
        LNil           -> return Nil
        LBool b        -> return (Bool b)
        LNum d         -> return (Number (Double d))
        LInt n         -> return (Number (Int n))
        LStr bs        -> String <$> fromByteString bs

getProp :: MLuaExecEnv -> Prop -> IO Bool
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
                   [Bool b] -> return b
                   [Nil]    -> return False
                   [_]      -> return True
                   _        -> typeError "exactly 1 parameter"
       IsInteger -> unary isNumber $ \n -> case n of
                                             Int _ -> True
                                             _     -> False

       IsNaN -> unary isNumber $ \n -> case n of
                                         Int _    -> False
                                         Double d -> isNaN d

       Equals     -> binary return   (==)
       NumberLT   -> binary isNumber (<)
       NumberLEQ  -> binary isNumber (<=)
       StringLT   -> binary isString (<)
       StringLEQ  -> binary isString (<=)



typeError :: String -> IO a
typeError what = crash ("Type error: expected a " ++ what ++ ".")

crash :: String -> IO a
crash msg = fail msg


isTable :: Value -> IO (Reference Table)
isTable val =
  case val of
    Table t -> return t
    _       -> typeError "table"

isFunction :: Value -> IO (Reference Closure)
isFunction val =
  case val of
    Closure c -> return c
    _         -> typeError "function"


isNumber :: Value -> IO Number
isNumber val =
  case val of
    Number n -> return n
    _        -> typeError "number"

isInt :: Value -> IO Int
isInt val =
  case val of
    Number (Int n) -> return n
    _              -> typeError "integer"

isDouble :: Value -> IO Double
isDouble val =
  case val of
    Number (Double d) -> return d
    _                 -> typeError "double"

isString :: Value -> IO LuaString
isString val =
  case val of
    String s -> return s
    _        -> typeError "string"

isBool :: Value -> IO Bool
isBool val =
  case val of
    Bool b -> return b
    _      -> typeError "boolean"

--------------------------------------------------------------------------------

performAssign :: MLuaExecEnv -> Reg -> Expr -> IO Next
performAssign f r e = do setReg f r =<< getExpr f e
                         return Continue


performNewTable :: VM -> MLuaExecEnv -> Reg -> IO Next
performNewTable vm f r =
  do setReg f r =<< machNewTable vm 0 0
     return Continue


performLookupTable :: MLuaExecEnv -> Reg -> Reg -> Expr -> IO Next
performLookupTable f res tab ix =
  do t <- isTable =<< getReg f tab
     v <- getExpr f ix
     setReg f res =<< getTableRaw t v
     return Continue


performSetTable :: MLuaExecEnv -> Reg -> Expr -> Expr -> IO Next
performSetTable f tab ix val =
  do t <- isTable =<< getReg f tab
     i <- getExpr f ix
     v <- getExpr f val
     setTableRaw t i v
     return Continue


performSetTableList :: MLuaExecEnv -> Reg -> Int -> IO Next
performSetTableList f tab ix =
  do t  <- isTable =<< getReg f tab
     xs <- getListReg f
     zipWithM_ (setTableRaw t) (map (Number . Int) [ ix .. ]) xs
     return Continue


performGetMeta :: VM -> MLuaExecEnv -> Reg -> Expr -> IO Next
performGetMeta vm f r e =
  do v <- getExpr f e
     let setMb mb = do case mb of
                         Nothing -> setReg f r ()
                         Just t  -> setReg f r t
                       return Continue

     tabs <- readIORef (machMetatablesRef (vmMachineEnv vm))
     let tyMeta t = setMb (Map.lookup t tabs)

     case v of

       Table tr     -> setMb =<< getTableMeta tr
       UserData ur  -> setMb =<< readIORef (userDataMeta (referenceVal ur))
       _            -> tyMeta (valueType v)


performRaise :: MLuaExecEnv -> Expr -> IO Next
performRaise f e =
  do v <- getExpr f e
     return (RaiseError v)


performCase :: MLuaExecEnv ->
               Expr -> [(ValueType,BlockName)] -> Maybe BlockName -> IO Next
performCase f e as d =
  do v <- getExpr f e
     case lookup (valueType v) as of
       Just b  -> return (EnterBlock b)
       Nothing ->
         case d of
           Nothing -> crash "Stuck case."
           Just b  -> return (EnterBlock b)


performIf :: MLuaExecEnv -> Prop -> BlockName -> BlockName -> IO Next
performIf f p tr fa =
  do b <- getProp f p
     return (EnterBlock (if b then tr else fa))


performGoto :: BlockName -> IO Next
performGoto b = return (EnterBlock b)


performCall :: MLuaExecEnv -> Reg -> IO Next
performCall f clo =
  do fun <- isFunction =<< getReg f clo
     vs  <- getListReg f
     return (MakeCall fun vs)


performTailCall :: MLuaExecEnv -> Reg -> IO Next
performTailCall f clo =
  do fun <- isFunction =<< getReg f clo
     vs  <- getListReg f
     return (MakeTailCall fun vs)


performReturn :: MLuaExecEnv -> IO Next
performReturn f =
  do vs <- getListReg f
     return (ReturnWith vs)


performNewClosure :: VM -> MLuaExecEnv -> Reg -> Int -> Code.Function -> IO Next
performNewClosure vm f res ix fun =
  do rs <- mapM (getRefExpr f) (funcUpvalRefExprs fun)

     let fid    = mluaExecFID f
         fu     = luaFunction (Code.subFun fid ix) fun

     setReg f res =<< machNewClosure vm fu =<< Vector.thaw (Vector.fromList rs)
     return Continue

lregRef :: MLuaExecEnv -> ListReg -> IORef [Value]
lregRef f r =
  case r of
    ArgReg  -> mluaExecArgReg f
    ListReg -> mluaExecListReg f


performDrop :: MLuaExecEnv -> ListReg -> Int -> IO Next
performDrop f res n =
  do modifyIORef' (lregRef f res) (drop n)
     return Continue


performSetList :: MLuaExecEnv -> ListReg -> [Expr] -> IO Next
performSetList f res es =
  do vs <- mapM (getExpr f) es
     writeIORef (lregRef f res) vs
     return Continue


performAppend :: MLuaExecEnv -> ListReg -> [Expr] -> IO Next
performAppend f res es =
  do vs <- mapM (getExpr f) es
     modifyIORef (lregRef f res) (vs ++)
     return Continue


performIndexList :: MLuaExecEnv -> Reg -> ListReg -> Int -> IO Next
performIndexList f res lst ix =
  do vs <- readIORef (lregRef f lst)
     case splitAt ix vs of
       (_,b:_) -> setReg f res b
       _       -> setReg f res ()
     return Continue


performNewRef :: MLuaExecEnv -> Reg -> Expr -> IO Next
performNewRef f res val =
  do setRefReg f res =<< newIORef =<< getExpr f val
     return Continue


performReadRef :: MLuaExecEnv -> Reg -> Expr -> IO Next
performReadRef f res ref =
  do r <- getRefExpr f ref
     setReg f res =<< readIORef r
     return Continue


performWriteRef :: MLuaExecEnv -> Expr -> Expr -> IO Next
performWriteRef f ref val =
  do r <- getRefExpr f ref
     v <- getExpr f val
     writeIORef r v
     return Continue



performArith1 :: MLuaExecEnv -> Reg -> Op1 -> Expr -> IO Next
performArith1 f res op e =
  do v <- getExpr f e
     case op of

       ToNumber ->
         case valueNumber v of
           Just n  -> setReg f res n
           Nothing -> setReg f res ()

       ToInt ->
         case valueInt v of
           Just n  -> setReg f res n
           Nothing -> setReg f res ()

       IntToDouble ->
         do n <- isInt v
            setReg f res (fromIntegral n :: Double)

       ToString ->
         case valueString v of
           Just s  -> setReg f res =<< fromByteString s
           Nothing -> setReg f res ()

       ToBoolean -> setReg f res (valueBool v)

       StringLen   -> setReg f res . luaStringLen =<< isString v

       TableLen    -> setReg f res =<< tableLen =<< isTable v

       NumberUnaryMinus ->
         do d <- isNumber v
            setReg f res (negate d)

       Complement  ->
         do i <- isInt v
            setReg f res (complement i)

       BoolNot ->
         do b <- isBool v
            setReg f res (not b)


     return Continue


performArith2 :: MLuaExecEnv -> Reg -> Op2 -> Expr -> Expr -> IO Next
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


runEndStmt :: MLuaExecEnv -> BlockStmt EndStmt -> IO Next
runEndStmt f stmt =
  case stmtCode stmt of
    Case e as d             -> performCase f e as d
    If p tr fa              -> performIf f p tr fa
    Goto b                  -> performGoto b

    TailCall clo            -> performTailCall f clo
    Return                  -> performReturn f
    Raise e                 -> performRaise f e




runStmt :: VM -> MLuaExecEnv -> BlockStmt Stmt -> IO Next
runStmt vm f stmt =
  case stmtCode stmt of
    Comment _               -> return Continue
    Assign r e              -> performAssign f r e

    -- Tables
    NewTable r              -> performNewTable vm f r
    LookupTable res tab ix  -> performLookupTable f res tab ix
    SetTable tab ix val     -> performSetTable f tab ix val
    SetTableList tab ix     -> performSetTableList f tab ix
    GetMeta r e             -> performGetMeta vm f r e

    -- Functions
    NewClosure res ix fun   -> performNewClosure vm f res ix fun
    Call clo                -> performCall f clo

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


runStmtAt :: VM -> MLuaExecEnv -> Block -> Int -> IO Next
runStmtAt vm f curBlock pc =
  case blockBody curBlock Vector.!? pc of
    Just stmt -> runStmt vm f stmt
    Nothing   -> runEndStmt f (blockEnd curBlock)





