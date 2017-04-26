{-# LANGUAGE RecordWildCards, NamedFieldPuns, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Galua.Micro.Type.Eval
  ( analyze, Result(..), GlobalBlockName(..), QualifiedBlockName(..)
  , valueCasesM
  ) where

import           Control.Monad
import           Data.Map ( Map )
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.ByteString as BS
import           Text.PrettyPrint
import           Debug.Trace

import Galua.Micro.AST
import Galua.Micro.Type.Value
import Galua.Micro.Type.Pretty()
import Galua.Micro.Type.Monad
import Galua.Pretty


regCasesM :: Reg -> BlockM SingleV
regCasesM reg =
  do regVal <- getReg reg
     value  <- regValCasesM regVal
     assign reg (RegVal (fromSingleV value))
     return value

exprCasesM :: Expr -> BlockM SingleV
exprCasesM = regValCasesM <=< evalExpr

regValCasesM :: RegVal -> BlockM SingleV
regValCasesM = valueCasesM <=< regValToVal

valueCasesM :: AnalysisM m => Value -> m SingleV
valueCasesM v = options (valueCases v)



data Next = EnterBlock BlockName
          | RaiseError Value
          | Continue
          | ReturnWith (List Value)



-- Does not return bottom
evalExpr :: Expr -> BlockM RegVal
evalExpr expr =
  case expr of
    EReg r -> getReg r
    EUp u  -> getUpVal u
    ELit l ->
      case l of
        LNil          -> ret (BasicValue Nil)
        LBool b       -> ret (BooleanValue (Just b))
        LNum _        -> ret (BasicValue Number)
        LInt _        -> ret (BasicValue Number)
        LStr b        -> ret (StringValue (Just b))
  where
  ret = return . RegVal . fromSingleV


regValToRef :: RegVal -> BlockM (Maybe RefId)
regValToRef rv =
  case rv of
    RegBottom -> impossible
    RegVal _  -> error "[bug] regValToRef got a val, not a ref"
    RegRef r  -> case r of
                   Top       -> return Nothing
                   NotTop xs -> Just <$> options (Set.toList xs)

regValToVal :: RegVal -> BlockM Value
regValToVal rv =
  case rv of
    RegBottom -> impossible
    RegVal v  -> return v
    RegRef _  -> error "[bu] regValToVal got a ref, not a val"


raiseError :: Value -> BlockM Next
raiseError v =
  do raisesError v
     return (RaiseError v)



evalStmt :: BlockStmt -> BlockM Next
evalStmt stmt =
  logTrace ("STMT: " ++ show (pp stmt)) >>

  case stmtCode stmt of

    Assign r e -> do assign r =<< evalExpr e
                     return Continue

    SetUpVal {} -> error "SetUpVal"

    NewTable r ->
      do here <- newTableId
         assign r (RegVal (newTable here))
         return Continue

    LookupTable r t i ->
      do TableValue l <- regCasesM t
         ti           <- exprCasesM i
         assign r =<< fmap RegVal (getTable l ti)
         return Continue

    SetTable t i v ->
      do val          <- regValToVal =<< evalExpr v
         TableValue l <- regCasesM t
         ti           <- regValToVal =<< evalExpr i
         setTable l ti val
         return Continue


    SetTableList t _ ->
      do vs <- getList ListReg
         let vr = appListAll vs
         TableValue tv <- regCasesM t
         setTable tv (basic Number) vr
         return Continue

    GetMeta r e ->
      -- Note: XXX: we don't really need to expand function values completely,
      -- as they all give the same result.
      do v    <- exprCasesM e
         GlobalState { basicMetas, stringMeta, funMeta, booleanMeta } <- getGlobal
         newV <- case v of
                   TableValue l -> getTableMeta l
                   BasicValue t     -> return (appFun basicMetas t)
                   StringValue _    -> return stringMeta
                   BooleanValue{}   -> return booleanMeta
                   FunctionValue _  -> return funMeta
         when (newV == bottom) impossible
         assign r (RegVal newV)
         return Continue


    -- End of block

    Raise e -> raiseError =<< regValToVal =<< evalExpr e

    Goto b -> return (EnterBlock b)

    Case e alts dflts ->
      do v <- regValToVal =<< evalExpr e
         let inDflt = case dflts of
                        Nothing -> impossible
                        Just bn -> upd bn (foldl rmAlt v (map fst alts))


             alt (vt,bn) =
               let check v' = upd bn (v /\ v')
               in case vt of
                    NumberType        -> check (basic Number)
                    NilType           -> check (basic Nil)
                    UserDataType      -> check (basic UserData)
                    LightUserDataType -> check (basic LightUserData)
                    ThreadType        -> check (basic Thread)

                    BoolType ->
                      check bottom { valueBoolean = valueBoolean v }

                    StringType ->
                      check bottom { valueString = valueString v }

                    FunctionType ->
                       check bottom { valueFunction = valueFunction v }

                    TableType ->
                        check bottom { valueTable = valueTable v }


         join $ options (inDflt : map alt alts)

      where
      upd bn v' | v' == bottom = impossible
                | otherwise    = do case e of
                                      EReg r -> assign r (RegVal v')
                                      ELit _ -> return ()
                                      EUp _  -> error "up index in case"
                                    return (EnterBlock bn)

      rmAlt v' ty =
        let rmBasic t = v' { valueBasic = Set.delete t (valueBasic v') }
        in case ty of
             NumberType        -> rmBasic Number
             NilType           -> rmBasic Nil
             UserDataType      -> rmBasic UserData
             LightUserDataType -> rmBasic LightUserData
             ThreadType        -> rmBasic Thread

             BoolType          -> v' { valueBoolean  = bottom }
             StringType        -> v' { valueString   = bottom }
             FunctionType      -> v' { valueFunction = bottom }
             TableType         -> v' { valueTable    = bottom }



    If (Prop p es) t f ->
      do vs <- mapM (regValToVal <=< evalExpr) es
         case (p,vs) of
           (Equals, [v1,v2]) ->
             do let newV  = v1 /\ v2
                    upd e = case e of
                              EReg r -> assign r (RegVal newV)
                              ELit _ -> return ()
                              EUp _  -> error "UpVal in if?"

                -- XXX: propagate some -ve info?
                -- Currently (/= nil) seems the only possibly interesting one,
                -- but maybe tracking some -ve information might be useful.
                if newV == bottom
                  then return (EnterBlock f)
                  else join
                     $ options
                         [ do upd (es !! 0)
                              upd (es !! 1)
                              return (EnterBlock t)
                         , return (EnterBlock f)
                         ]

           _ -> options [ EnterBlock t, EnterBlock f ]


    NewClosure r proto fun ->
      do let ups = funcUpvalExprs fun
         upRs' <- mapM (regValToRef <=< evalExpr) ups
         let upRs = case sequence upRs' of
                      Just rs -> rs
                      Nothing -> error "NewClosure: TOP references?"
         fid  <- newFunId proto upRs
         assign r (RegVal (newFun fid))
         return Continue


    Return -> do vs <- getList ListReg
                 return (ReturnWith vs)

    CloseStack _ -> error "CloseStack"

    -- Functions
    Call fun ->
      do vs <- getList ListReg
         FunctionValue fid <- regCasesM fun
         next <- callFun fid vs
         case next of
           Left v   -> raiseError v
           Right xs -> do setList ListReg xs
                          return Continue

    TailCall fun ->
      do vs  <- getList ListReg
         FunctionValue fid <- regCasesM fun
         next <- callFun fid vs
         case next of
           Left v   -> raiseError v
           Right xs -> return (ReturnWith xs)

    -- Lists
    Append xs es ->
      do ves <- mapM (regValToVal <=< evalExpr) es
         oldvs <- getList xs
         setList xs (listAppend ves oldvs)
         return Continue

    SetList xs es ->
      do ves <- mapM (regValToVal <=< evalExpr) es
         let nil = listConst (basic Nil)
         setList xs (listAppend ves nil)
         return Continue

    Drop xs n ->
      do vs <- getList xs
         setList xs (listDrop n vs)
         return Continue

    IndexList r xs n ->
      do vs <- getList xs
         assign r (RegVal (appList vs n))
         return Continue

    -- Arith
    Arith2 r op e1 e2 ->
      do ve1 <- regValToVal =<< evalExpr e1
         ve2 <- regValToVal =<< evalExpr e2
         let vr = case op of
                    NumberAdd   -> basic Number
                    NumberSub   -> basic Number
                    NumberMul   -> basic Number
                    NumberPow   -> basic Number

                    FMod        -> basic Number
                    IMod        -> basic Number
                    IDiv        -> basic Number
                    NumberDiv   -> basic Number

                    And         -> basic Number
                    Or          -> basic Number
                    Xor         -> basic Number
                    Shl         -> basic Number
                    Shr         -> basic Number

                    Concat ->
                      case (valueString ve1, valueString ve2) of
                        (OneValue x, OneValue y) ->
                           bottom { valueString = OneValue (BS.append x y) }
                        _ -> anyString
         assign r (RegVal vr)
         return Continue

    Arith1 r op e ->
      do ve <- regValToVal =<< evalExpr e
         let vr = case op of
                    ToNumber ->
                      let veNum    = ve /\ basic Number
                      in joins
                           [ veNum
                           , if veNum /= ve  then basic Nil else bottom
                           ]

                    ToInt -> basic Nil \/
                             (if ve /\ (basic Number \/ anyString) /= bottom
                               then basic Number else bottom)

                    ToString ->
                     let veNum    = ve /\ basic Number
                         veStr    = ve /\ anyString
                         veNumStr = ve /\ (basic Number \/ anyString)
                     in joins
                          [ veStr
                          , if veNumStr /= ve     then basic Nil else bottom
                          , if veNum    /= bottom then anyString else bottom
                          ]

                    IntToDouble      -> basic Number
                    StringLen        -> basic Number
                    TableLen         -> basic Number
                    NumberUnaryMinus -> basic Number
                    Complement       -> basic Number

                    ToBoolean        -> anyBoolean -- TODO: Implement logic
                    BoolNot          -> anyBoolean
         assign r (RegVal vr)
         return Continue


    -- References
    NewRef r e ->
      do ve   <- regValToVal =<< evalExpr e
         here <- newRefId ve
         assign r (newRef here)
         return Continue

    ReadRef r e ->
      do ref <- regValToRef =<< evalExpr e
         vr  <- readRefId ref
         assign r (RegVal vr)
         return Continue

    WriteRef r e ->
      do ref <- regValToRef =<< evalExpr r
         ve  <- regValToVal =<< evalExpr e
         writeRefId ref ve
         return Continue

    -- Misc
    Comment _ -> return Continue



--------------------------------------------------------------------------------


evalBlock :: AnalysisM m => GlobalBlockName -> State -> m (Next, State)
evalBlock bn s = inBlock bn s go
  where
  go = do logTrace ("BLOCK: " ++ show (pp bn))
          next <- evalStmt =<< curStmt
          case next of
            Continue -> continue >> go
            _        -> return next


callFun :: Maybe ClosureId -> List Value -> BlockM (Either Value (List Value))
callFun mb as = doCall =<< maybe anyFunId return mb
  where doCall fid = do (callsite,save) <- getCont
                        glob            <- getGlobal
                        (next,newG)     <- evalFun callsite fid as glob
                        setGlobal newG
                        setCont save
                        return next



evalFun :: AnalysisM m =>
            CallsiteId -> ClosureId -> List Value -> GlobalState ->
                                    m (Either Value (List Value), GlobalState)
evalFun caller cid as glob =
  case functionFID funV of
    NoValue -> impossible
    MultipleValues -> error "evalFun: multiple values" -- todo: report intractibility
    OneValue (CFunImpl ptr) ->
      do mbPrim  <- getPrim ptr
         case mbPrim of
           -- XXX: UNSOUND!!!
           -- For now we just treat C functions as if they don't do anything.
           Nothing -> return (Right (listConst topVal), glob)
           Just (PrimImpl prim) -> prim glob as

    OneValue (LuaFunImpl fid) ->
      do let locals = LocalState
                        { env     = bottom
                        , argReg  = as
                        , listReg = bottom
                        , upvals  = functionUpVals funV
                        }
             block b = GlobalBlockName caller (QualifiedBlockName fid b)

             go b s  =
               do (next,s1) <- evalBlock (block b) s
                  case next of
                    Continue      -> error "Continue"
                    EnterBlock b1 -> go b1 s1
                    RaiseError v  -> return (Left v, s1)
                    ReturnWith xs -> return (Right xs, s1)

         (next,s1) <- go EntryBlock State { localState = locals
                                          , globalState = glob }
         return (next, globalState s1)
  where
  funV = functions glob Map.! cid

data Result = Result
  { resReturns      :: List Value
  , resRaises       :: Value
  , resGlobals      :: GlobalState
  , resStates       :: Map GlobalBlockName State
  , resBlockRaises  :: Map GlobalBlockName Value
  } deriving Show

instance Pretty Result where
  pp Result { .. } =
    vcat [ entry "returns" resReturns
         , entry "raises"  resRaises
         , entry "finals"  resGlobals
         , entry "all_states" resStates
         , entry "raise_states" resBlockRaises
         ]
    where
    entry x y
      | y == bottom = empty
      | otherwise   = x <> colon $$ nest 2 (pp y)

{-
analyze ::
  Map FunId Function ->
  Map CFun PrimImpl ->
  ClosureId ->
  List Value ->
  GlobalState -> Result
analyze funs prims cid args glob =
  Result { resReturns     = joins [ v | Right v <- nexts ]
         , resRaises      = joins [ v | Left  v <- nexts ]
         , resGlobals     = joins gs
         , resStates      = joins states
         , resBlockRaises = joins errs
         }
  where
  (nexts,gs)         = unzip rss
  (rss, states,errs)
                     = unzip3
                     $ allPaths funs prims
                     $ evalFun initialCaller cid args glob
-}

analyze ::
  Map FunId MicroFunction ->
  Map CFun PrimImpl ->
  ClosureId ->
  List Value ->
  GlobalState -> Result
analyze funs prims cid args glob =
  trace (unlines logs) $
  Result { resReturns     = joins [ v | Right v <- nexts ]
         , resRaises      = joins [ v | Left  v <- nexts ]
         , resGlobals     = joins gs
         , resStates      = states
         , resBlockRaises = errs
         }
  where
  (nexts,gs)          = unzip rss
  (rss, states, errs, logs)
                      = singlePath funs prims
                      $ evalFun initialCaller cid args glob



