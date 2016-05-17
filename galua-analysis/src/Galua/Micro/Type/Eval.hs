{-# LANGUAGE RecordWildCards, NamedFieldPuns, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Galua.Micro.Type.Eval
  ( analyze, Result(..), GlobalBlockName(..), QualifiedBlockName(..)
  ) where

import           Data.Map ( Map )
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.ByteString as BS
import           Text.PrettyPrint

import           MonadLib hiding (raises)

import Galua.Micro.AST
import Galua.Micro.Type.Value
import Galua.Micro.Type.Pretty()
import Language.Lua.Bytecode.Pretty(PP(..),pp)
import Galua.Micro.Type.Monad



valueCasesM :: AnalysisM m => Value -> m SingleV
valueCasesM v = options (valueCases v)



data Next = EnterBlock BlockName
          | RaiseError Value
          | Continue
          | ReturnWith (List Value)



-- Does not return bottom
evalExpr :: Expr -> BlockM Value
evalExpr expr =
  case expr of
    EReg r -> getReg r
    EUp u  -> getUpVal u
    ELit l ->
      case l of
        KNil          -> ret (BasicValue Nil)
        KBool _       -> ret (BasicValue Bool)
        KNum _        -> ret (BasicValue Number)
        KInt _        -> ret (BasicValue Number)
        KString b     -> ret (StringValue (Just b))
        KLongString b -> ret (StringValue (Just b))
  where
  ret = return . fromSingleV


raiseError :: Value -> BlockM Next
raiseError v =
  do raisesError v
     return (RaiseError v)



evalStmt :: Stmt -> BlockM Next
evalStmt stmt =

  case stmt of

    Assign r e -> do assign r =<< evalExpr e
                     return Continue

    SetUpVal {} -> error "SetUpVal"

    NewTable r ->
      do here <- newTableId
         assign r (newTable here)
         return Continue

    LookupTable r t i ->
      do TableValue l <- valueCasesM =<< getReg t
         ti           <- valueCasesM =<< evalExpr i
         assign r =<< getTable l ti
         return Continue

    SetTable t i v ->
      do val          <- evalExpr v
         TableValue l <- valueCasesM =<< getReg  t
         ti           <- valueCasesM =<< evalExpr i
         setTable l ti val
         return Continue


    SetTableList t _ ->
      do vs <- getList ListReg
         let vr = appListAll vs
         TableValue tv <- valueCasesM =<< getReg t
         setTable tv (BasicValue Number) vr
         return Continue

    GetMeta r e ->
      -- Note: XXX: we don't really need to expand function values completely,
      -- as they all give the same result.
      do v    <- valueCasesM =<< evalExpr e
         GlobalState { basicMetas, stringMeta, funMeta } <- getGlobal
         newV <- case v of
                   TableValue l -> getTableMeta l
                   BasicValue t     -> return (appFun basicMetas t)
                   StringValue _    -> return stringMeta
                   FunctionValue _  -> return funMeta
                   RefValue {}      -> impossible
         when (newV == bottom) impossible
         assign r newV
         return Continue


    -- End of block

    Raise e -> raiseError =<< evalExpr e

    Goto b -> return (EnterBlock b)

    Case e alts dflts ->
      do v <- evalExpr e
         let inDflt = case dflts of
                        Nothing -> impossible
                        Just bn -> upd bn (foldl rmAlt v (map fst alts))


             alt (vt,bn) =
               let check v' = upd bn (v /\ v')
               in case vt of
                    NumberType        -> check (basic Number)
                    BoolType          -> check (basic Bool)
                    NilType           -> check (basic Nil)
                    UserDataType      -> check (basic UserData)
                    LightUserDataType -> check (basic LightUserData)
                    ThreadType        -> check (basic Thread)

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
                                      EReg r -> assign r v'
                                      ELit _ -> return ()
                                      EUp _  -> error "up index in case"
                                    return (EnterBlock bn)

      rmAlt v' ty =
        let rmBasic t = v' { valueBasic = Set.delete t (valueBasic v') }
        in case ty of
             NumberType        -> rmBasic Number
             BoolType          -> rmBasic Bool
             NilType           -> rmBasic Nil
             UserDataType      -> rmBasic UserData
             LightUserDataType -> rmBasic LightUserData
             ThreadType        -> rmBasic Thread

             StringType        -> v' { valueString   = bottom }
             FunctionType      -> v' { valueFunction = bottom }
             TableType         -> v' { valueTable    = bottom }



    If (Prop p es) t f ->
      do vs <- mapM evalExpr es
         case (p,vs) of
           (Equals, [v1,v2]) ->
             do let newV  = v1 /\ v2
                    upd e = case e of
                              EReg r -> assign r newV
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


    NewClosure r proto ups ->
      do upRs <- mapM evalExpr ups
         let toRef v = case valueCases v of
                         [ RefValue (Just ref) ] -> ref
                         _                -> error "toRef: not a single ref"
         fid  <- newFunId proto (map toRef upRs)
         assign r (newFun fid)
         return Continue


    Return -> do vs <- getList ListReg
                 return (ReturnWith vs)

    CloseStack _ -> error "CloseStack"

    -- Functions
    Call fun ->
      do vs <- getList ListReg
         FunctionValue fid <- valueCasesM =<< getReg fun
         next <- callFun fid vs
         case next of
           Left v   -> raiseError v
           Right xs -> do setList ListReg xs
                          return Continue

    TailCall fun ->
      do vs  <- getList ListReg
         FunctionValue fid <- valueCasesM =<< getReg fun
         next <- callFun fid vs
         case next of
           Left v   -> raiseError v
           Right xs -> return (ReturnWith xs)

    -- Lists
    Append xs es ->
      do ves <- mapM evalExpr es
         oldvs <- getList xs
         setList xs (listAppend ves oldvs)
         return Continue

    SetList xs es ->
      do ves <- mapM evalExpr es
         let nil = listConst (basic Nil)
         setList xs (listAppend ves nil)
         return Continue

    Drop xs n ->
      do vs <- getList xs
         setList xs (listDrop n vs)
         return Continue

    IndexList r xs n ->
      do vs <- getList xs
         assign r (appList vs n)
         return Continue

    -- Arith
    Arith2 r op e1 e2 ->
      do ve1 <- evalExpr e1
         ve2 <- evalExpr e2
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
         assign r vr
         return Continue

    Arith1 r op e ->
      do ve <- evalExpr e
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

                    ToBoolean        -> basic Bool
                    IntToDouble      -> basic Number
                    StringLen        -> basic Number
                    TableLen         -> basic Number
                    NumberUnaryMinus -> basic Number
                    Complement       -> basic Number
                    BoolNot          -> basic Bool
         assign r vr
         return Continue


    -- References
    NewRef r e ->
      do ve   <- evalExpr e
         here <- newRefId ve
         assign r (newRef here)
         return Continue

    ReadRef r e ->
      do ve <- evalExpr e
         RefValue ref <- valueCasesM ve
         vr           <- readRefId ref
         assign r vr
         return Continue

    WriteRef r e ->
      do vr           <- evalExpr r
         RefValue ref <- valueCasesM vr
         ve           <- evalExpr e
         writeRefId ref ve
         return Continue

    -- Misc
    Comment _ -> return Continue



--------------------------------------------------------------------------------


evalBlock :: AnalysisM m => GlobalBlockName -> State -> m (Next, State)
evalBlock bn s = inBlock bn s go
  where
  go = do next <- evalStmt =<< curStmt
          case next of
            Continue -> continue >> go
            _        -> return next


callFun :: Maybe FunId -> List Value -> BlockM (Either Value (List Value))
callFun mb as = doCall =<< maybe anyFunId return mb
  where doCall fid = do (callsite,save) <- getCont
                        glob        <- getGlobal
                        (next,newG) <- evalFun callsite fid as glob
                        setGlobal newG
                        setCont save
                        return next



evalFun :: AnalysisM m => CallsiteId -> FunId -> List Value -> GlobalState ->
                                    m (Either Value (List Value), GlobalState)
evalFun caller fid as glob =
  do let locals = LocalState
                    { env     = bottom
                    , argReg  = as
                    , listReg = bottom
                    }

     (next,s1) <- go EntryBlock State { localState = locals
                                      , globaleState = glob }
     return (next, globaleState s1)
  where
  go b s =
    do (next,s1) <- evalBlock (GlobalBlockName caller (QualifiedBlockName fid b)) s
       case next of
         Continue      -> error "Continue"
         EnterBlock b1 -> go b1 s1
         RaiseError v  -> return (Left v, s1)
         ReturnWith xs -> return (Right xs, s1)


data Result = Result
  { resReturns      :: List Value
  , resRaises       :: Value
  , resGlobals      :: GlobalState
  , resStates       :: Map GlobalBlockName State
  , resBlockRaises  :: Map GlobalBlockName Value
  } deriving Show

instance PP Result where
  pp n Result { .. } =
    vcat [ entry "returns" resReturns
         , entry "raises"  resRaises
         , entry "finals"  resGlobals
         , entry "all_states" resStates
         , entry "raise_states" resBlockRaises
         ]
    where
    entry x y
      | y == bottom = empty
      | otherwise   = x <> colon $$ nest 2 (pp n y)

{-
analyze :: Map [Int] Function ->
           FunId -> List Value -> GlobalState -> Result
analyze funs fid args glob =
  Result { resReturns     = joins [ v | Right v <- nexts ]
         , resRaises      = joins [ v | Left  v <- nexts ]
         , resGlobals     = joins gs
         , resStates      = joins states
         , resBlockRaises = joins errs
         }
  where
  (nexts,gs)         = unzip rss
  (rss, states,errs) = unzip3
                     $ runAnalysisM code (FunId [])
                     $ evalFun fid args glob

  code   = Map.fromList [ (FunId x, y) | (x,y) <- Map.toList funs ]
-}

analyze :: Map FunId Function ->
           FunId -> List Value -> GlobalState -> Result
analyze funs fid args glob =
  Result { resReturns     = joins [ v | Right v <- nexts ]
         , resRaises      = joins [ v | Left  v <- nexts ]
         , resGlobals     = joins gs
         , resStates      = states
         , resBlockRaises = errs
         }
  where
  (nexts,gs)          = unzip rss
  (rss, states, errs) = singlePath allocFuns funs
                      $ evalFun initialCaller fid args setupGlob

  gbn0 = GlobalBlockName initialCaller (QualifiedBlockName fid EntryBlock)

  -- XXX: we hack in an upvalue with an empty table,
  -- so that we can use globals, when running a chunk. 
  -- This should be more systematic.
  allocFuns = Map.singleton fid [upVal]
  upVal     = RefId gbn0 0

  setupGlob =
    let tid   = TableId gbn0 0
        empty = TableV { tableFields = fConst (basic Nil)
                       , tableKeys   = bottom
                       , tableValues = bottom
                       }


    in glob { tables = Map.insert tid empty (tables glob)
            , heap   = Map.insert upVal (fromSingleV (TableValue (Just tid)))
                                        (heap glob)
            }





