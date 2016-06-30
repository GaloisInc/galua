{-# LANGUAGE NamedFieldPuns, RecordWildCards, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module Galua.Micro.Type.Monad
  ( -- * Analysis
    AnalysisM, GlobalBlockName(..)
  , allPaths, singlePath

    -- ** Backtracking
  , impossible, options

    -- ** Global state
  , getGlobal, setGlobal


    -- * Single function

    -- ** Blocks
  , BlockM, inBlock

    -- ** Functions
  , newFunId, setCurFun

  , PrimImpl(..)
  , getPrim


    -- * Single block
  , curStmt, continue

    -- ** References
  , newRefId, readRefId, writeRefId

    -- ** Tables
  , newTableId, getTable, setTable, getTableMeta

    -- ** Locals
  , assign, getReg, getUpVal
  , getList, setList

  , -- ** Continuations
    Cont, getCont, setCont, CallsiteId, initialCaller

  , -- ** Errors
    raisesError

  , -- * Top values
    anyTableId, anyRefId, anyFunId




  ) where

import           Data.Map ( Map )
import qualified Data.Map as Map
import           Data.Vector(Vector)
import qualified Data.Vector as Vector
import           Data.Maybe(fromMaybe)

import qualified MonadLib as M
import           MonadLib hiding (raises,sets,sets_,ask,get)

import qualified Language.Lua.Bytecode as OP
import           Language.Lua.Bytecode.FunId

import Galua.Micro.AST
import Galua.Micro.Type.Value


class Monad m => AnalysisM m where
  ask     :: m RO
  sets    :: (AnalysisS -> (a,AnalysisS)) -> m a
  options :: [a] -> m a

newtype AllPaths a   = AllPaths (ReaderT RO (StateT AnalysisS []) a)
                        deriving (Functor,Applicative,Monad)

newtype SinglePath a = SinglePath (ReaderT RO (ChoiceT (StateT AnalysisS Id)) a)
                        deriving (Functor,Applicative,Monad)



instance AnalysisM AllPaths where
  ask     = AllPaths M.ask
  sets    = AllPaths . M.sets
  options = AllPaths . lift . lift

instance AnalysisM SinglePath where
  ask     = SinglePath M.ask
  sets    = SinglePath . M.sets
  options = SinglePath . lift . msum . map return

get :: AnalysisM m => m AnalysisS
get = sets $ \s -> (s,s)

sets_ :: AnalysisM m => (AnalysisS -> AnalysisS) -> m ()
sets_ f = sets $ \s -> ((),f s)


data RO = RO
  { roCode     :: Map FunId Function
  , roPrims    :: Map CFun PrimImpl
  }

newtype PrimImpl = PrimImpl (forall m. AnalysisM m => List Value -> m (List Value))

data AnalysisS = AnalysisS
  -- rwFunctions :: !(Map FunId (Map UpIx RefId))
    -- Up-values for allocated functions.

  { rwStates    :: !(Map GlobalBlockName State)
    -- ^ Known states at the entries of blocks.

  , rwRaises    :: !(Map GlobalBlockName Value)

  }


--------------------------------------------------------------------------------

impossible :: AnalysisM m => m a
impossible = options []

allPaths ::
  Map FunId Function ->
  Map CFun PrimImpl ->
  AllPaths a ->
  [ (a, Map GlobalBlockName State , Map GlobalBlockName Value) ]
allPaths funs prims (AllPaths m) =
  [ (a, rwStates rw, rwRaises rw)
    | (a,rw) <- runStateT initS $ runReaderT ro m ]
  where
  initS = AnalysisS { rwStates    = Map.empty
                    , rwRaises    = Map.empty
                    }

  ro    = RO { roCode = funs, roPrims = prims }


singlePath ::
  Map FunId Function ->
  Map CFun PrimImpl ->
  SinglePath a ->
  ([a], Map GlobalBlockName State, Map GlobalBlockName Value)
singlePath funs prims (SinglePath m) = (as, rwStates rw, rwRaises rw)
  where
  (as,rw) = runId $ runStateT initS $ findAll $ runReaderT ro m

  initS = AnalysisS { rwStates    = Map.empty
                    , rwRaises    = Map.empty
                    }

  ro = RO { roCode = funs, roPrims = prims }






{-
anyAllocatedFunId :: AnalysisM m => m FunId
anyAllocatedFunId =
  do AnalysisS { rwFunctions } <- get
     options (Map.keys rwFunctions)

anyFunId :: AnalysisM m => m FunId
anyFunId =
  do RO { roCode } <- ask
     options (Map.keys roCode)
-}



--------------------------------------------------------------------------------

tryEnterBlockAt :: AnalysisM m => GlobalBlockName -> State -> m BlockS
tryEnterBlockAt loc sCur =
  do AnalysisS { rwStates = states } <- get
     let GlobalBlockName caller (QualifiedBlockName curFun b) = loc
         sOld = Map.findWithDefault bottom loc states

     case addNewInfo sCur sOld of
       Just sNew ->
         do sets_ $ \AnalysisS { .. } ->
                     AnalysisS { rwStates = Map.insert loc sNew rwStates, .. }

            RO { roCode } <- ask
            let ss = case Map.lookup curFun roCode of
                       Just f ->
                         case Map.lookup b (functionCode f) of
                           Just b1 -> b1
                           Nothing -> error $ "The block is missing: " ++
                                                            show b ++
                                              "\nI'm in function: " ++
                                                        funIdString curFun
                       Nothing ->
                         error $ "The current functions is missing: " ++
                                                            show curFun
            return BlockS { rwCurBlock   = b
                          , rwCurOpCode  = 0
                          , rwStatements = ss
                          , rwCurState   = sNew
                          , rwCurFunId   = curFun
                          , rwCurCallsite = caller
                          }

       _ -> impossible



runBlockM :: AnalysisM m => BlockS -> BlockM a -> m (a, State)
runBlockM s (BlockM m) = do (a,rw) <- runStateT s m
                            return (a, rwCurState rw)


inBlock :: AnalysisM m => GlobalBlockName -> State -> BlockM a -> m (a, State)
inBlock b s m =
  do s1 <- tryEnterBlockAt b s
     runBlockM s1 m









--------------------------------------------------------------------------------
newtype BlockM a  = BlockM (forall m. AnalysisM m => StateT BlockS m a)

instance Functor BlockM where
  fmap = liftM

instance Applicative BlockM where
  pure a = BlockM (return a)
  (<*>)  = ap

instance Monad BlockM where
  BlockM m >>= f = BlockM (do a <- m
                              let BlockM m1 = f a
                              m1)

instance AnalysisM BlockM where
  ask         = BlockM (lift ask)
  sets f      = BlockM (lift (sets f))
  options os  = BlockM (lift (options os))

getPrim :: AnalysisM m => CFun -> m (Maybe PrimImpl)
getPrim ptr =
  do RO { roPrims } <- ask
     return (Map.lookup ptr roPrims)


data BlockS = BlockS
  { rwCurBlock    :: !BlockName
  , rwCurOpCode   :: !Int
  , rwStatements  :: !(Vector Stmt)
  , rwCurState    :: !State
  , rwCurFunId    :: !FunId     -- ^ Current function that is executed.
  , rwCurCallsite :: !CallsiteId -- ^ Where we were called from.
  }

--------------------------------------------------------------------------------
-- Errors

raisesError :: Value -> BlockM ()
raisesError v =
  do BlockS { rwCurBlock } <- BlockM M.get
     blockRaisesError rwCurBlock v

blockRaisesError :: BlockName -> Value -> BlockM ()
blockRaisesError b v =
  do gb <- getCurGlobalBlockName
     sets_ $ \a -> a { rwRaises = Map.insertWith (\/) gb v (rwRaises a) }


--------------------------------------------------------------------------------
-- Continuations, for functions calls

data Cont = Cont { plFun    :: FunId
                 , plBlock  :: BlockName
                 , plPC     :: Int
                 , plStmts  :: Vector Stmt
                 , plLocals :: LocalState
                 , plCallsite :: CallsiteId
                 }

getCont :: BlockM (CallsiteId, Cont)
getCont = do BlockS { .. } <- BlockM M.get

             let callsiteId = CallsiteId (QualifiedBlockName rwCurFunId rwCurBlock) rwCurOpCode
                 cont = Cont
                         { plFun    = rwCurFunId
                         , plCallsite = rwCurCallsite
                         , plBlock  = rwCurBlock
                         , plPC     = rwCurOpCode
                         , plStmts  = rwStatements
                         , plLocals = localState rwCurState
                         }

             return (callsiteId, cont)

setCont :: Cont -> BlockM ()
setCont Cont { .. } =
  BlockM $
    M.sets_ $ \s -> s { rwCurBlock   = plBlock
                      , rwCurCallsite = plCallsite
                      , rwCurOpCode  = plPC
                      , rwStatements = plStmts
                      , rwCurFunId   = plFun
                      , rwCurState   = (rwCurState s) { localState = plLocals }
                      }




--------------------------------------------------------------------------------

curStmt :: BlockM Stmt
curStmt = BlockM $ do BlockS { rwStatements, rwCurOpCode } <- M.get
                      case rwStatements Vector.!? rwCurOpCode of
                        Just s  -> return s
                        Nothing -> error "Fell-off the end of a block."

continue :: BlockM ()
continue = BlockM $ M.sets_ $ \s -> s { rwCurOpCode = 1 + rwCurOpCode s }







--------------------------------------------------------------------------------
-- Manipulating the state

getState :: BlockM State
getState = BlockM $ do BlockS { rwCurState } <- M.get
                       return rwCurState

setState :: State -> BlockM ()
setState s = BlockM $ M.sets_ $ \b -> b { rwCurState = s }

updState :: (State -> State) -> BlockM ()
updState f = do s <- getState
                setState (f s)

getGlobal :: BlockM GlobalState
getGlobal = globalState <$> getState

setGlobal :: GlobalState -> BlockM ()
setGlobal g = updState $ \State { .. } -> State { globalState = g, .. }

updGlobal :: (GlobalState -> GlobalState) -> BlockM ()
updGlobal f = do g <- getGlobal
                 setGlobal (f g)

getLocal :: BlockM LocalState
getLocal = localState <$> getState

setLocal :: LocalState -> BlockM ()
setLocal l = updState $ \State { .. } -> State { localState = l, .. }

updLocal :: (LocalState -> LocalState) -> BlockM ()
updLocal f = do l <- getLocal
                setLocal (f l)



--------------------------------------------------------------------------------
-- Working with local values.

assign :: Reg -> RegVal -> BlockM ()
assign r v = updLocal $ \LocalState { env, .. } ->
                         LocalState { env = Map.insert r v env, .. }

-- Does not return bottom
getReg :: Reg -> BlockM RegVal
getReg r = do LocalState { env } <- getLocal
              case Map.lookup r env of
                Nothing -> impossible
                Just v  -> return v





getList :: ListReg -> BlockM (List Value)
getList r =
  do LocalState { argReg, listReg } <- getLocal
     return $! case r of
                 ListReg -> listReg
                 ArgReg  -> argReg

setList :: ListReg -> List Value -> BlockM ()
setList r vs = updLocal $ \s ->
                 case r of
                   ListReg -> s { listReg = vs }
                   ArgReg  -> s { argReg  = vs }

-- Does not return bottom
getUpVal :: UpIx -> BlockM RegVal
getUpVal u = do LocalState { upvals } <- getLocal
                return $! case Map.lookup u upvals of
                            Just r  -> toVal r
                            Nothing -> RegBottom
  where
  toVal r = case r of
              NoValue        -> RegBottom
              MultipleValues -> RegTop
              OneValue v     -> RegRef v




--------------------------------------------------------------------------------
-- Tables


anyTableId :: BlockM TableId
anyTableId =
  do GlobalState { tables } <- getGlobal
     options $ Map.keys tables


newTableId :: BlockM TableId
newTableId =
  do BlockS { rwCurOpCode } <- BlockM M.get
     gb <- getCurGlobalBlockName
     let tid = TableId gb rwCurOpCode
     writeTableId tid emptyTable
     return tid
  where
  emptyTable = TableV { tableFields = fConst (basic Nil)
                      , tableKeys   = bottom
                      , tableValues = bottom
                      }



readTableId :: TableId -> BlockM TableV
readTableId l = do GlobalState { tables } <- getGlobal
                   case Map.lookup l tables of
                     Just v | v /= bottom  -> return v
                     _                     -> impossible

writeTableId :: TableId -> TableV -> BlockM ()
writeTableId t v = updGlobal $ \GlobalState { tables, .. } ->
                                GlobalState { tables = Map.insert t v tables
                                            , .. }

{- NOTE: Modifying "Top" values.
  "Top" is used when we are doing a partial analysis, and we don't know
  anything about a value. So, when we modify such a value, we could
  be modifying any of the currently known values of that type,
  *OR* some other external value that we did not know about.
  This is why we add the options that *nothing* gets modified.
-}


-- See: Modifying "Top" values
setTable :: Maybe TableId -> SingleV -> Value -> BlockM ()
setTable mb vi v =
  case mb of
    Just l  -> doSetTable l
    Nothing -> join $ options [ doSetTable =<< anyTableId, return () ]
  where
  doSetTable l =
    do TableV { .. } <- readTableId l
       writeTableId l $
         case vi of

           StringValue mb ->
             case mb of
               Nothing -> TableV { tableFields = letFunAll v tableFields
                                 , .. }
               Just ss -> TableV { tableFields = letFun (Field ss) v tableFields
                                 , .. }

           BasicValue Nil -> error "doSetTable: Nil key"

           _ -> TableV { tableKeys   = fromSingleV vi \/ tableKeys
                       , tableValues = v              \/ tableValues
                       , .. }


getTable :: Maybe TableId -> SingleV -> BlockM Value
getTable Nothing _ = return topVal
getTable (Just l) ti =
  do TableV { .. } <- readTableId l
     return $ case ti of
                StringValue (Just xx) -> appFun tableFields (Field xx)
                StringValue Nothing   -> appAll tableFields
                _ | ti `elem` valueCases tableKeys -> basic Nil \/ tableValues
                  | otherwise -> basic Nil


getTableMeta :: Maybe TableId -> BlockM Value
getTableMeta Nothing = return (basic Nil \/ fromSingleV (TableValue Nothing))
getTableMeta (Just l) =
  do TableV { .. } <- readTableId l
     return (appFun tableFields Metatable)




--------------------------------------------------------------------------------
-- References

anyRefId :: BlockM RefId
anyRefId =
  do GlobalState { heap } <- getGlobal
     options $ Map.keys heap


newRefId :: Value -> BlockM RefId
newRefId v =
  do BlockS { rwCurOpCode } <- BlockM M.get
     gb <- getCurGlobalBlockName
     let ref = RefId gb rwCurOpCode
     writeRefId (Just ref) v
     return ref

readRefId :: Maybe RefId -> BlockM Value
readRefId Nothing = return topVal
readRefId (Just l) = do GlobalState { heap } <- getGlobal
                        case Map.lookup l heap of
                          Just v | v /= bottom -> return v
                          _                    -> impossible

-- See: Modifying "Top" values
writeRefId :: Maybe RefId -> Value -> BlockM ()
writeRefId mb v =
  case mb of
    Just r  -> upd r
    Nothing -> join $ options [ upd =<< anyRefId, return () ]
  where
  upd r = updGlobal $ \GlobalState { heap, .. } ->
                       GlobalState { heap = Map.insert r v heap, .. }



--------------------------------------------------------------------------------
-- Closures


-- | Allocate a new function, given a prototype, and some references.
newFunId :: Int -> [RefId] -> BlockM ClosureId
newFunId proto refs =
  do BlockS { rwCurOpCode } <- BlockM M.get
     gb                     <- getCurGlobalBlockName

     let ref = ClosureId gb rwCurOpCode

     curFun <- getCurFun
     let clo = FunV { functionUpVals = Map.fromList (zipWith up [ 0 .. ] refs)
                    , functionFID    = OneValue (LuaFunImpl (subFun curFun proto))
                    }

     updGlobal $ \GlobalState { functions, .. } ->
                  GlobalState { functions = Map.insert ref clo functions, .. }

     return ref

  where
  up n r = (OP.UpIx n, OneValue r)


anyFunId :: BlockM ClosureId
anyFunId =
  do GlobalState { functions } <- getGlobal
     options $ Map.keys functions




--------------------------------------------------------------------------------
-- Current function


getCurFun :: BlockM FunId
getCurFun = do BlockS { rwCurFunId } <- BlockM M.get
               return rwCurFunId

setCurFun :: FunId -> BlockM ()
setCurFun fid = BlockM $ M.sets_ $ \s -> s { rwCurFunId = fid }

getCurGlobalBlockName :: BlockM GlobalBlockName
getCurGlobalBlockName =
  do BlockS{..} <- BlockM M.get
     return (GlobalBlockName rwCurCallsite (QualifiedBlockName rwCurFunId rwCurBlock))
