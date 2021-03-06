{-# LANGUAGE NamedFieldPuns, RecordWildCards, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
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

  , getLocal

  , -- ** Continuations
    Cont, getCont, setCont, CallsiteId, initialCaller

  , -- ** Errors
    raisesError

  , -- * Top values
    anyTableId, anyRefId, anyFunId


  , -- * Logging
    logTrace, logError


  ) where

import           Data.Map ( Map )
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vector

import qualified MonadLib as M
import           MonadLib hiding (raises,sets,sets_,ask,get)


import Galua.Micro.AST
import Galua.Micro.Type.Value
import qualified Galua.Code as Code


class Monad m => AnalysisM m where
  ask     :: m RO
  sets    :: (AnalysisS -> (a,AnalysisS)) -> m a
  options :: [a] -> m a

-- | Consider all paths through the code.
-- More preciese, but also more expensive.
newtype AllPaths a   = AllPaths (ReaderT RO (StateT AnalysisS []) a)
                        deriving (Functor,Applicative,Monad)

-- | Consier only one path through the code, joining alternatives.
-- Less precise, but a bit cheaper.
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


-- | Infromation about code in the environemnt.
data RO = RO
  { roCode     :: Map FunId MicroFunction
  , roPrims    :: Map CFun PrimImpl
  }

-- | A primitive: given the state and some arguments, return an update
-- state, and either an exception or a list of restuls.
newtype PrimImpl = PrimImpl
  (forall m. AnalysisM m => GlobalState -> List Value ->
                                    m (Either Value (List Value), GlobalState))

-- | The state of the analysis.
data AnalysisS = AnalysisS
  { rwStates    :: !(Map GlobalBlockName State)
    -- ^ Known states at the entries of blocks.

  , rwRaises    :: !(Map GlobalBlockName Value)

  , logLines    :: [String]

  }


--------------------------------------------------------------------------------

impossible :: AnalysisM m => m a
impossible = options []

allPaths ::
  Map FunId MicroFunction ->
  Map CFun PrimImpl ->
  AllPaths a ->
  [ (a, Map GlobalBlockName State , Map GlobalBlockName Value, [String]) ]
allPaths funs prims (AllPaths m) =
  [ (a, rwStates rw, rwRaises rw, reverse (logLines rw))
    | (a,!rw) <- runStateT initS $ runReaderT ro m ]
  where
  initS = AnalysisS { rwStates    = Map.empty
                    , rwRaises    = Map.empty
                    , logLines    = []
                    }

  ro    = RO { roCode = funs, roPrims = prims }


singlePath ::
  Map FunId MicroFunction ->
  Map CFun PrimImpl ->
  SinglePath a ->
  ([a], Map GlobalBlockName State, Map GlobalBlockName Value, [String])
singlePath funs prims (SinglePath m) = (as, rwStates rw, rwRaises rw, reverse(logLines rw))
  where
  (as,!rw) = runId $ runStateT initS $ findAll $ runReaderT ro m

  initS = AnalysisS { rwStates    = Map.empty
                    , rwRaises    = Map.empty
                    , logLines    = []
                    }

  ro = RO { roCode = funs, roPrims = prims }



--------------------------------------------------------------------------------

{- | Enter a block with with a particular state.
If we've already done the analysis for this state (i.e., adding it
to the old state does not produce any new information), then we don't
do anything. -}
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
                                                        Code.funIdString curFun
                       Nothing ->
                         error $ "The current function is missing: " ++
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
  , rwStatements  :: !Block
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
blockRaisesError _ v =
  do gb <- getCurGlobalBlockName
     sets_ $ \a -> a { rwRaises = Map.insertWith (\/) gb v (rwRaises a) }


--------------------------------------------------------------------------------
-- Continuations, for functions calls

data Cont = Cont { plFun    :: FunId
                 , plBlock  :: BlockName
                 , plPC     :: Int
                 , plStmts  :: Block
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

curStmt :: BlockM (Either (BlockStmt Stmt) (BlockStmt EndStmt))
curStmt = BlockM $ do BlockS { rwStatements, rwCurOpCode } <- M.get
                      case blockBody rwStatements Vector.!? rwCurOpCode of
                        Just s  -> return (Left s)
                        Nothing -> return (Right (blockEnd rwStatements))


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
                let r = Map.findWithDefault Top u upvals
                {- NOTE: generally, upvalues should not be missing, however,
                if we are analyzing some source code, and we have no info
                about the up-values, it is simpler to pass an empty map,
                rather than a map full of Top. -}
                return (RegRef r)


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
                      , tableValues = basic Nil
                      , tableMeta   = basic Nil
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
  This is why we add the `return ()` csae, indicating that
  *nothing* gets modified.
-}


-- See: Modifying "Top" values
setTable :: WithTop TableId -> Value -> Value -> BlockM ()
setTable mb vi v =
  case mb of
    NotTop l  -> doSetTable l
    Top       -> join $ options [ doSetTable =<< anyTableId, return () ]
  where
  doSetTable l =
    do t <- readTableId l
       writeTableId l (setTableEntry (vi,v) t)

getTable :: WithTop TableId -> SingleV -> BlockM Value
getTable Top _ = return topVal
getTable (NotTop l) ti =
  do TableV { .. } <- readTableId l
     return $ case ti of
                StringValue (NotTop xx) -> appFun tableFields xx
                StringValue Top         -> appAll tableFields
                _ | ti `elem` valueCases tableKeys -> tableValues
                  | otherwise -> basic Nil


getTableMeta :: WithTop TableId -> BlockM Value
getTableMeta Top = return (basic Nil \/ fromSingleV (TableValue Top))
getTableMeta (NotTop l) =
  do TableV { .. } <- readTableId l
     return tableMeta




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
  up n r = (Code.UpIx n, NotTop (Set.singleton r))


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

------------------------------------------------------------------------
-- Logging

logError :: String -> BlockM ()
logError = logTrace -- TODO: severities

logTrace :: String -> BlockM ()
logTrace msg = BlockM $ lift $ sets_ $ \s ->
  let msgs = logLines s
  in msgs `seq` s { logLines = msg : msgs }
