{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards, BangPatterns, OverloadedStrings #-}
module Galua.Debugger
  ( Debugger(..)
  , IdleReason(..)
  , Chunks(..)
  , ChunkInfo(..)
  , Source(..)
  , newEmptyDebugger
  , stepInto
  , stepOver
  , stepIntoLine
  , stepOverLine
  , stepOutOf
  , whenStable
  , run
  , runNonBlock
  , pause
  , goto
  , executeStatement
  , addBreakPoint
  , removeBreakPoint
  , clearBreakPoints
  , poll

  , ExportableState(..)
  , newExportableState
  , Exportable(..)
  , ValuePath(..)
  , showValuePath
  , getValue
  , setPathValue

  , FunVisName(..)
  , BreakCondition(..)

  , ExecEnvId(..)
  , exportExecEnvId
  , importExecEnvId
  , resolveName

  -- * WatchList
  , WatchList, watchListEmpty, watchListRemove, watchListExtend
  , watchListToList

  -- * Globals
  , GlobalTypeEntry(..)
  ) where

import           Galua(setupLuaState)
import           Galua.CallIntoC (handleCCallState)
import           Galua.Debugger.PrettySource
                  (lexChunk,Line,NameId,LocatedExprName)
import           Galua.Debugger.Options
import           Galua.Debugger.NameHarness
import           Galua.Debugger.Console(recordConsoleInput,recordConsoleValues)

import           Galua.Mach
import           Galua.Stepper
import           Galua.Reference
import           Galua.Value
import           Galua.FunValue
import           Galua.Names.Eval
import           Galua.Names.Find(LocatedExprName(..),ppExprName)
import qualified Galua.Util.SizedVector as SV

import qualified Galua.Spec.AST as Spec
import qualified Galua.Spec.Parser as Spec

import           Language.Lua.Bytecode(Function(..))
import           Language.Lua.Bytecode.Debug
                    (lookupLineNumber,inferSubFunctionNames,deepLineNumberMap)
import           Language.Lua.Bytecode.FunId

import           Data.Maybe (catMaybes,mapMaybe,fromMaybe,maybeToList)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.Set ( Set )
import qualified Data.Set as Set
import           Data.Word(Word64)
import           Data.Text(Text)
import qualified Data.Text as Text
import           Data.Text.Read(decimal)
import           Data.Text.Encoding(decodeUtf8,encodeUtf8)

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B

import           Foreign (Ptr)
import           Data.Ord(comparing)
import           Data.List(unfoldr,minimumBy)
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as IOVector
import           Data.IORef(IORef,readIORef,writeIORef,
                            modifyIORef,modifyIORef',newIORef,
                            atomicModifyIORef')

import            Control.Concurrent
                    ( MVar, newEmptyMVar, putMVar, takeMVar, forkIO )
import           Control.Applicative ((<|>))
import           Control.Concurrent (killThread, ThreadId)
import           Control.Concurrent.STM (STM, atomically, takeTMVar)
import           Control.Concurrent.STM.TQueue (TQueue, newTQueue, writeTQueue, readTQueue, isEmptyTQueue)
import           Control.Monad.IO.Class(liftIO)
import           Control.Monad(join, when,forever)
import           Control.Exception
import           MonadLib(ExceptionT,runExceptionT,raise,lift)
import           System.Timeout(timeout)


{- The fields in this are mutable, so that the interpreter can modify them
easily, without having to pass the state aroud.  For example, when we
load a new module, the interpreter uses an IO action, which modifies 
dbgSource. -}
data Debugger = Debugger
  { dbgSources   :: !(IORef Chunks)
    {- ^ Source code for chunks and corresponding parsed functions.
          The functions are also in the VM state, but that's only available
          while the machine is running. -}


  , dbgCommand   :: !(TQueue (DebuggerCommand,Bool))
    {- ^ Commands from the outside world, telling the debugger what to do next.
         The boolean indicates if this command modifis the state, and as such
         should modify `dbgCommandCounter` -}

  , dbgCommandCounter :: !(IORef Word64)
    {- ^ Every time we do a command, we should increment this counter.
    This is useful to implement polling from the outside world,
    where the client can know if there have been any commands executed
    since last time they looked. -}



  , dbgClients   :: !(IORef [MVar ()]) --
    -- ^ Notify these when we stop


  , dbgIdleReason :: !(IORef IdleReason)
    -- ^ Why are we not running

  , dbgStateVM   :: !(IORef VMState)                -- ^ The interpreter state

  , dbgBreaks    :: !(IORef (Map (Int,FunId) (Maybe BreakCondition)))
    -- ^ Static break points.  The key of the map is the break point,
    -- the value is an optional condition.  The breakpoint will only
    -- be active if the condition is true.

  , dbgBreakOnError :: !(IORef Bool)
    -- ^ Should we stop automatically, when we encounter an error.

  , dbgBrkAddOnLoad :: !(IORef CommandLineBreakPoints)
    {- ^ When a chunk is loaded, we added a break point at the beginning
         of each of the functions in the corresponding entry in the map.
         The break points at key 'Nothing' are added to the first chunk
         tath is loaded. -}

  , dbgWatches   :: !(IORef WatchList)

  , dbgExportable :: !(IORef ExportableState)
    -- ^ Things that may be expanded further.

    -- ^ Types
  , dbgDeclaredTypes :: !(IORef GlobalTypeMap)
  }

type SpecType = Spec.ValDecl Spec.Parsed
type SpecDecl = Spec.Decl Spec.Parsed


data IdleReason = Ready
                | ReachedBreakPoint
                | ThrowingError Value
                | Executing

data BreakCondition = BreakCondition
  { brkCond :: CompiledStatment
    -- ^ Use this to evaluate the condition
  , brkText :: Text
    -- ^ Use this to display the condition
  }


--------------------------------------------------------------------------------
-- The watch list

data WatchList = WatchList
  { wlNextId :: !Int
  , wlItems  :: !(IntMap ValuePath)
  }

watchListEmpty :: WatchList
watchListEmpty = WatchList { wlNextId = 0, wlItems = IntMap.empty }

watchListToList :: WatchList -> [(Int,ValuePath)]
watchListToList WatchList { .. } = IntMap.toList wlItems

watchListExtend :: ValuePath -> WatchList -> (Int,WatchList)
watchListExtend vp WatchList { .. } =
  (wlNextId, WatchList { wlNextId = 1 + wlNextId
                       , wlItems  = IntMap.insert wlNextId vp wlItems
                       })

watchListRemove :: Int -> WatchList -> WatchList
watchListRemove n WatchList { .. } =
  WatchList { wlItems = IntMap.delete n wlItems, .. }



--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Keeping track of expand/collapsed state of references

data ExportableState = ExportableState
  { expNextThing    :: !Integer
  , expClosed       :: Map Integer Exportable
  , openThreads     :: !(Set Int)
    -- ^ Reference ids of the threads that have been expended.
    -- We keep an int instead of the actual reference, so that the reference
    -- can be garbage collected if it finished.
  }

newExportableState :: ExportableState
newExportableState = ExportableState { expNextThing = 0
                                     , expClosed = Map.empty
                                     , openThreads = Set.empty
                                     }

data Exportable = ExportableValue ValuePath Value
                    -- ^ a collapsed value that may be

                | ExportableStackFrame Int ExecEnv
                  -- ^ A collapse stack frame for a function call

data ValuePath
  = VP_Field ValuePath Value -- ^ path to value indexed by given key
  | VP_Key   ValuePath Value -- ^ path to actual key
  | VP_CUpvalue ValuePath Int -- ^ path to upvalues of a closure
  | VP_MetaTable ValuePath
  | VP_Register ExecEnv Int
  | VP_Upvalue ExecEnv Int
  | VP_Varargs ExecEnv Int
  | VP_Registry Value
  | VP_None



showValuePath :: ValuePath -> String
showValuePath = flip go ""
  where
  go (VP_Key p v) = go p . showChar '{' . showString (prettyValue v) . showChar '}'
  go (VP_Field p v) = go p . showChar '[' . showString (prettyValue v) . showChar ']'
  go (VP_MetaTable p) = go p . showString ".MT"
  go (VP_Register _env n)= showString ("R["++show (n+1)++"]")
  go (VP_CUpvalue p n) = go p . showString (".U["++show (n+1)++"]")
  go (VP_Upvalue _env n) = showString ("U["++show (n+1)++"]")
  go (VP_Varargs _env n) = showString ("{...}["++show (n+1)++"]")
  go VP_Registry{}       = showString "REGISTRY"
  go VP_None    = showString "none"


-- | Get the value referenced by the given path, if any.
getValue :: ValuePath -> IO (Maybe Value)
getValue vp0 = do res <- runExceptionT (getVal vp0)
                  case res of
                    Left _  -> return Nothing
                    Right a -> return (Just a)
  where
  getVal :: ValuePath -> ExceptionT () IO Value
  getVal vp =
    case vp of

      VP_Field vp' key ->
        do val <- getVal vp'
           case val of
             Table ref -> lift (getTableRaw ref key)
             _         -> raise ()

      VP_Key vp' key ->
        do val <- getVal vp'
           case val of
             Table _  -> return key  -- XXX: Should we check that key present?
             _        -> raise ()

      VP_CUpvalue vp' n ->
        do val <- getVal vp'
           case val of
             Closure ref ->
                do let ups = cloUpvalues (referenceVal ref)
                   if 0 <= n && n < IOVector.length ups
                     then lift (readIORef =<< IOVector.read ups n)
                     else raise ()
             _ -> raise ()

      VP_MetaTable vp' ->
        do val <- getVal vp'
           case val of
             Table r -> do mb <- lift $ getTableMeta r
                           case mb of
                             Just tr -> return (Table tr)
                             Nothing -> raise ()
             UserData r ->
               do mb <- lift (readIORef (userDataMeta (referenceVal r)))
                  case mb of
                    Just tr -> return (Table tr)
                    Nothing -> raise ()
             _ -> raise () -- XXX: Does anything else have a metatable?

      VP_Register ExecEnv { execStack } n ->
        do mb <- lift (SV.getMaybe execStack n)
           case mb of
             Just r  -> lift (readIORef r)
             Nothing -> raise ()

      VP_Upvalue ExecEnv { execUpvals } n ->
        if 0 <= n && n < IOVector.length execUpvals
          then lift (readIORef =<< IOVector.read execUpvals n)
          else raise ()

      VP_Varargs ExecEnv { execVarargs } n ->
        do varargs <- lift (readIORef execVarargs)
           case drop n varargs of
             r:_ -> return r
             []  -> raise ()

      VP_Registry registry -> return registry

      VP_None -> raise ()




setValue :: ValuePath -> Value -> IO ()
setValue vp v =
  case vp of
    VP_Field vp' key   -> getValue vp' `andThen` setField key
    VP_Key   vp' key   -> getValue vp' `andThen` setKey   key
    VP_CUpvalue vp' n  -> getValue vp' `andThen` setCUpval n
    VP_MetaTable vp'   -> getValue vp' `andThen` setMeta
    VP_Register env n  -> setReg n env
    VP_Upvalue  env n  -> setUVal n env
    VP_Varargs env n   -> setVArg n env
                                    -- or put the var-args in a reference.
    VP_Registry{}      -> return () -- not currently supported
    VP_None            -> return ()


  where
  andThen :: IO (Maybe a) -> (a -> IO ()) -> IO ()
  andThen thing k =
    do res <- thing
       case res of
         Nothing -> return ()
         Just a  -> k a

  setField key val =
    case val of
      Table ref -> setTableRaw ref key v
      _         -> return ()

  -- Note that this if the new key clashes with another key,
  -- the value at the other key will be lost
  setKey key val =
    case val of
      Table ref -> do f <- getTableRaw ref key
                      setTableRaw ref key Nil
                      setTableRaw ref v f
      _ -> return ()

  setCUpval n val =
    case val of
      Closure ref ->
        do let ups = cloUpvalues (referenceVal ref)
           when (0 <= n && n < IOVector.length ups) $
              do r <- IOVector.read ups n
                 writeIORef r v

      _ -> return ()

  setMeta val =
    case v of
      Nil     -> doSetMeta val Nothing
      Table r -> doSetMeta val (Just r)
      _       -> return ()

  doSetMeta val mb1 =
    case val of
      Table ref    -> setTableMeta ref mb1
      UserData ref -> writeIORef (userDataMeta (referenceVal ref)) mb1
      _            -> return ()

  setReg n ExecEnv { execStack } =
    do mb <- SV.getMaybe execStack n
       case mb of
         Just r  -> writeIORef r v
         Nothing -> return ()

  setUVal n ExecEnv { execUpvals } =
    when (0 <= n && n < IOVector.length execUpvals) $
      do r <- IOVector.read execUpvals n
         writeIORef r v

  setVArg n ExecEnv { execVarargs } =
    do modifyIORef' execVarargs $ \varargs ->
         case splitAt n varargs of
           (a,_:b) -> a++v:b
           _       -> varargs


--------------------------------------------------------------------------------

-- | Identifies an execution environment>
-- This is used when we resolve name references.

data ExecEnvId = StackFrameExecEnv !Integer
                 -- ^ Exportable id of a stack frame

               | ThreadExecEnv !Int
                 -- ^ Reference id of a thread object

               | ClosureEnvId !Int
                 -- ^ Reference id of a closure object

                 deriving Show

exportExecEnvId :: ExecEnvId -> Text
exportExecEnvId eid =
  Text.pack $
  case eid of
    StackFrameExecEnv n -> "s_" ++ show n
    ThreadExecEnv n     -> "t_" ++ show n
    ClosureEnvId n      -> "c_" ++ show n

importExecEnvId :: Text -> Maybe ExecEnvId
importExecEnvId txt =
  case Text.splitAt 2 txt of
    ("s_", s) | Just n <- num s -> Just (StackFrameExecEnv n)
    ("t_", s) | Just n <- num s -> Just (ThreadExecEnv n)
    ("c_", s) | Just n <- num s -> Just (ClosureEnvId n)
    _                           -> Nothing
  where
  num s = case decimal s of
            Right (a,"") -> Just a
            _            -> Nothing

findNameResolveEnv :: Debugger -> VM -> ExecEnvId -> IO (FunId, NameResolveEnv)
findNameResolveEnv dbg vm eid =
  do metaTabs <- readIORef $ machMetatablesRef $ vmMachineEnv vm
     case eid of
       StackFrameExecEnv sid ->
         do ExportableState { expClosed } <- readIORef (dbgExportable dbg)
            case Map.lookup sid expClosed of
              Just (ExportableStackFrame _ env) ->
                execEnvToNameResolveEnv metaTabs env
              _ -> nameResolveException ("Invalid stack frame: " ++ show sid)

       ThreadExecEnv tid ->
         do mb <- lookupRef (vmAllocRef vm) tid
            case mb of
              Nothing  -> nameResolveException "Invalid thread."
              Just ref ->
                do eenv <- getThreadField stExecEnv ref
                   execEnvToNameResolveEnv metaTabs eenv

       ClosureEnvId cid ->
         do mb <- lookupRef (vmAllocRef vm) cid
            case mb of
              Nothing  -> nameResolveException "Invalid closure."
              Just ref ->
                do let closure = referenceVal ref
                   closureToResolveEnv metaTabs closure

closureToResolveEnv ::
  TypeMetatables -> Closure -> IO (FunId, NameResolveEnv)
closureToResolveEnv metaTabs c =
  do (fid,func) <- case luaOpCodes (cloFun c) of
                     Just (fid,func) -> return (fid,func)
                     _ -> nameResolveException "Not in a Lua function."
     stack <- SV.new
     ups <- Vector.freeze (cloUpvalues c)
     let nre = NameResolveEnv
                 { nrUpvals   = ups
                 , nrStack    = stack
                 , nrFunction = func
                 , nrMetas    = metaTabs
                 }
     return (fid, nre)

execEnvToNameResolveEnv ::
  TypeMetatables -> ExecEnv -> IO (FunId, NameResolveEnv)
execEnvToNameResolveEnv metaTabs eenv =
  do (fid,func) <- case luaOpCodes (execFunction eenv) of
                     Just (fid,func) -> return (fid,func)
                     _ -> nameResolveException "Not in a Lua function."

     ups <- Vector.freeze (execUpvals eenv)
     let nre = NameResolveEnv
                 { nrUpvals   = ups
                 , nrStack    = execStack eenv
                 , nrFunction = func
                 , nrMetas    = metaTabs
                 }

     return (fid, nre)


resolveName :: Debugger -> ExecEnvId -> Maybe Int -> NameId ->
                IO (Either NotFound (String,Value,Maybe GlobalTypeEntry))
resolveName dbg eid pc nid =
  whenStable dbg False $
  whenNotFinishied dbg (Left $ NotFound "Not executing") $ \vm _ ->
  try $
  do (fid, resEnv) <- findNameResolveEnv dbg vm eid
     chunks        <- readIORef (dbgSources dbg)

     chunk <- case getRoot fid of
                Nothing   -> nameResolveException "Empty function?"
                Just cid  ->
                  case Map.lookup cid (topLevelChunks chunks) of
                    Nothing -> nameResolveException "Invalid context."
                    Just c  -> return c

     name <- case Map.lookup nid (srcNames (chunkSource chunk)) of
               Nothing -> nameResolveException "Invalid name idnentifier."
               Just e  -> return e


     let en = exprName name
     v <- exprToValue resEnv pc en
     mbG <- globalInfo resEnv pc en
     mbT <- case mbG of
              Nothing -> return Nothing
              Just (g,revSel) ->
                do ds <- readIORef (dbgDeclaredTypes dbg)
                   return (lookupGlobal ds g (reverse revSel))
     return (ppExprName en, v, mbT)



newtype GlobalTypeMap = GTM (Map ByteString GlobalTypeEntry)
data GlobalTypeEntry  = GlobalType [SpecType] | GlobalNamespace GlobalTypeMap


makeGlobalTypeMap :: [SpecDecl] -> GlobalTypeMap
makeGlobalTypeMap = mk . mapMaybe decl
  where
  nm x = encodeUtf8 (Spec.nameText x)

  decl d = case d of
             Spec.DClass {}     -> Nothing
             Spec.DType {}      -> Nothing
             Spec.DNamespace ns -> Just (namespace ns)
             Spec.DValDecl vd   -> Just (valDecl vd)

  valDecl vd = (nm (Spec.valName vd), GlobalType [vd])

  jn (GlobalType xs) (GlobalType ys) = GlobalType (xs ++ ys)
  jn (GlobalNamespace (GTM x)) (GlobalNamespace (GTM y)) =
      GlobalNamespace (GTM (Map.unionWith jn x y))
  jn x _ = x -- XXX: shouldn't happen

  mk = GTM . Map.fromListWith jn

  namespace x =
    ( nm (Spec.namespaceName x)
    , GlobalNamespace $ mk $ map valDecl (Spec.namespaceMembers x) ++
                             map namespace (Spec.namespaceNested x)
    )




lookupGlobal ::
  GlobalTypeMap -> ByteString -> [ByteString] -> Maybe GlobalTypeEntry
lookupGlobal (GTM mp) x ls =
  do ent <- Map.lookup x mp
     case ls of
       [] -> Just ent
       l : more ->
          case ent of
            GlobalType _        -> Nothing
            GlobalNamespace mp1 -> lookupGlobal mp1 l more



--------------------------------------------------------------------------------

-- | All loaded top-level functions.
data Chunks = Chunks
  { topLevelChunks :: Map Int ChunkInfo
    -- ^ The key is the chunk id.  The top-level id for chunk @k@ is @[k]@.
  , allFunNames    :: Map FunId FunVisName
  }

data FunVisName = FunVisName
                    { funVisName        :: !(Maybe Text)
                    , funVisLineStart   :: !Int
                    , funVisLineEnd     :: !Int
                    , funVisFile        :: !(Maybe Text)
                    }


data ChunkInfo = ChunkInfo
  { chunkSource   :: Source
  , chunkFunction :: Function
  , chunkLineInfo :: Map Int [(FunId,[Int])]
    -- ^ Map line numbers to the (function & pc) pair that occupies it.
  }



addTopLevel ::
  Maybe String -> ByteString -> Int -> Function -> Chunks -> Chunks
addTopLevel mbName bytes cid fun Chunks { .. } =
  Chunks { topLevelChunks = Map.insert cid newChunk topLevelChunks
         , allFunNames = foldr (uncurry Map.insert)
                               (Map.insert (rootFun cid) chunkName allFunNames)
                             $ concat
                             $ unfoldr nextFun [(rootFun cid,fun)]
         }
  where
  bytes'
    | B.isPrefixOf "\ESCLua" bytes = B.empty
    | otherwise                    = bytes

  newChunk =
    ChunkInfo { chunkSource   = lexSourceFile cid mbName bytes'
              , chunkFunction = fun
              , chunkLineInfo = fmap (\xs -> [ (subFun path cid,n) | (path,n) <- xs ])
                                       $ deepLineNumberMap fun
              }

  chunkName = FunVisName { funVisName      = Just "(top level)"
                         , funVisLineStart = 1
                         , funVisLineEnd   = length (B.lines bytes)
                         , funVisFile      = Text.pack <$> mbName
                         }


  nextFun todo =
    case todo of
      []             -> Nothing
      ((fid,f) : fs) -> Just (out,new ++ fs)
        where
        protos    = zip [ 0 .. ] (Vector.toList (funcProtos f))
        subNames  = Map.fromList (inferSubFunctionNames f)
        out       = [ (subFun fid i, funVisName (Map.lookup i subNames) sf)
                                         | (i,sf) <- protos ]
        new       = [ (subFun fid i, sf) | (i,sf) <- protos ]


  funVisName mb f = FunVisName
    { funVisName      = decodeUtf8 <$> mb
    , funVisLineStart = funcLineDefined f
    , funVisLineEnd   = funcLastLineDefined f
    , funVisFile      = fmap Text.pack mbName
    }




-- | Source code for a chunk.
data Source = Source { srcName  :: Maybe String
                     , srcLines :: Vector Line
                     , srcNames :: Map NameId LocatedExprName
                     }


-- | Syntax high-lighting for a source file.
lexSourceFile :: Int -> Maybe String -> ByteString -> Source
lexSourceFile chunkId srcName bytes = Source { srcName, srcLines, srcNames }
  where (srcLines,srcNames) = lexChunk chunkId (fromMaybe "" srcName) bytes

-- | Keep track of the source code for loaded modules.
addSourceFile :: IORef CommandLineBreakPoints ->
                 IORef (Map (Int,FunId) (Maybe BreakCondition)) ->
                 IORef Chunks ->
                 Maybe String -> ByteString -> Int -> Function -> IO ()
addSourceFile brks breakRef sources mbName bytes cid fun =
  do modifyIORef' sources (addTopLevel mbName bytes cid fun)
     todo <- atomicModifyIORef' brks $ \brkPoints ->
              let del _ _    = Nothing
                  (mb1,brk1) = Map.updateLookupWithKey del "" brkPoints
                  (mb2,brk2) =
                    case mbName of
                      Nothing -> (Nothing, brk1)
                      Just s  -> Map.updateLookupWithKey del s brk1
              in (brk2, concat (maybeToList mb1 ++ maybeToList mb2 ))
     Chunks { topLevelChunks } <- readIORef sources
     case Map.lookup cid topLevelChunks of
       Nothing -> return () -- should not happen
       Just c  ->
         do let lineInfo = chunkLineInfo c
                newBrk   = catMaybes [ findClosest lineInfo ln | ln <- todo ]
            modifyIORef' breakRef $ \old -> foldr addBrk old newBrk

  where
  addBrk b mp = Map.insertWith (\_ y -> y) b Nothing mp

  -- Line 0 is special cased to stop on the first instruction of a chunk.
  findClosest _ 0 = Just (0, FunId [cid])

  findClosest info ln =
    chooseExactLoc $
    case Map.splitLookup ln info of
      (_,Just b,_) -> b
      (_,_,bigger)  | Just (a,_) <- Map.minView bigger  -> a
      (smaller,_,_) | Just (a,_) <- Map.maxView smaller -> a
      _ -> []


  choosePC :: (FunId,[Int]) -> Maybe (Int,FunId)
  choosePC (fid,pcs)
    | null pcs  = Nothing
    | otherwise = Just (minimum pcs, fid)

  chooseExactLoc :: [ (FunId,[Int]) ] -> Maybe (Int,FunId)
  chooseExactLoc funs =
    case funs of
      []   -> Nothing
      [x]  -> choosePC x
      many -> choosePC (minimumBy (comparing (funNestDepth . fst)) many)


newEmptyDebugger :: MVar ThreadId -> Options -> IO (Ptr (), Debugger)
newEmptyDebugger threadVar opts =
  do let chunks = Chunks { topLevelChunks = Map.empty
                         , allFunNames    = Map.empty
                         }
     dbgCommand <- atomically newTQueue
     dbgCommandCounter <- newIORef 0


     dbgClients <- newIORef []
     dbgSources <- newIORef chunks
     dbgWatches <- newIORef watchListEmpty
     dbgBrkAddOnLoad <- newIORef (optBreakPoints opts)
     dbgBreaks       <- newIORef Map.empty

     let query "port" = return (Number 8000)
         query _      = return Nil

         cfg = MachConfig
                 { machOnChunkLoad = addSourceFile dbgBrkAddOnLoad
                                                   dbgBreaks
                                                   dbgSources
                 , machOnShutdown =
                     do a <- takeMVar threadVar
                        killThread a
                 , machOnQuery    = query
                 }

     (cptr, vm, next) <- setupLuaState cfg

     dbgIdleReason   <- newIORef Ready
     dbgStateVM      <- newIORef (Running vm next)

     dbgExportable   <- newIORef newExportableState
     dbgBreakOnError <- newIORef (optBreakOnError opts)

     dbgDeclaredTypes <-
        (newIORef . makeGlobalTypeMap) =<<
          (do s <- Spec.specFromFile "lua.spec" -- XXX: search for specs, etc.
              return (Spec.specDecls s)
            `catch` \SomeException {} -> return [])

     let dbg = Debugger { dbgSources, dbgIdleReason
                        , dbgBreaks, dbgWatches
                        , dbgCommand, dbgCommandCounter, dbgClients
                        , dbgExportable, dbgStateVM
                        , dbgBreakOnError, dbgBrkAddOnLoad
                        , dbgDeclaredTypes
                        }

     _ <- forkIO (runDebugger dbg)

     return (cptr, dbg)



setIdleReason :: Debugger -> IdleReason -> IO ()
setIdleReason Debugger { .. } x = writeIORef dbgIdleReason x


--------------------------------------------------------------------------------
-- Entry points


setPathValue :: Debugger -> Integer -> Value -> IO (Maybe ValuePath)
setPathValue dbg vid newVal =
  -- NOTE:  This action is marked as invisible, because when we set a value
  -- we take care to patch up the UI state appropriately.  In this way,
  -- we don't need to redraw the entire debugger state, which preserves
  -- open tabs, etc.
  -- XXX: One day we should probalby do something smarter, where we
  -- can compute exactly what changed, and only redraw those things...
  whenStable dbg False $ whenNotFinishied dbg Nothing $ \_ _ ->
    do ExportableState { expClosed } <- readIORef dbgExportable
       case Map.lookup vid expClosed of
         Just (ExportableValue path _) ->
           do setValue path newVal
              return (Just path)
         _   -> return Nothing
  where
  Debugger { dbgExportable } = dbg


-- | Interrupt an executing command.
pause :: Debugger -> IO ()
pause dbg = startExec dbg True Stop

-- | Run until something causes us to stop.
run :: Debugger -> IO ()
run dbg = startExec dbg True Run

-- | Start running, but return immediately.
runNonBlock :: Debugger -> IO ()
runNonBlock dbg = startExec dbg False Run

-- | Make a step.  If the step is a call, step into the called function.
stepInto :: Debugger -> IO ()
stepInto dbg = startExec dbg True StepIntoOp

-- | Make a step.  If the step is a call, step until the function returns.
stepOver :: Debugger -> IO ()
stepOver dbg = startExec dbg True StepOverOp

-- | Make a step, and continue stepping while the op-codes belong to the
-- same source code line.
stepIntoLine :: Debugger -> IO ()
stepIntoLine dbg = startExec dbg True (StepIntoLine (-42))

-- 1. Steps on same line until call
-- 2. Add dynamic break
-- 3. Steps until back from call
-- 4. Continue till the end of same line
stepOverLine :: Debugger -> IO ()
stepOverLine dbg = startExec dbg True (StepOverLine (-42))


-- | Step when returning from the current function, or find break point.
stepOutOf :: Debugger -> IO ()
stepOutOf dbg = startExec dbg True (StepOut Stop)

goto :: Int -> Debugger -> IO ()
goto pc dbg =
  whenIdle dbg $
  whenRunning dbg () $ \vm _ ->
    writeIORef (dbgStateVM dbg) (Running vm (Goto pc))


executeStatement :: Debugger -> Integer -> Text -> IO ()
executeStatement dbg frame statement =
  whenIdle dbg $
    do things <- expClosed <$> readIORef (dbgExportable dbg)
       case Map.lookup frame things of
         Just (ExportableStackFrame pc env) ->
           do whenRunning dbg () $ \vm next ->
                do recordConsoleInput statement
                   let stat = Text.unpack statement
                   next' <- executeStatementInContext vm pc env stat $ \vs ->
                     Interrupt next <$ liftIO (recordConsoleValues vs)
                   writeIORef (dbgStateVM dbg) (Running vm next')

              runNonBlock dbg

         _ -> return ()


poll :: Debugger -> Word64 -> Int {- ^ Timeout in seconds -} -> IO Word64
poll dbg _ secs =
  do mvar <- newEmptyMVar
     sendCommand dbg (AddClient mvar) False
     _ <- timeout (secs * 1000000) (takeMVar mvar)
     readIORef (dbgCommandCounter dbg)


--------------------------------------------------------------------------------


addBreakPoint ::
  Debugger -> (Int,FunId) -> Maybe Text -> IO (Maybe BreakCondition)
addBreakPoint dbg@Debugger { dbgBreaks } loc txtCon =
  whenStable dbg True $
    do mb <- case txtCon of
               Nothing  -> return Nothing
               Just txt -> prepareCondition dbg loc txt
       modifyIORef dbgBreaks (Map.insert loc mb)
       return mb

prepareCondition :: Debugger -> (Int,FunId) -> Text -> IO (Maybe BreakCondition)
prepareCondition dbg (pc,fid) expr =
  do fun  <- lookupFID dbg fid
     let stat = "return " ++ Text.unpack expr
     res <- try (compileStatementForLocation (LuaOpCodes fun) pc stat)
     return $! case res of
       Left ParseError{} -> Nothing
       Right cp -> Just BreakCondition { brkCond = cp, brkText = expr }

lookupFID :: Debugger -> FunId -> IO Function
lookupFID dbg fid =
  case funIdList fid of
    [] -> fail "Invalid chunk"
    r : rs ->
      do chunks <- readIORef (dbgSources dbg)
         case (go rs . chunkFunction) =<< Map.lookup r (topLevelChunks chunks) of
           Nothing  -> fail "Invalid chunk."
           Just fun -> return fun
  where
  go path fun =
    case path of
      []     -> return fun
      x : xs -> go xs =<< (funcProtos fun Vector.!? x)

removeBreakPoint :: Debugger -> (Int,FunId) -> IO ()
removeBreakPoint dbg@Debugger { dbgBreaks } loc =
  whenStable dbg True $ modifyIORef' dbgBreaks (Map.delete loc)

clearBreakPoints :: Debugger -> IO ()
clearBreakPoints dbg@Debugger { dbgBreaks } =
  whenStable dbg True $ writeIORef dbgBreaks Map.empty



data StepMode
  = Run
    -- ^ Evaluate until something causes us to stop.

  | StepIntoOp
    -- ^ Evaluate one op-code.
    -- If the op-code is a function call, stop at the beginning of the
    -- function.

  | StepOverOp
    -- ^ Evaluate one op-code.
    -- If the op-code is a function call, do not stop until the function
    -- returns, or something else causes us to stop.

  | Stop
    -- ^ Evaluate until the closest safe place to stop.

  | StepOut StepMode
    -- ^ Evaluate until we return from the current function.
    -- After that, procdeed with the given mode.


  | StepOverLine Int
    -- ^ Evaluate while we are on this line number.
    -- If we encoutner functions, do not stop until they return
    -- or some other condition caused us to stop.

  | StepIntoLine Int
    -- ^ Evaluate while we are on this line number.
    -- ^ If we encoutner a function-call, then we stop at the beginning
    -- of the called function.

  | StepOutYield StepMode
    -- ^ Evaluate until the current thread yeilds (or something else
    -- causes us to stop).
    -- After that, proceed with the given mode.

  deriving (Eq, Show)

runDebugger :: Debugger -> IO ()
runDebugger dbg =
  forever $
    do cmd    <- waitForCommand dbg
       mbMode <- handleCommand dbg True cmd
       case mbMode of
         Nothing   -> return ()
         Just Stop -> return () -- already stopped
         Just mode ->
           do state <- readIORef (dbgStateVM dbg)
              case state of
                Running vm next ->
                  do setIdleReason dbg Executing
                     writeIORef (dbgStateVM dbg) =<< doStepMode dbg vm next mode
                _ -> return ()
              clients  <- readIORef (dbgClients dbg)
              writeIORef (dbgClients dbg) []
              mapM_ (\client -> putMVar client ()) clients


handleCommand :: Debugger -> Bool -> DebuggerCommand -> IO (Maybe StepMode)
handleCommand dbg isIdle cmd =
  case cmd of
    WhenIdle io        -> Nothing <$ when isIdle io
    TemporaryStop m    -> Nothing <$ m
    AddClient client   -> Nothing <$ modifyIORef (dbgClients dbg) (client :)
    StartExec m client -> Just m  <$ modifyIORef (dbgClients dbg) (client :)


getCurrentLineNumber :: VM -> IO Int
getCurrentLineNumber vm =
    do let th = vmCurThread vm
       eenv <- getThreadField stExecEnv th
       pc   <- getThreadField stPC th
       return $! fromMaybe 0 $
              do (_,func) <- luaOpCodes (execFunction eenv)
                 lookupLineNumber func pc

doStepMode :: Debugger -> VM -> NextStep -> StepMode -> IO VMState
doStepMode dbg vm next mode =
  do vmstate <- oneStep vm next
     mode' <- nextMode vm next mode
     case vmstate of
       RunningInC vm' ->
          do let luaTMVar = machLuaServer (vmMachineEnv vm')
             res <- atomically $ Left  <$> waitForCommandSTM dbg
                             <|> Right <$> takeTMVar luaTMVar
             case res of
               Left m ->
                 do command <- m
                    mbNewMode <- handleCommand dbg False command
                    doStepMode dbg vm' WaitForC (fromMaybe mode' mbNewMode)

               Right cResult ->
                 do next' <- runMach vm' (handleCCallState cResult)
                    doStepMode dbg vm' next' mode'

       Running vm' next' ->
        checkStopError dbg vm' next' $
          if not (mayPauseAfter next)
            then doStepMode dbg vm' next' mode'
            else checkStop dbg mode' vm' next' $
                 checkBreakPoint dbg mode' vm' next' $
                 do mb <- peekCmd dbg
                    case mb of
                      Nothing      -> doStepMode dbg vm' next' mode'
                      Just command ->
                        do mbNewMode <- handleCommand dbg False command
                           let mode'' = fromMaybe mode' mbNewMode
                           -- external stop directive and we're already
                           -- at a safe point
                           if mode'' == Stop then
                              do setIdleReason dbg Ready
                                 return vmstate
                           else
                              doStepMode dbg vm' next' mode''

       _ -> return vmstate


checkStop :: Debugger -> StepMode -> VM -> NextStep -> IO VMState -> IO VMState
checkStop dbg mode vm next k =
  case mode of
    Stop -> do setIdleReason dbg Ready
               return (Running vm next)
    _    -> k

checkStopError :: Debugger -> VM -> NextStep -> IO VMState -> IO VMState
checkStopError dbg vm next k =
  case next of
    ThrowError e ->
      do stopOnErr <- readIORef (dbgBreakOnError dbg)
         if stopOnErr
            then do setIdleReason dbg (ThrowingError e)
                    return (Running vm next)
            else k
    _ -> k



checkBreakPoint :: Debugger -> StepMode -> VM -> NextStep ->
                                                    IO VMState -> IO VMState
checkBreakPoint dbg mode vm nextStep k =
  do let th = vmCurThread vm
     eenv <- getThreadField stExecEnv th
     let curFun = funValueName (execFunction eenv)
     pc <- getThreadField stPC th
     case curFun of
       LuaFID fid ->
         do let loc = (pc,fid)
            breaks <- readIORef (dbgBreaks dbg)
            case Map.lookup loc breaks of
              Just mbCond ->
                case mbCond of
                  Nothing -> do setIdleReason dbg ReachedBreakPoint
                                return (Running vm nextStep)
                  Just c ->
                    do nextStep' <- executeCompiledStatment vm eenv (brkCond c)
                                  $ \vs -> return $!
                                    let stop = valueBool (trimResult1 vs)
                                    in if stop
                                         then Interrupt nextStep
                                         else nextStep
                       doStepMode dbg vm nextStep' (StepOut mode)
              _ -> k
       _ -> k


-- | After executing this "step" we may pause execution.
mayPauseAfter :: NextStep -> Bool
mayPauseAfter lastStep =
  case lastStep of
    Goto{}        -> True
    ApiStart{}    -> True
    ApiEnd{}      -> True
    Interrupt{}   -> True
    _             -> False




nextMode :: VM -> NextStep -> StepMode -> IO StepMode

nextMode _ Interrupt{} _ = return Stop

nextMode vm step mode =
  case mode of

    Stop                -> return Stop

    StepIntoOp -> return $
      case step of
        Goto    {}      -> Stop
        ApiStart{}      -> Stop
        ApiEnd {}       -> Stop
        _               -> mode

    StepOverOp -> return $
      case step of
        Goto    {}      -> Stop
        ApiStart{}      -> StepOut mode
        ApiEnd {}       -> Stop
        FunCall {}      -> StepOut mode
        Resume  {}      -> StepOutYield mode
        _               -> mode

    StepOut m -> return $
      case step of
        FunCall   {}    -> StepOut mode
        ApiStart  {}    -> StepOut mode
        Resume    {}    -> StepOutYield mode
        FunReturn {}    -> m
        ErrorReturn {}  -> m
        ApiEnd    {}    -> m
        _               -> mode

    StepIntoLine n
      | n < 0 -> do l <- getCurrentLineNumber vm
                    nextMode vm step (StepIntoLine l)
      | otherwise ->
      case step of
        FunCall {}      -> return Stop
        FunTailcall {}  -> return Stop
        FunReturn {}    -> return Stop
        ErrorReturn {}  -> return Stop
        ApiStart {}     -> return Stop
        ApiEnd {}       -> return Stop
        Goto {}         -> do l <- getCurrentLineNumber vm
                              return (if n /= l then Stop else StepIntoLine l)
        _               -> return mode

    StepOverLine n
      | n < 0 -> do l <- getCurrentLineNumber vm
                    nextMode vm step (StepOverLine l)
      | otherwise ->
      case step of
        FunCall {}      -> return (StepOut mode)
        FunTailcall {}  -> return (StepOut mode)
        FunReturn {}    -> return Stop
        ErrorReturn {}  -> return Stop
        ApiStart {}     -> return (StepOut mode)
        ApiEnd {}       -> return Stop
        Resume {}       -> return (StepOutYield mode)
        Goto {}         -> do l <- getCurrentLineNumber vm
                              return (if n /= l then Stop else StepOverLine l)
        _               -> return mode

    StepOutYield m -> return $
      case step of
        Yield {}        -> m
        ThreadExit {}   -> m
        ThreadFail {}   -> m
        Resume {}       -> StepOutYield mode
        _               -> mode

    Run                 -> return Run



data DebuggerCommand =
    StartExec StepMode (MVar ())
    -- ^ Switch exetion to the given mode, or start executing if we were idle.

  | AddClient (MVar ())

  | TemporaryStop (IO ())
    -- ^ Pause the debugger---if it was executing---and execute the
    -- given IO action.  Once the action completes, the debugger will resume.

  | WhenIdle (IO ())
    -- ^ Execute the IO action, but only if the debugger is currently
    -- not doing anything else.



peekCmd :: Debugger -> IO (Maybe DebuggerCommand)
peekCmd dbg@Debugger { dbgCommand } =
  join $ atomically $
         do notready <- isEmptyTQueue dbgCommand
            if notready
              then return (return Nothing)
              else fmap Just <$> waitForCommandSTM dbg

waitForCommand :: Debugger -> IO DebuggerCommand
waitForCommand = join . atomically . waitForCommandSTM

waitForCommandSTM :: Debugger -> STM (IO DebuggerCommand)
waitForCommandSTM Debugger { dbgCommand, dbgCommandCounter } =
  do (c,vis) <- readTQueue dbgCommand
     return (c <$ when vis (modifyIORef' dbgCommandCounter (+ 1)))


{- | The boolan indicates if the command should contribute towards the
global command counter.  Generally, commands that might change something
in the state should affect this, while "read-only" commands may be invisible. -}
sendCommand :: Debugger -> DebuggerCommand -> Bool {-^Visible?-} -> IO ()
sendCommand Debugger { dbgCommand } c vis = atomically (writeTQueue dbgCommand (c,vis))

{- | Switch the debugger to the given execution mode.
The boolean indicates if we should block:  if it is 'True', then
this command will block until the execution stops.  If it is 'False',
then we return immediately. -}
startExec :: Debugger -> Bool -> StepMode -> IO ()
startExec dbg blocking mode =
  do mvar <- newEmptyMVar
     sendCommand dbg (StartExec mode mvar) True
     when blocking (takeMVar mvar)

-- | Execute the function, only if the debugger has not finished.
whenNotFinishied :: Debugger -> a -> (VM -> NextStep -> IO a) -> IO a
whenNotFinishied dbg a io =
  do state <- readIORef (dbgStateVM dbg)
     case state of
       Running vm next -> io vm next
       RunningInC vm   -> io vm WaitForC
       _               -> return a

-- | Execute the function, only if the debugger is running in Lua
-- rather than waiting for C.
whenRunning :: Debugger -> a -> (VM -> NextStep -> IO a) -> IO a
whenRunning dbg a io =
  do state <- readIORef (dbgStateVM dbg)
     case state of
       Running vm next -> io vm next
       _               -> return a


whenIdle :: Debugger -> IO () -> IO ()
whenIdle dbg io = sendCommand dbg (WhenIdle io) True
  -- The 'True' Is conservative, but generally if we are going to be doing
  -- something when we have to be idle, it probably affects the state.

{- | Execute an IO action as soon as the debugger is in a stable state.
This IO action should not block or take too long, otherwise the debugger
will be unresponsive.  Generally, the IO action should compute something
about the debugger state, or update some sort of internal state.  -}
whenStable :: Debugger -> Bool -> IO a -> IO a
whenStable dbg vis io =
  do mvar <- newEmptyMVar
     sendCommand dbg (TemporaryStop (putMVar mvar =<< io)) vis
     takeMVar mvar

--------------------------------------------------------------------------------









