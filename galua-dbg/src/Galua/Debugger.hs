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

  , ExecEnvId(..)
  , exportExecEnvId
  , importExecEnvId
  , resolveName
  ) where

import           Galua(setupLuaState)
import           Galua.CallIntoC (handleCCallState)
import           Galua.Debugger.PrettySource
                  (lexChunk,Line,NameId,LocatedExprName)
import           Galua.Debugger.Options

import           Galua.Mach
import           Galua.Stepper
import           Galua.Reference
import           Galua.Value
import           Galua.FunValue
import           Galua.Names.Eval
import           Galua.Names.Find(LocatedExprName(..),ppExprName)
import qualified Galua.Util.SizedVector as SV

import           Language.Lua.Bytecode(Function(..))
import           Language.Lua.Bytecode.Debug
                    (lookupLineNumber,inferSubFunctionNames,deepLineNumberMap)
import           Language.Lua.Bytecode.FunId

import           Data.Maybe (catMaybes)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set ( Set )
import qualified Data.Set as Set
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Word(Word64)
import           Data.Text(Text)
import qualified Data.Text as Text
import           Data.Text.Read(decimal)
import           Data.Text.Encoding(decodeUtf8)

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B

import           Foreign (Ptr)
import           Data.Maybe(fromMaybe,maybeToList)
import           Data.Ord(comparing)
import           Data.List(unfoldr,minimumBy)
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Data.IORef(IORef,readIORef,writeIORef,
                            modifyIORef,modifyIORef',newIORef,
                            atomicModifyIORef')

import           Control.Concurrent.Async (cancel, waitCatch, Async)
import            Control.Concurrent
                    ( MVar, newEmptyMVar, putMVar, takeMVar, forkIO )
import           Control.Applicative ((<|>))
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
dbgSource.  Similarly, `dbgNames` is updated when we allocate new
values. -}
data Debugger = Debugger
  { dbgSources   :: !(IORef Chunks)
    {- ^ Source code for chunks and corresponding parsed functions.
          The functions are also in the VM state, but that's only available
          while the machine is running. -}

  , dbgNames     :: !AllocRef                  -- ^ Object identities


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

  , dbgBreaks    :: !(IORef (Set (Int,FunId)))      -- ^ Static break points
  , dbgBreakOnError :: !(IORef Bool)
    -- ^ Should we stop automatically, when we encounter an error.

  , dbgBrkAddOnLoad :: !(IORef CommandLineBreakPoints)
    {- ^ When a chunk is loaded, we added a break point at the beginning
         of each of the functions in the corresponding entry in the map.
         The break points at key 'Nothing' are added to the first chunk
         tath is loaded. -}

  , dbgWatches   :: !(IORef (Seq ValuePath))       -- ^ Watched values

  , dbgExportable :: !(IORef ExportableState)
    -- ^ Things that may be expanded further.
  }




data IdleReason = Ready
                | ReachedBreakPoint
                | ThrowingError Value
                | Executing

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
                do clo <- lift (readRef ref)
                   case cloUpvalues clo Vector.!? n of
                     Just r  -> lift (readIORef r)
                     Nothing -> raise ()
             _ -> raise ()

      VP_MetaTable vp' ->
        do val <- getVal vp'
           case val of
             Table r -> do mb <- lift $ getTableMeta r
                           case mb of
                             Just tr -> return (Table tr)
                             Nothing -> raise ()
             UserData r -> do u <- lift (readRef r)
                              case userDataMeta u of
                                Just tr -> return (Table tr)
                                Nothing -> raise ()
             _ -> raise () -- XXX: Does anything else have a metatable?

      VP_Register ExecEnv { execStack } n ->
        do mb <- lift (SV.getMaybe execStack n)
           case mb of
             Just r  -> lift (readIORef r)
             Nothing -> raise ()

      VP_Upvalue ExecEnv { execUpvals } n ->
        case execUpvals Vector.!? n of
          Just r  -> lift (readIORef r)
          Nothing -> raise ()

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
        do clo <- readRef ref
           case cloUpvalues clo Vector.!? n of
             Nothing -> return ()
             Just r  -> writeIORef r v

      _ -> return ()

  setMeta val =
    case v of
      Nil     -> doSetMeta val Nothing
      Table r -> doSetMeta val (Just r)
      _       -> return ()

  doSetMeta val mb1 =
    case val of
      Table ref    -> setTableMeta ref mb1
      UserData ref -> modifyRef ref (\u -> u { userDataMeta = mb1 })
      _            -> return ()

  setReg n ExecEnv { execStack } =
    do mb <- SV.getMaybe execStack n
       case mb of
         Just r  -> writeIORef r v
         Nothing -> return ()

  setUVal n ExecEnv { execUpvals } =
    case execUpvals Vector.!? n of
      Just r  -> writeIORef r v
      Nothing -> return ()

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
         runAllocWith (dbgNames dbg) $
           do mb <- lookupRef tid
              case mb of
                Nothing  -> liftIO $ nameResolveException "Invalid thread."
                Just ref ->
                  do eenv <- stExecEnv <$> readRef ref
                     liftIO $ execEnvToNameResolveEnv metaTabs eenv

       ClosureEnvId cid ->
         runAllocWith (dbgNames dbg) $
           do mb <- lookupRef cid
              case mb of
                Nothing  -> liftIO $ nameResolveException "Invalid closure."
                Just ref ->
                  do closure <- readRef ref
                     liftIO $ closureToResolveEnv metaTabs closure

closureToResolveEnv ::
  TypeMetatables -> Closure -> IO (FunId, NameResolveEnv)
closureToResolveEnv metaTabs c =
  do (fid,func) <- case luaOpCodes (cloFun c) of
                     Just (fid,func) -> return (fid,func)
                     _ -> nameResolveException "Not in a Lua function."
     stack <- SV.new
     let nre = NameResolveEnv
                 { nrUpvals   = cloUpvalues c
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

     let nre = NameResolveEnv
                 { nrUpvals   = execUpvals eenv
                 , nrStack    = execStack eenv
                 , nrFunction = func
                 , nrMetas    = metaTabs
                 }

     return (fid, nre)

resolveName :: Debugger -> ExecEnvId -> Maybe Int -> NameId ->
                IO (Either NotFound (String,Value))
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
     return (ppExprName en, v)





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
                 IORef (Set (Int,FunId)) ->
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
            modifyIORef' breakRef $ Set.union $ Set.fromList newBrk

  where
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


newEmptyDebugger :: MVar (Async a) -> Options -> IO (Ptr (), Debugger)
newEmptyDebugger threadVar opts =
  do let chunks = Chunks { topLevelChunks = Map.empty
                         , allFunNames    = Map.empty
                         }
     dbgCommand <- atomically newTQueue
     dbgCommandCounter <- newIORef 0


     dbgClients <- newIORef []
     dbgSources <- newIORef chunks
     dbgWatches <- newIORef Seq.empty
     dbgBrkAddOnLoad <- newIORef (optBreakPoints opts)
     dbgBreaks       <- newIORef Set.empty

     let cfg = MachConfig
                 { machOnChunkLoad = addSourceFile dbgBrkAddOnLoad
                                                   dbgBreaks
                                                   dbgSources
                 , machOnShutdown =
                     do a <- takeMVar threadVar
                        cancel a
                        () <$ waitCatch a
                 }

     (cptr, dbgNames, vm, next) <- setupLuaState cfg

     dbgIdleReason   <- newIORef Ready
     dbgStateVM      <- newIORef (Running vm next)

     dbgExportable   <- newIORef newExportableState
     dbgBreakOnError <- newIORef (optBreakOnError opts)

     let dbg = Debugger { dbgSources, dbgNames, dbgIdleReason
                        , dbgBreaks, dbgWatches
                        , dbgCommand, dbgCommandCounter, dbgClients
                        , dbgExportable, dbgStateVM
                        , dbgBreakOnError, dbgBrkAddOnLoad }

     _ <- forkIO (runDebugger dbg)

     return (cptr, dbg)





-- | Check for a break point or interruption.
-- Returns 'True' if we should stop.
checkBreak :: Debugger -> VM -> NextStep -> IO Bool
checkBreak d@Debugger { dbgBreaks, dbgBreakOnError } vm next =
  mOr [ checkForErr, checkStaticBreak ]
  where
  mOr opts =
    case opts of
      [] -> return False
      m : ms -> do mb <- m
                   if mb then return True else mOr ms

  checkForErr =
    do breakOnErr <- readIORef dbgBreakOnError
       if breakOnErr
          then case next of
                 ThrowError e -> do setIdleReason d (ThrowingError e)
                                    return True
                 _            -> return False
          else return False

  checkStaticBreak =
    do th <- readRef (vmCurThread vm)
       case funValueName (execFunction (stExecEnv th)) of
         LuaFID fid ->
           do breaks <- readIORef dbgBreaks
              case next of
                Goto pc | (pc, fid) `Set.member` breaks ->
                  do setIdleReason d ReachedBreakPoint
                     return True
                _  -> return False
         _ -> return False

setIdleReason :: Debugger -> IdleReason -> IO ()
setIdleReason Debugger { .. } x = writeIORef dbgIdleReason x


--------------------------------------------------------------------------------
-- Entry points


setPathValue :: Debugger -> Integer -> Value -> IO ()
setPathValue dbg vid newVal =
  whenStable dbg True $ whenNotFinishied dbg () $ \_ _ ->
    do ExportableState { expClosed } <- readIORef dbgExportable
       case Map.lookup vid expClosed of
         Just (ExportableValue path _) -> setValue path newVal
         _                             -> return ()
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


poll :: Debugger -> Word64 -> Int {- ^ Timeout in seconds -} -> IO Word64
poll dbg _ secs =
  do mvar <- newEmptyMVar
     sendCommand dbg (AddClient mvar) False
     _ <- timeout (secs * 1000000) (takeMVar mvar)
     readIORef (dbgCommandCounter dbg)


--------------------------------------------------------------------------------


addBreakPoint :: Debugger -> (Int,FunId) -> IO ()
addBreakPoint dbg@Debugger { dbgBreaks } loc =
  whenStable dbg True $ modifyIORef' dbgBreaks (Set.insert loc)

removeBreakPoint :: Debugger -> (Int,FunId) -> IO ()
removeBreakPoint dbg@Debugger { dbgBreaks } loc =
  whenStable dbg True $ modifyIORef' dbgBreaks (Set.delete loc)

clearBreakPoints :: Debugger -> IO ()
clearBreakPoints dbg@Debugger { dbgBreaks } =
  whenStable dbg True $ writeIORef dbgBreaks Set.empty

doStartExec :: Debugger -> VM -> NextStep -> StepMode -> IO VMState
doStartExec dbg vm next newMode =
  do setIdleReason dbg Executing
     doStepMode dbg vm next newMode True



data StepMode
  = Run
  | StepIntoOp
  | StepOverOp
  | Stop
  | StepOut StepMode
  | StepOverLine Int
  | StepIntoLine Int
  | StepOutYield StepMode
  deriving Show

runDebugger :: Debugger -> IO ()
runDebugger dbg =
  forever $
    do cmd    <- waitForCommand dbg
       mbMode <- handleCommand dbg True cmd
       case mbMode of
         Nothing   -> return ()
         Just mode ->
           do state <- readIORef (dbgStateVM dbg)
              case state of
                Running vm next ->
                  do newState <- doStartExec dbg vm next mode
                     writeIORef (dbgStateVM dbg) newState
                _ -> return ()
              finishStep dbg

handleCommand :: Debugger -> Bool -> DebuggerCommand -> IO (Maybe StepMode)
handleCommand dbg isIdle cmd =
  case cmd of
    WhenIdle io -> when isIdle io >> return Nothing
    TemporaryStop m -> m >> return Nothing
    AddClient client -> do modifyIORef (dbgClients dbg) (client :)
                           return Nothing
    StartExec mode client ->
      do modifyIORef (dbgClients dbg) (client :)
         return (Just mode)

finishStep :: Debugger -> IO ()
finishStep dbg =
  do clients  <- readIORef (dbgClients dbg)
     writeIORef (dbgClients dbg) []
     mapM_ (\client -> putMVar client ()) clients



doStepMode :: Debugger -> VM -> NextStep -> StepMode -> Bool -> IO VMState
doStepMode dbg vm next mode firstStep =
  case (mode,next) of
    (Run, _         )     -> proceed Run
    (_  , PrimStep{})     -> proceed mode

    (Stop, op) | breakImmune op -> proceed mode
               | otherwise      -> done

    (StepOutYield m, Yield{})       -> proceed m
    (StepOutYield m, ThreadExit{})  -> proceed m
    (StepOutYield m, ThreadFail{})  -> proceed m
    (StepOutYield{}, Resume{})      -> proceed (StepOutYield mode)
    (StepOutYield{}, _)             -> proceed mode

    (StepOut{}, Resume{})           -> proceed (StepOutYield mode)
    (StepOut m, ErrorReturn{})      -> proceed m
    (StepOut{}, FunCall  {})        -> proceed (StepOut mode)
    (StepOut m, FunReturn{})        -> proceed m
    (StepOut{}, _          )        -> proceed mode

    (StepIntoOp, _         ) | firstStep -> proceed StepIntoOp
    (StepIntoOp, Goto    {}) -> done
    (StepIntoOp, ApiStart{}) -> done
    (StepIntoOp, ApiEnd  {}) -> done
    (StepIntoOp, _         ) -> proceed mode

    (StepOverOp, Resume  {}) -> proceed (StepOutYield mode)
    (StepOverOp, FunCall {}) -> proceed (StepOut Stop)
    (StepOverOp, ApiStart{}) -> proceed (StepOut Stop)
    (StepOverOp, _         ) | firstStep -> proceed mode
    (StepOverOp, Goto    {}) -> done
    (StepOverOp, ApiEnd  {}) -> done
    (StepOverOp, _         ) -> proceed mode

    (StepOverLine{}, Resume  {})    -> proceed (StepOutYield mode)
    (StepOverLine{}, FunTailcall{}) -> proceed Stop
    (StepOverLine{}, FunCall {})    -> proceed (StepOut mode)
    (StepOverLine{}, ApiStart{})    -> proceed (StepOut mode)
    (StepOverLine l, Goto pc   )    ->
      do l' <- getLineNumber pc
         if firstStep
           then proceed (StepOverLine l')
           else if l == l' && l' /= 0
             then proceed mode
             else done
    (StepOverLine{}, _         ) | firstStep -> proceed mode
    (StepOverLine{}, ApiEnd  {}) -> done
    (StepOverLine{}, FunReturn{}) -> proceed Stop
    (StepOverLine{}, ErrorReturn{}) -> proceed Stop
    (StepOverLine{}, ThreadExit{})  -> proceed Stop
    (StepOverLine{}, ThreadFail{})  -> proceed Stop
    (StepOverLine{}, Yield{})       -> proceed Stop
    (StepOverLine{}, _          ) -> proceed mode

    (StepIntoLine l, Goto pc   ) ->
      do l' <- getLineNumber pc
         if firstStep
           then proceed (StepIntoLine l')
           else if l == l' && l' /= 0
             then proceed mode
             else done
    (StepIntoLine{}, FunTailcall{}) -> proceed Stop
    (StepIntoLine{}, FunCall {})    -> proceed Stop
    (StepIntoLine{}, FunReturn{})   -> proceed Stop
    (StepIntoLine{}, ErrorReturn{}) -> proceed Stop
    (StepIntoLine{}, Yield{})       -> proceed Stop
    (StepIntoLine{}, ThreadExit{})  -> proceed Stop
    (StepIntoLine{}, ThreadFail{})  -> proceed Stop
    (StepIntoLine{}, _         ) | firstStep -> proceed mode
    (StepIntoLine{}, ApiStart{}) -> done
    (StepIntoLine{}, ApiEnd  {}) -> done
    (StepIntoLine{}, _          ) -> proceed mode

  where
  getLineNumber pc =
    do th <- readRef (vmCurThread vm)
       return $!
         case luaOpCodes (execFunction (stExecEnv th)) of
           Just (_,func) | Just l' <- lookupLineNumber func pc -> l'
           _ -> 0

  done = do r <- readIORef (dbgIdleReason dbg)
            case r of
              Executing -> setIdleReason dbg Ready
              _         -> return ()
            return (Running vm next)

  breakImmune Goto{}        = False
  breakImmune ApiStart{}    = False
  breakImmune ApiEnd{}      = False
  breakImmune ThrowError {} = False
  breakImmune _             = True

  goOn vm1 mode' =
    do let Debugger { dbgNames } = dbg
       st <- runAllocWith dbgNames (oneStep vm1 next)
       case st of
         Running vm' next' -> doStepMode dbg vm' next' mode' False

         RunningInC vm' ->
            do let luaTMVar = machLuaServer (vmMachineEnv vm')
               res <- atomically $ Left  <$> waitForCommandSTM dbg
                               <|> Right <$> takeTMVar luaTMVar
               case res of
                 Left m ->
                   do command <- m
                      mbMode <- handleCommand dbg False command
                      case mbMode of
                        Nothing      -> goOn vm' mode'
                        Just newMode -> doStartExec dbg vm' WaitForC newMode

                 Right cResult ->
                   do let next' = runMach vm' (handleCCallState cResult)
                      doStepMode dbg vm' next' mode' False

         _                 -> return st

  proceed mode' =
    do let Debugger { dbgNames } = dbg
       breaked <- if firstStep || breakImmune next
                    then return False
                    else checkBreak dbg vm next


       if breaked
         then done
         else do mb <- peekCmd dbg
                 case mb of
                   Nothing -> goOn vm mode'
                   Just command ->
                     do mbMode <- handleCommand dbg False command
                        case mbMode of
                          Nothing      -> goOn vm mode'
                          Just newMode -> doStartExec dbg vm next newMode



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









