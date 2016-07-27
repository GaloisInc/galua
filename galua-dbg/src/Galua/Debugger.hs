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
  ) where

import           Galua(setupLuaState)
import           Galua.Debugger.PrettySource(lexChunk,Line)
import           Galua.Debugger.Options

import           Galua.Mach
import           Galua.Stepper
import           Galua.Reference
import           Galua.Value
import qualified Galua.SizedVector as SV

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

import            Control.Concurrent
                    ( MVar, newEmptyMVar, putMVar, takeMVar
                    , forkIO, forkFinally
                    , threadDelay, killThread
                    )
import           Control.Concurrent(Chan,newChan,isEmptyChan,readChan,writeChan)
import           Control.Monad(when,forever)
import           MonadLib(ExceptionT,runExceptionT,raise,lift)


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


  , dbgCommand   :: !(Chan (DebuggerCommand,Bool))
    {- ^ Commands from the outside world, telling the debugger what to do next.
         The boolean indicates if this command modifis the state, and as such
         should modify `dbgCommandCounter` -}

  , dbgCommandCounter :: !(IORef Word64)
    {- ^ Every time we do a commond, we should increment this counter.
    This is useful to implement polling from the outside world,
    where the client can know if there have been any commands executed
    since last time they looked. -}



  , dbgClients   :: !(IORef [MVar ()]) --
    -- ^ Notify these when we stop


  , dbgIdleReason :: !(IORef IdleReason)

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
  }

newExportableState :: ExportableState
newExportableState = ExportableState { expNextThing = 0
                                     , expClosed = Map.empty
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

-- | All loaded top-level functions.
data Chunks = Chunks
  { topLevelChunks :: Map Int ChunkInfo
    -- ^ The key is the chunk id.  The top-level function has for chunk @k@
    -- is @[k]@.
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
    ChunkInfo { chunkSource   = lexSourceFile mbName bytes'
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
                     }

-- | Syntax high-lighting for a source file.
lexSourceFile :: Maybe String -> ByteString -> Source
lexSourceFile srcName bytes = Source { srcName, srcLines }
  where srcLines = lexChunk (fromMaybe "" srcName) bytes

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


newEmptyDebugger :: Options -> IO (Ptr (), Debugger)
newEmptyDebugger opts =
  do let chunks = Chunks { topLevelChunks = Map.empty
                         , allFunNames    = Map.empty
                         }
     dbgCommand <- newChan
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
                 }

     (cptr, dbgNames, vm, next) <- setupLuaState cfg

     dbgIdleReason   <- newIORef Ready
     dbgStateVM      <- newIORef (Running vm next)

     dbgExportable   <- newIORef newExportableState
     dbgBreakOnError <- newIORef True

     let dbg = Debugger { dbgSources, dbgNames, dbgIdleReason
                        , dbgBreaks, dbgWatches
                        , dbgCommand, dbgCommandCounter, dbgClients
                        , dbgExportable, dbgStateVM
                        , dbgBreakOnError, dbgBrkAddOnLoad }

     _ <- forkIO (runDebbugger dbg)

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
       case execFunction (stExecEnv th) of
         LuaFunction fid _ ->
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
  whenStable dbg True $ whenNotFinishied dbg $ \_ _ ->
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
  whenNotFinishied dbg $ \vm _ ->
  writeIORef (dbgStateVM dbg) (Running vm (Goto pc))


poll :: Debugger -> Word64 -> Int {- ^ Timeout in seconds -} -> IO Word64
poll dbg _ timeout =
  do mvar <- newEmptyMVar
     sendCommand dbg (AddClient mvar) False
     tid <- -- we are forking here
            do threadDelay (timeout * 10^(6::Int))
               putMVar mvar ()
             `forkFinally` \_ -> return ()

     takeMVar mvar
     killThread tid
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

runDebbugger :: Debugger -> IO ()
runDebbugger dbg =
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
         case execFunction (stExecEnv th) of
           LuaFunction _ func | Just l' <- lookupLineNumber func pc -> l'
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

  proceed mode' =
    do let Debugger { dbgNames } = dbg
       breaked <- if firstStep || breakImmune next
                    then return False
                    else checkBreak dbg vm next

       let goOn =
            do st <- runAllocWith dbgNames (oneStep vm next)
               case st of
                 Running vm' next' -> doStepMode dbg vm' next' mode' False
                 _                 -> return st

       if breaked
         then done
         else do mb <- peekCmd dbg
                 case mb of
                   Nothing -> goOn
                   Just command ->
                     do mbMode <- handleCommand dbg False command
                        case mbMode of
                          Nothing      -> goOn
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
  do isEmpty <- isEmptyChan dbgCommand
     if isEmpty then return Nothing else Just <$> waitForCommand dbg

waitForCommand :: Debugger -> IO DebuggerCommand
waitForCommand Debugger { dbgCommand, dbgCommandCounter } =
  do (c,vis) <- readChan dbgCommand
     when vis (modifyIORef' dbgCommandCounter (+ 1))
     return c


{- | The boolan indicates if the command should contribute towards the
global command counter.  Generally, commands that might change something
in the state should affect this, while "read-only" commands may be invisible. -}
sendCommand :: Debugger -> DebuggerCommand -> Bool {-^Visible?-} -> IO ()
sendCommand Debugger { dbgCommand } c vis = writeChan dbgCommand (c,vis)

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
whenNotFinishied :: Debugger -> (VM -> NextStep -> IO ()) -> IO ()
whenNotFinishied dbg io =
  do state <- readIORef (dbgStateVM dbg)
     case state of
       Running vm next -> io vm next
       _               -> return ()


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









