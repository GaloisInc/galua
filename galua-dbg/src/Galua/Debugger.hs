{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# Language BangPatterns, OverloadedStrings #-}
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
import           Galua.Debugger.PrettySource(NameId)
import           Galua.Debugger.Options
import           Galua.Debugger.NameHarness
import           Galua.Debugger.Console(recordConsoleInput,recordConsoleValues)
import           Galua.Debugger.CommandQueue
import           Galua.Debugger.ValuePath
import           Galua.Debugger.Source
import           Galua.Debugger.WatchList
import           Galua.Debugger.Specs
import           Galua.Debugger.Exportable
import           Galua.Debugger.ResolveName
import           Galua.Debugger.StepMode

import           Galua.Code
import           Galua.Mach
import           Galua.MachUtils(VMState(..))
import           Galua.Stepper
import           Galua.Value
import           Galua.FunValue
import           Galua.Names.Eval
import           Galua.Names.Find(LocatedExprName(..),ppExprName)
import qualified Galua.Util.SmallVec as SMV
import           Galua.Util.IOURef

import qualified Galua.Spec.AST as Spec
import qualified Galua.Spec.Parser as Spec

import           Data.Maybe (fromMaybe)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Word(Word64)
import           Data.Text(Text)
import qualified Data.Text as Text

import           Foreign (Ptr)
import qualified Data.Vector as Vector
import           Data.IORef(IORef,readIORef,writeIORef,
                            modifyIORef,modifyIORef',newIORef)

import            Control.Concurrent
                    ( MVar, newEmptyMVar, putMVar, takeMVar, forkIO )
import           Control.Applicative ((<|>))
import           Control.Concurrent (killThread, ThreadId)
import           Control.Concurrent.STM (atomically, takeTMVar)
import           Control.Monad(when,forever)
import           Control.Exception
import           System.Timeout(timeout)


{- The fields in this are mutable, so that the interpreter can modify them
easily, without having to pass the state aroud.  For example, when we
load a new module, the interpreter uses an IO action, which modifies 
dbgSource. -}
data Debugger = Debugger
  { dbgSources   :: {-# UNPACK #-} !(IORef Chunks)
    {- ^ Source code for chunks and corresponding parsed functions.
          The functions are also in the VM state, but that's only available
          while the machine is running. -}

  , dbgCommand   :: {-# UNPACK #-} !(CommandQueue DebuggerCommand)


  , dbgClients   :: {-# UNPACK #-} !(IORef [MVar ()]) --
    -- ^ Notify these when we stop


  , dbgIdleReason :: {-# UNPACK #-} !(IORef IdleReason)
    -- ^ Why are we not running

  , dbgStateVM   :: {-# UNPACK #-} !(IORef VMState)
    -- ^ The interpreter state

  , dbgBreaks    :: {-# UNPACK #-}
                          !(IORef (Map (Int,FunId) (Maybe BreakCondition)))
    -- ^ Static break points.  The key of the map is the break point,
    -- the value is an optional condition.  The breakpoint will only
    -- be active if the condition is true.

  , dbgBreakOnError :: {-# UNPACK #-} !(IOURef Bool)
    -- ^ Should we stop automatically, when we encounter an error.

  , dbgBrkAddOnLoad :: {-# UNPACK #-} !(IORef CommandLineBreakPoints)
    {- ^ When a chunk is loaded, we added a break point at the beginning
         of each of the functions in the corresponding entry in the map.
         The break points at key 'Nothing' are added to the first chunk
         that is loaded. -}

  , dbgWatches   :: {-# UNPACK #-} !(IORef WatchList)
    -- ^ Monitored locations.

  , dbgExportable :: {-# UNPACK #-} !(IORef ExportableState)
    -- ^ Things that may be expanded further.

  , dbgDeclaredTypes :: {-# UNPACK #-} !(IORef GlobalTypeMap)
    -- ^ Types
  }


--------------------------------------------------------------------------------
-- | Tell the user why we stopped.
data IdleReason = Ready
                | ReachedBreakPoint
                | ThrowingError Value
                | Executing

-- | Modify the paused setting.
setIdleReason :: Debugger -> IdleReason -> IO ()
setIdleReason Debugger { .. } x = writeIORef dbgIdleReason x
--------------------------------------------------------------------------------


-- | Conditional break points.
data BreakCondition = BreakCondition
  { brkCond :: CompiledStatment
    -- ^ Use this to evaluate the condition

  , brkText :: Text
    -- ^ Use this to display the condition

  , brkActive :: IORef (Maybe Bool)
    -- ^ Did we alerady check this condition.
  }


--------------------------------------------------------------------------------


-- | Allocate a new debugger structure and fork off a thread that
-- listens for things to do.
newEmptyDebugger :: MVar ThreadId -> Options -> IO (Ptr (), Debugger)
newEmptyDebugger threadVar opts =
  do dbgCommand <- newCommandQueue

     dbgClients <- newIORef []
     dbgSources <- newIORef blankChunks
     dbgWatches <- newIORef watchListEmpty
     dbgBrkAddOnLoad <- newIORef (optBreakPoints opts)
     dbgBreaks       <- newIORef Map.empty

     let query "port" = return (Number 8000)    -- XXX:??
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
     dbgBreakOnError <- newIOURef (optBreakOnError opts)

     dbgDeclaredTypes <-
        (newIORef . makeGlobalTypeMap) =<<
          (do s <- Spec.specFromFile "lua.spec" -- XXX: search for specs, etc.
              return (Spec.specDecls s)
            `catch` \SomeException {} -> return [])

     let dbg = Debugger { dbgSources, dbgIdleReason
                        , dbgBreaks, dbgWatches
                        , dbgCommand, dbgClients
                        , dbgExportable, dbgStateVM
                        , dbgBreakOnError, dbgBrkAddOnLoad
                        , dbgDeclaredTypes
                        }

     _ <- forkIO (forever (runDebugger dbg))

     return (cptr, dbg)

-- | Handle a dommand for the debugger.
runDebugger :: Debugger -> IO ()
runDebugger dbg =
  do cmd    <- waitForCommand (dbgCommand dbg)
     mbMode <- handleCommand dbg True cmd
     case mbMode of
       Nothing   -> return ()
       Just Stop -> return () -- already stopped
       Just mode ->
         do state <- readIORef (dbgStateVM dbg)
            case state of
              Running vm next ->
                do setIdleReason dbg Executing
                   let mode' = nextMode vm next mode
                   vms <- doStepMode dbg vm next $! mode'
                   writeIORef (dbgStateVM dbg) vms
              _ -> return ()
            clients  <- readIORef (dbgClients dbg)
            writeIORef (dbgClients dbg) []
            mapM_ (\client -> putMVar client ()) clients



data DebuggerCommand =
    StartExec StepMode (MVar ())
    -- ^ Switch exetion to the given mode, or start executing if we were idle.

  | AddClient (MVar ())

  | TemporaryStop (IO ())
    -- ^ Pause the debugger---if it was executing---and execute the
    -- given IO action.  Once the action completes, the debugger will resume.

  | WhenIdle (IO ())
    -- ^ Execute the IO action, but only if the debugger is currently
    -- not doing anything el


handleCommand :: Debugger -> Bool -> DebuggerCommand -> IO (Maybe StepMode)
handleCommand dbg isIdle cmd =
  case cmd of
    WhenIdle io        -> Nothing <$ when isIdle io
    TemporaryStop m    -> Nothing <$ m
    AddClient client   -> Nothing <$ modifyIORef (dbgClients dbg) (client :)
    StartExec m client -> Just m  <$ modifyIORef (dbgClients dbg) (client :)



doStepMode :: Debugger -> VM -> NextStep -> StepMode -> IO VMState
doStepMode dbg vm next mode = oneStep' Cont { .. } vm next
  where
  runningInC !vm' =
    do let luaTMVar = machLuaServer (vmMachineEnv vm')
       res <- atomically $ Left  <$> waitForCommandSTM (dbgCommand dbg)
                       <|> Right <$> takeTMVar luaTMVar
       case res of
         Left m ->
           do command <- m
              mbNewMode <- handleCommand dbg False command
              doStepMode dbg vm' WaitForC (fromMaybe mode mbNewMode)

         Right cResult ->
           do next' <- handleCCallState vm' cResult
              let mode' = nextMode vm' next' mode
              checkStop dbg mode' (Running vm' next') $
                doStepMode dbg vm' next' mode'



  running !vm' !next' =
    do mode' <- return $! nextMode vm' next' mode
       checkStopError dbg vm' next' $
         if not (mayPauseBefore next')
           then doStepMode dbg vm' next' mode'
           else checkStop dbg mode' (Running vm' next') $
                checkBreakPoint dbg mode' vm' next' $
                do mb <- peekCmd (dbgCommand dbg)
                   case mb of
                     Nothing      -> doStepMode dbg vm' next' mode'
                     Just command ->
                       do mbNewMode <- handleCommand dbg False command
                          let mode'' = fromMaybe mode' mbNewMode
                          -- external stop directive and we're already
                          -- at a safe point
                          checkStop dbg mode'' (Running vm' next') $
                            doStepMode dbg vm' next' mode''

  finishedOk vs       = return $! FinishedOk vs
  finishedWithError v = return $! FinishedWithError v


--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- Entry points

resolveName :: Debugger -> ExecEnvId -> Maybe Int -> NameId ->
                IO (Either NotFound (String,Value,Maybe GlobalTypeEntry))
resolveName dbg eid pc nid =
  whenStable dbg False $
  whenNotFinishied dbg (Left $ NotFound "Not executing") $ \vm _ ->
  try $
  do (fid, resEnv) <- findNameResolveEnv (dbgExportable dbg) vm eid
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
                   (next',vm') <-
                      executeStatementInContext vm pc env stat $ \vs ->
                        Interrupt next <$ recordConsoleValues (SMV.toList vs)
                   writeIORef (dbgStateVM dbg) (Running vm' next')

              runNonBlock dbg

         _ -> return ()


poll :: Debugger -> Word64 -> Int {- ^ Timeout in seconds -} -> IO Word64
poll dbg _ secs =
  do mvar <- newEmptyMVar
     sendCommand (dbgCommand dbg) (AddClient mvar) False
     _ <- timeout (secs * 1000000) (takeMVar mvar)
     getCommandCount (dbgCommand dbg)



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
     act <- newIORef Nothing
     return $! case res of
       Left ParseError{} -> Nothing
       Right cp -> Just BreakCondition { brkCond = cp
                                       , brkText = expr
                                       , brkActive = act }

lookupFID :: Debugger -> FunId -> IO Function
lookupFID dbg fid =
  case funIdList fid of
    [] -> fail "Invalid chunk"
    r : rs ->
      do chunks <- readIORef (dbgSources dbg)
         case go rs . chunkFunction =<< Map.lookup r (topLevelChunks chunks) of
           Nothing  -> fail "Invalid chunk."
           Just fun -> return fun
  where
  go path fun =
    case path of
      []     -> return fun
      x : xs -> go xs =<< (funcNested fun Vector.!? x)

removeBreakPoint :: Debugger -> (Int,FunId) -> IO ()
removeBreakPoint dbg@Debugger { dbgBreaks } loc =
  whenStable dbg True $ modifyIORef' dbgBreaks (Map.delete loc)

clearBreakPoints :: Debugger -> IO ()
clearBreakPoints dbg@Debugger { dbgBreaks } =
  whenStable dbg True $ writeIORef dbgBreaks Map.empty




checkStop :: Debugger -> StepMode -> VMState -> IO VMState -> IO VMState
checkStop dbg mode vms k =
  case mode of
    Stop -> do setIdleReason dbg Ready
               return vms
    _    -> k

checkStopError :: Debugger -> VM -> NextStep -> IO VMState -> IO VMState
checkStopError dbg vm next k =
  case next of
    ThrowError e ->
      do stopOnErr <- readIOURef (dbgBreakOnError dbg)
         if stopOnErr
            then do setIdleReason dbg (ThrowingError e)
                    return (Running vm next)
            else k
    _ -> k



checkBreakPoint :: Debugger -> StepMode -> VM -> NextStep ->
                                                    IO VMState -> IO VMState
checkBreakPoint dbg mode vm nextStep k =
  do let eenv = vmCurExecEnv vm
         curFun = funValueName (execFun eenv)
     case (nextStep, curFun) of
       (Goto pc, LuaFID fid) ->
         do let loc = (pc,fid)
            breaks <- readIORef (dbgBreaks dbg)
            case Map.lookup loc breaks of
              Just mbCond ->
                case mbCond of
                  Nothing -> atBreak
                  Just c ->
                    do act <- readIORef (brkActive c)
                       case act of
                         -- Already checked if active
                         Just active ->
                           do writeIORef (brkActive c) Nothing
                              if active then atBreak else k

                         -- Start evaluating the breakpoint condition
                         Nothing ->
                           do (nextStep',vm') <-
                                 executeCompiledStatment vm eenv (brkCond c)
                                   $ \vs -> do writeIORef (brkActive c)
                                                 $! Just
                                                 $! valueBool (trimResult1 vs)
                                               return nextStep
                              doStepMode dbg vm' nextStep' (StepOut mode)
              _ -> k
       _ -> k

  where
  atBreak = do setIdleReason dbg ReachedBreakPoint
               return (Running vm nextStep)






{- | Switch the debugger to the given execution mode.
The boolean indicates if we should block:  if it is 'True', then
this command will block until the execution stops.  If it is 'False',
then we return immediately. -}
startExec :: Debugger -> Bool -> StepMode -> IO ()
startExec dbg blocking mode =
  do mvar <- newEmptyMVar
     sendCommand (dbgCommand dbg) (StartExec mode mvar) True
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
whenIdle dbg io = sendCommand (dbgCommand dbg) (WhenIdle io) True
  -- The 'True' Is conservative, but generally if we are going to be doing
  -- something when we have to be idle, it probably affects the state.

{- | Execute an IO action as soon as the debugger is in a stable state.
This IO action should not block or take too long, otherwise the debugger
will be unresponsive.  Generally, the IO action should compute something
about the debugger state, or update some sort of internal state.  -}
whenStable :: Debugger -> Bool -> IO a -> IO a
whenStable dbg vis io =
  do mvar <- newEmptyMVar
     sendCommand (dbgCommand dbg) (TemporaryStop (putMVar mvar =<< io)) vis
     takeMVar mvar

--------------------------------------------------------------------------------









