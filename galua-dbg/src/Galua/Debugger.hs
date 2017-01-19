{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards, BangPatterns, OverloadedStrings #-}
module Galua.Debugger
  ( Debugger
  , newEmptyDebugger

  , getValue
  , setPathValue
  , resolveName

  , dbgSources
  , poll

  , addBreakPoint
  , removeBreakPoint
  , clearBreakPoints
  , setBreakOnError

  , executeStatement

  , goto
  , stepInto
  , stepOver
  , stepIntoLine
  , stepOverLine
  , stepOutOf
  , runNonBlock
  , run
  , pause
  ) where

import           Galua.Debugger.PrettySource (NameId)
import           Galua.Debugger.Options
import           Galua.Debugger.NameHarness
import           Galua.Debugger.Console(recordConsoleInput,recordConsoleValues)
import           Galua.Debugger.CommandQueue
import           Galua.Debugger.Types
import           Galua.Debugger.Execute

import           Galua.Code
import           Galua.Mach
import           Galua.Reference
import           Galua.Value
import           Galua.FunValue
import           Galua.Names.Eval
import           Galua.Names.Find(LocatedExprName(..),ppExprName)
import qualified Galua.Util.SizedVector as SV
import           Galua.Util.IOURef

import qualified Galua.Spec.AST as Spec
import qualified Galua.Spec.Parser as Spec

import           Data.Maybe (catMaybes,mapMaybe,maybeToList)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Word(Word64)
import           Data.Text(Text)
import qualified Data.Text as Text
import           Data.Text.Encoding(encodeUtf8)

import           Data.ByteString (ByteString)

import           Foreign (Ptr)
import           Data.Ord(comparing)
import           Data.List(minimumBy)
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as IOVector
import           Data.IORef

import           Control.Concurrent ( MVar, newEmptyMVar, takeMVar
                                    , killThread, ThreadId )
import           Control.Monad(when)
import           Control.Exception
import           MonadLib(ExceptionT,runExceptionT,raise,lift)
import           System.Timeout(timeout)




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

findNameResolveEnv :: Debugger -> MachineEnv -> ExecEnvId -> IO (FunId, NameResolveEnv)
findNameResolveEnv dbg menv eid =
  do metaTabs <- readIORef (machMetatablesRef menv)
     case eid of
       StackFrameExecEnv sid ->
         do ExportableState { expClosed } <- readIORef (dbgExportable dbg)
            case Map.lookup sid expClosed of
              Just (ExportableStackFrame _ env) ->
                execEnvToNameResolveEnv metaTabs env
              _ -> nameResolveException ("Invalid stack frame: " ++ show sid)

       ThreadExecEnv tid ->
         do mb <- lookupRef (machAllocRef menv) tid
            case mb of
              Nothing  -> nameResolveException "Invalid thread."
              Just ref ->
                do eenv <- getThreadField stExecEnv ref
                   execEnvToNameResolveEnv metaTabs eenv

       ClosureEnvId cid ->
         do mb <- lookupRef (machAllocRef menv) cid
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
  try $
  do (fid, resEnv) <- findNameResolveEnv dbg (dbgMachEnv dbg) eid
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

     dbgSources      <- newIORef chunks
     dbgWatches      <- newIORef watchListEmpty
     dbgBrkAddOnLoad <- newIORef (optBreakPoints opts)

     dbgExec         <- newExecState opts

     let query "port" = return (Number 8000)
         query _      = return Nil

         cfg = MachConfig
                 { machOnChunkLoad = addSourceFile dbgBrkAddOnLoad
                                                   (dbgBreaks dbgExec)
                                                   dbgSources
                 , machOnShutdown =
                     do a <- takeMVar threadVar
                        -- XXX: Free stable pointers?
                        killThread a
                 , machOnQuery    = query
                 , machRunner     = handleAPICall dbgExec
                 }

     dbgMachEnv <- newMachineEnv cfg
     let cptr = threadCPtr (machMainThreadRef dbgMachEnv)

     dbgExportable   <- newIORef newExportableState

     dbgDeclaredTypes <-
        (newIORef . makeGlobalTypeMap) =<<
          (do s <- Spec.specFromFile "lua.spec" -- XXX: search for specs, etc.
              return (Spec.specDecls s)
            `catch` \SomeException {} -> return [])

     let dbg = Debugger { dbgSources, dbgWatches, dbgExportable
                        , dbgBrkAddOnLoad, dbgDeclaredTypes
                        , dbgExec, dbgMachEnv
                        }

     return (cptr, dbg)



--------------------------------------------------------------------------------
-- Entry points


setPathValue :: Debugger -> Integer -> Value -> IO (Maybe ValuePath)
setPathValue dbg vid newVal =
  do ExportableState { expClosed } <- readIORef (dbgExportable dbg)
     case Map.lookup vid expClosed of
       Just (ExportableValue path _) ->
         do setValue path newVal
            return (Just path)
       _   -> return Nothing


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
  modifyIORef' (dbgStatus (dbgExec dbg)) $ \st ->
    case st of
      PausedInLua vm _ reason -> PausedInLua vm (Goto pc) reason
      _                       -> st


-- XXX: Don't block server while executing statement.
executeStatement :: Debugger -> Integer -> Text -> IO ()
executeStatement dbg frame statement = undefined{-
  whenIdle dbg True $
    do things <- expClosed <$> readIORef (dbgExportable dbg)
       case Map.lookup frame things of
         Just (ExportableStackFrame pc env) ->
           do whenInLua dbg () $ \vm next ->
                do recordConsoleInput statement
                   let stat = Text.unpack statement
                   (next',vm') <-
                      executeStatementInContext vm pc env stat $ \vs ->
                        Interrupt next <$ recordConsoleValues vs
                   writeIORef (dbgStateVM dbg) (Running vm' next')

              runNonBlock dbg

         _ -> return () -}


poll :: Debugger -> Word64 -> Int {- ^ Timeout in seconds -} -> IO Word64
poll dbg _ secs =
  do mvar <- newEmptyMVar
     sendDbgCommand dbg (AddClient mvar) False
     _ <- timeout (secs * 1000000) (takeMVar mvar)
     getCommandCount (dbgCommand (dbgExec dbg))

--------------------------------------------------------------------------------

setBreakOnError :: Debugger -> Bool -> IO ()
setBreakOnError dbg on = writeIOURef (dbgBreakOnError (dbgExec dbg)) on

addBreakPoint ::
  Debugger -> (Int,FunId) -> Maybe Text -> IO (Maybe BreakCondition)
addBreakPoint dbg loc txtCon =
  do mb <- case txtCon of
             Nothing  -> return Nothing
             Just txt -> prepareCondition dbg loc txt
     modifyIORef (dbgBreaks (dbgExec dbg)) (Map.insert loc mb)
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
         case (go rs . chunkFunction) =<< Map.lookup r (topLevelChunks chunks) of
           Nothing  -> fail "Invalid chunk."
           Just fun -> return fun
  where
  go path fun =
    case path of
      []     -> return fun
      x : xs -> go xs =<< (funcNested fun Vector.!? x)

removeBreakPoint :: Debugger -> (Int,FunId) -> IO ()
removeBreakPoint dbg loc =
  modifyIORef' (dbgBreaks (dbgExec dbg)) (Map.delete loc)

clearBreakPoints :: Debugger -> IO ()
clearBreakPoints dbg = writeIORef (dbgBreaks (dbgExec dbg)) Map.empty





sendDbgCommand :: Debugger -> DebuggerCommand -> Bool -> IO ()
sendDbgCommand dbg = sendCommand (dbgCommand (dbgExec dbg))





{- | Switch the debugger to the given execution mode.
The boolean indicates if we should block:  if it is 'True', then
this command will block until the execution stops.  If it is 'False',
then we return immediately. -}
startExec :: Debugger -> Bool -> StepMode -> IO ()
startExec dbg blocking mode =
  do mvar <- newEmptyMVar
     sendDbgCommand dbg (StartExec mode mvar) True
     when blocking (takeMVar mvar)










