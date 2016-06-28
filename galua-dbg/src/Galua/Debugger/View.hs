{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, NamedFieldPuns #-}
module Galua.Debugger.View
  ( exportDebugger, exportFun, expandExportable, watchExportable
  , importBreakLoc
  , exportBreakLoc
  , analyze
  ) where

import Galua.CObjInfo(CObjInfo(..))
import Galua.Debugger
import Galua.Debugger.PrettySource (omittedLine)
import Galua.Debugger.Trie
import Galua.Debugger.View.Utils
import Galua.Mach
import Galua.Number
import Galua.LuaString
import Galua.Value
import Galua.DlInfo
import qualified Galua.Table as Tab
import Galua.Reference
import Galua.Debugger.Console
import Galua.Debugger.View.Analysis(exportResult)
import qualified Galua.Stack as Stack
import qualified Galua.SizedVector as SV

import qualified Galua.Micro.AST         as Analysis
import qualified Galua.Micro.Type.Import as Analysis
import qualified Galua.Micro.Type.Value  as Analysis
import qualified Galua.Micro.Type.Eval   as Analysis
import qualified Galua.Micro.Type.Pretty as Analysis
import qualified Galua.Micro.Translate   as Analysis

import Language.Lua.Bytecode(Reg(..),Function(..),DebugInfo(..),OpCode(..),
                              ProtoIx(..))
import Language.Lua.Bytecode.FunId
import Language.Lua.Bytecode.Pretty (ppOpCode,blankPPInfo,pp)
import Language.Lua.Bytecode.Debug
                            (lookupLocalName,inferFunctionName,lookupLineNumber)
import Language.Lua.StringLiteral (constructStringLiteral)

import qualified Data.Aeson as JS
import qualified Data.Aeson.Types as JS
import           Data.Aeson (toJSON, (.=))
import           Data.List(intercalate,sortBy,nub,intersperse)
import           Data.Foldable(toList)
import           Data.Function(on)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as IOVector
import           Data.String(fromString)
import           Data.Maybe(fromMaybe)
import           Data.Traversable(for)
import           Data.Word(Word8)
import           Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Text.Read(readMaybe)
import           Data.IORef(readIORef,writeIORef,modifyIORef')
import           MonadLib
import           System.FilePath(splitFileName,splitPath,(</>))
import           Numeric (showHex)
import qualified System.Clock as Clock
import           Foreign.C.String
import           Foreign.Ptr (nullPtr, nullFunPtr)

newtype ExportM a = ExportM (StateT ExportableState IO a)
                    deriving (Functor,Applicative,Monad)

io :: IO a -> ExportM a
io = ExportM . inBase

-- | Assumes that we are holding the lock
runExportM :: Debugger -> ExportM a -> IO a
runExportM Debugger { dbgExportable } (ExportM m) =
  do s      <- readIORef dbgExportable
     (a,s1) <- runStateT s m
     writeIORef dbgExportable s1
     return a


newThing :: Exportable -> ExportM Integer
newThing x = ExportM $ sets $ \rw ->
  let i = expNextThing rw
  in ( i, rw { expNextThing = 1 + i
             , expClosed = Map.insert i x (expClosed rw) })

lookupThing :: Integer -> ExportM (Maybe Exportable)
lookupThing n = ExportM $ do ExportableState { expClosed } <- get
                             return (Map.lookup n expClosed)

--------------------------------------------------------------------------------

tag :: String -> JS.Pair
tag x = "tag" .= x


exportDebugger :: Debugger -> IO JS.Value
exportDebugger dbg =
  whenStable dbg False $
  do st      <- readIORef dbgStateVM
     funs    <- readIORef dbgSources
     outs    <- getConsoleLines
     watches <- readIORef dbgWatches
     brkErr  <- readIORef dbgBreakOnError
     idle    <- readIORef dbgIdleReason
     brks    <- exportBreaks dbg
     writeIORef dbgExportable newExportableState
     (jsWatches, jsSt, jsIdle)
       <- runExportM dbg $
         do jsWatches <- traverse (exportValuePath funs) (toList watches)
            jsSt      <- exportVMState funs st
            jsIdle <- exportIdleReason funs idle
            return (jsWatches, jsSt, jsIdle)

     return $ JS.object [ "sources" .= exportSources
                                          (Map.toList (topLevelChunks funs))
                        , "breakPoints" .= brks
                        , "state"       .= jsSt
                        , "breakOnError" .= brkErr
                        , "watches"     .= jsWatches
                        , "prints"      .= fmap exportPrintedLine outs
                        , "idle"        .= jsIdle
                        ]
  where Debugger { dbgSources, dbgExportable, dbgIdleReason,
                   dbgStateVM, dbgWatches, dbgBreakOnError } = dbg


exportIdleReason :: Chunks -> IdleReason -> ExportM JS.Value
exportIdleReason funs r =
  case r of
    Executing         -> simple "Executing"
    Ready             -> simple "Ready"
    ReachedBreakPoint -> simple "Breakpoint"
    ThrowingError val ->
      do js <- exportValue funs VP_None val
         return $ JS.object [ tag "Error thrown", "error" .= js ]

  where
  simple x = return (JS.object [ tag x ])



exportPrintedLine :: Line -> JS.Value
exportPrintedLine Line { lineCount, lineBody } =
  JS.object [ "num"   .= lineCount
            , "words" .= Text.split (=='\t') lineBody
            ]

exportSources :: [(Int,ChunkInfo)] -> JS.Value
exportSources = toJSON . map exportTree . groupSources getPath

  where
  getPath (_,info) =
    maybe "" unpackUtf8 (funcSource (chunkFunction info))

  exportTree t =
    case t of
      Node path ts -> JS.object [ tag "path"
                                , "name" .= path
                                , "subs" .= map exportTree ts
                                ]
      Leaf (nm,(n,info)) ->
        JS.object [ tag "chunk"
                  , "id"   .= n
                  , "name" .= nm
                  , "funs" .= exportChunkFuns n (chunkFunction info)
                  ]

groupSources :: (a -> FilePath) -> [a] -> [Tree FilePath (String,a)]
groupSources getPath = map (collapseTree (</>))
                     . trieToForest
                     . foldr add emptyTrie
  where
  add a trie = let (dir,file) = splitFileName (getPath a)
               in insertTrie (splitPath dir) (file,a) trie




--------------------------------------------------------------------------------

analyze :: Debugger -> Integer -> IO (Maybe JS.Value)
analyze dbg n =
  whenStable dbg False $
    runExportM dbg $
      do mb <- lookupThing n
         case mb of
           Just (ExportableValue _ (Closure r)) ->
             io $
             do vms <- readIORef (dbgStateVM dbg)
                case vms of
                  Running vm _ ->
                    do let metas = machMetatablesRef (vmMachineEnv vm)
                       (cid,glob) <- Analysis.importMainClosure metas r
                       srcs <- readIORef (dbgSources dbg)
                       let funs = expandSources (topLevelChunks srcs)
                           args = Analysis.initLuaArgList
                           res  = Analysis.analyze funs cid args glob
                           txt  = show $ pp blankPPInfo res
                       save "out" funs
                       writeFile "imported.txt" (show glob)
                       writeFile "va.txt" txt
                       putStrLn txt
                       return $ Just $ exportResult res

                  _ -> return Nothing


           _ -> return Nothing

  where
  expandSources     = foldr expandTop Map.empty . Map.toList
  expandTop (r,f) m = Analysis.translateAll (rootFun r) (chunkFunction f) m

  save pre x = sequence_
                    [ writeFile (dotFile pre fid)
                                (show $ Analysis.ppDot $ Analysis.functionCode fu) |
                                              (fid,fu) <- Map.toList x ]

  dotFile pre x = pre ++ "_" ++ funIdString x ++ ".dot"


watchExportable :: Debugger -> Integer -> IO (Maybe JS.Value)
watchExportable dbg n =
  whenStable dbg False $
     runExportM dbg $
       do mb <- lookupThing n
          case mb of
            Just (ExportableValue p _) ->
                do io $ modifyIORef' dbgWatches $ \watches -> watches Seq.|> p
                   return (Just (JS.object ["path" .= showValuePath p]))
            Just (ExportableStackFrame {}) -> return Nothing
            Nothing -> return Nothing
  where
  Debugger { dbgWatches } = dbg

expandExportable :: Debugger -> Integer -> IO (Maybe JS.Value)
expandExportable dbg n =
  whenStable dbg False $
  do funs <- readIORef dbgSources
     runExportM dbg $
       do mb   <- lookupThing n
          for mb $ \thing ->
            case thing of
              ExportableValue p v -> expandValue funs p v
              ExportableStackFrame pc env -> exportExecEnv funs pc env

  where
  Debugger { dbgSources } = dbg


exportBreaks :: Debugger -> IO JS.Value
exportBreaks Debugger { dbgBreaks, dbgSources } =
  do funs <- readIORef dbgSources
     brks <- readIORef dbgBreaks
     return $ toJSON $ map (exportBreakLoc funs) $ Set.toList brks

exportBreakLoc :: Chunks -> (Int,FunId) -> JS.Value
exportBreakLoc funs (op,fid) =
  JS.object [ "name" .= getFunctionName funs fid
            , "file" .= fmap unpackUtf8 (funcSource =<< fun)
            , "op"   .= op
            , "line" .= ((`lookupLineNumber` op) =<< fun)
            , "id"   .= exportBreakLocStr (op,fid)
            , "fid"  .= exportFID fid
            ]
  where
  fun = fmap snd (lookupFun funs fid)

exportBreakLocStr :: (Int,FunId) -> String
exportBreakLocStr (op,fid) = funIdString fid ++ "-" ++ show op

importBreakLoc :: String -> Maybe (Int,FunId)
importBreakLoc txt =
  case break (== '-') txt of
    (as,_:bs) -> do fid <- funIdFromString as
                    op  <- readMaybe bs
                    return (op,fid)
    _ -> Nothing

exportValuePath :: Chunks -> ValuePath -> ExportM JS.Value
exportValuePath funs path =
  do v <- exportValue funs path . fromMaybe Nil =<< io (getValue path)
     return $ JS.object
       [ "name" .= showValuePath path
       , "val"  .= v
       ]


exportVMState :: Chunks -> VMState -> ExportM JS.Value
exportVMState funs vms =
  case vms of

    FinishedOk vs       ->
      do js <- mapM (exportValue funs VP_None) vs
         return $ JS.object [ tag "finished", "result" .= js ]

    FinishedWithError v ->
      do js <- exportValue funs VP_None v
         return $ JS.object [ tag "error", "error"  .= js ]

    Running vm next ->
      do jsVM <- exportVM funs vm next
         return $ JS.object [ tag "running", "vm" .= jsVM ]



exportValue :: Chunks -> ValuePath -> Value -> ExportM JS.Value
exportValue funs path val =
  case val of
    Nil         -> simple [ tag "nil", "text" .= str "nil" ]
    Bool b      -> simple [ tag "bool"
                          , "text" .= (if b then str "true" else "false") ]

    Number b    -> simple [ tag "number", "text" .= numberToString b ]
    String b'   -> let b = toByteString b'
                   in simple [ tag "string", "text" .= constructStringLiteral (L.fromStrict b)
                                           , "alt"  .= hexString b ]
    LightUserData p -> simple [ tag "light_user_data", "text" .= show p ]

    Table r    -> ref r [ tag "table", "text" .= prettyRef r ]

    Closure r  ->
      do MkClosure { cloFun } <- io (readRef r)
         let fs = exportFunctionValue funs (-1) cloFun
         ref r (fs ++ [ tag "closure",     "text" .= prettyRef r ])

    UserData r ->
      do mbName <- io $ do MkUserData{ userDataMeta } <- readRef r
                           maybe (return Nothing) lookupMetaName userDataMeta
         ref r [ tag "user_data", "text" .= prettyRef r, "name" .= mbName ]
    Thread r ->
      do thread <- io (readRef r)
         ref r [ tag "thread"
               , "text"   .= prettyRef r
               , "status" .= exportThreadStatus (threadStatus thread) ]
  where
  str x     = x :: String
  origin r  = Just (exportRefLoc funs (getRefLoc r))
  simple a  = struct (Nothing :: Maybe ()) a

  ref r     = struct (origin r)
  struct mb xs =
    do addr <- newThing (ExportableValue path val)
       return (JS.object ("ref" .= addr : "origin" .= mb : xs))


lookupMetaName :: Reference Table -> IO (Maybe String)
lookupMetaName tab =
  do key <- fromByteString "__name"
     nameVal <- getTableRaw tab (String key)
     return $! case nameVal of
       String name -> Just (unpackUtf8 (toByteString name))
       _           -> Nothing


expandValue :: Chunks -> ValuePath -> Value -> ExportM JS.Value
expandValue funs path val =
  case val of
    Table r    -> exportRef r (exportTable funs)
    UserData r -> exportRef r exportUserData
    Closure r  -> exportRef r (exportClosure funs)
    Thread r   -> exportThread funs Nothing r
    _          -> exportValue funs path val

  where exportRef r how = how path =<< io (readRef r)


exportThread :: Chunks -> Maybe NextStep -> Reference Thread -> ExportM JS.Value
exportThread funs mbNext tRef =
  do MkThread { stPC, stStack , stExecEnv, stHandlers,
                    threadStatus } <- io (readRef tRef)
     let curPC = case mbNext of
                   Just (Goto x) -> x
                   _             -> stPC
     env   <- exportExecEnv funs curPC stExecEnv
     stack <- mapM (exportStackFrameShort funs) (toList stStack)
     hs    <- mapM (exportHandler funs) stHandlers
     cur   <- exportCallStackFrameShort funs curPC stExecEnv
     return $ JS.object
                [ tag "thread"
                , "name"     .= prettyRef tRef
                , "status"   .= exportThreadStatus threadStatus
                , "pc"       .= curPC
                , "stack"    .= (cur : stack)
                , "handlers" .= hs
                , "env"      .= env
                ]


exportThreadStatus :: ThreadStatus -> String
exportThreadStatus st =
  case st of
    ThreadSuspended _ -> "suspended"
    ThreadNew         -> "new"
    ThreadRunning     -> "running"
    ThreadNormal _    -> "normal"
    ThreadCrashed     -> "crashed"



exportTable :: Chunks -> ValuePath -> Table -> ExportM JS.Value
exportTable funs path t =
  do js <- (mapM entry . sortBy (compare `on` fst)) =<< io (Tab.tableToList t)
     let pairs = [ tag "table", "values" .= js ]
     mb <- io (Tab.getTableMeta t)
     ref <- case mb of
              Table r  ->
                 do i <- newThing (ExportableValue (VP_MetaTable path)
                                                   (Table r))
                    return [ "text" .= prettyRef r, "ref" .= i ]
              _ -> return []

     return (JS.object (pairs ++ ref))
  where
  entry (k,v) = do j1 <- exportValue funs (VP_Key path k) k
                   j2 <- exportValue funs (VP_Field path k) v
                   return (JS.object [ "key" .= j1, "value" .= j2 ])


exportUserData :: ValuePath -> UserData -> ExportM JS.Value
exportUserData path MkUserData { userDataMeta } =
  do ref <- case userDataMeta of
              Nothing -> return []
              Just r  -> do i <- newThing (ExportableValue (VP_MetaTable path) (Table r))
                            return [ "text" .= prettyRef r, "ref" .= i ]
     return $ JS.object
            $ tag "user_data" : ref

exportClosure :: Chunks -> ValuePath -> Closure -> ExportM JS.Value
exportClosure funs path MkClosure { cloFun, cloUpvalues } =
  do (fs,uNames) <-
        case cloFun of
          LuaFunction _ func ->
            return ([ tag "lua-fun" ], debugInfoUpvalues (funcDebug func))


          CFunction _ -> return ([ tag "c-fun" ], Vector.empty)

     let exportU n v =
          do j <- exportValue funs (VP_CUpvalue path n) v
             return (JS.object [ "name" .= fmap unpackUtf8 (uNames Vector.!? n)
                               , "val"  .= j ])

     vs <- io $ mapM readIORef cloUpvalues
     js <- zipWithM exportU [ 0 .. ] (Vector.toList vs)
     return $ JS.object $ ("upvalues" .= js) : fs




exportVM :: Chunks -> VM -> NextStep -> ExportM JS.Value
exportVM funs vm next =
  do t  <- exportThread funs (Just next) (vmCurThread vm)
     ts <- mapM (exportValue funs VP_None . Thread) (toList (vmBlocked vm))
     stats <- io $ exportProfilingStats funs $ machProfiling $ vmMachineEnv vm
     return (JS.object [ "thread" .= t, "blocked" .= ts
                       , "stats"  .= stats
                       ])

exportProfilingStats :: Chunks -> ProfilingInfo -> IO JS.Value
exportProfilingStats funs info =
  do calls  <- readIORef (profCallCounters info)
     allocs <- readIORef (profAllocCounters info)
     times  <- readIORef (profFunctionTimers info)
     return $ JS.object
        [ "calls"  .= [ JS.object
                          [ "loc"   .= exportFunName funs f
                          , "calls" .= n
                          , "cum"   .= exportTimeSpec (runtimeCumulative rts)
                          , "ind"   .= exportTimeSpec (runtimeIndividual rts) ]
                      | (f,(n,rts)) <- Map.toList (Map.intersectionWith (,) calls times)]
        , "allocs" .= [ JS.object
                          [ "loc"   .= exportCodeLoc funs l
                          , "calls" .= n ]
                      | (l,n) <- Map.toList allocs]
        ]

-- | Export a timespec in seconds
exportTimeSpec :: Clock.TimeSpec -> Double
exportTimeSpec t = fromInteger (Clock.toNanoSecs t) / 1e9


exportHandler :: Chunks -> HandlerType -> ExportM JS.Value
exportHandler funs h =
  case h of

    FunHandler ref ->
      do v <- exportValue funs VP_None (Closure ref)
         return (JS.object [ tag "handler", "value" .= v ])

    DefaultHandler ->
      return (JS.object [ tag "default" ])


exportStackFrameShort :: Chunks -> StackFrame -> ExportM JS.Value
exportStackFrameShort funs sf =
  case sf of
    ErrorFrame -> return (JS.object [ tag "throw_error" ])
    CallFrame pc env _ _ -> exportCallStackFrameShort funs pc env

exportCallStackFrameShort :: Chunks -> Int -> ExecEnv -> ExportM JS.Value
exportCallStackFrameShort funs pc env =
  do ref <- newThing (ExportableStackFrame pc env)
     st  <- io (readIORef (execApiCall env))
     apiInfo <- case st of
                  NoApiCall -> pure []
                  ApiCallAborted api  -> exportApiCall api
                  ApiCallActive api _ -> exportApiCall api
     return (JS.object ( apiInfo ++
                         tag "call"
                       : "ref" .= ref
                       : exportFunctionValue funs pc (execFunction env)))

exportApiCall :: ApiCall -> ExportM [ JS.Pair ]
exportApiCall api =
  do args <- traverse exportPrimArg (apiCallArgs api)
     pure [ "method" .= apiCallMethod api
          , "args"   .= args
          ]



exportPrimArg :: PrimArgument -> ExportM JS.Value
exportPrimArg a =
  case a of
    PrimIntArg i     -> pure $ JS.object [ tag "int", "text" .= show i]
    PrimDoubleArg d  -> pure $ JS.object [ tag "double", "text" .= show d]
    PrimCIntArg p    -> pure $ JS.object [ tag "ptr", "text" .= show p]
    PrimCSizeArg p   -> pure $ JS.object [ tag "ptr", "text" .= show p]

    PrimPtrArg p | p == nullPtr ->
      pure $ JS.object [ tag "ptr", "text" .= ("NULL"::Text)]
    PrimPtrArg p     -> pure $ JS.object [ tag "ptr", "text" .= show p]

    PrimFunPtrArg p
      | p == nullFunPtr ->
         pure $ JS.object [ tag "funptr", "text" .= ("NULL"::Text)]
    PrimFunPtrArg p  ->
      do mb <- io (funPtrInfo p)
         let txt = fromMaybe (show p)
                 $ do info <- mb
                      name <- dlinfoSymName info
                      let o = dlinfoSymOffset info
                      return $ if o == 0
                             then name
                             else name++'+':show o
         pure $ JS.object [ tag "funptr", "text" .= txt]

    PrimCStringArg p | p == nullPtr ->
         pure $ JS.object [ tag "string", "text" .= ("NULL"::Text)]

    PrimCStringArg p ->
      do txt <- io (peekCString p)
         pure $ JS.object [ tag "string", "text" .= exportString txt]

    PrimCStringLenArg (p,_) | p == nullPtr ->
         pure $ JS.object [ tag "string", "text" .= ("NULL"::Text)]

    PrimCStringLenArg p ->
      do txt <- io (peekCStringLen p)
         pure $ JS.object [ tag "string", "text" .= exportString txt]

  where
    exportString
      = constructStringLiteral
      . L.fromStrict
      . Text.encodeUtf8
      . Text.pack

exportChunkFuns :: Int -> Function -> [JS.Value]
exportChunkFuns chunkId fun0 =
  [ JS.object [ "id" .= exportFID fid, "name" .= nm ]
                              | (fid,nm) <- computeFunNames chunkId fun0 ]



exportFunName :: Chunks -> FunName -> JS.Value
exportFunName funs cl =
  JS.object $
  case cl of
    CFID CFunName { cfunName } ->
      [ "type" .= str "C"
      , "name" .= fromMaybe (cObjAddr cfunName) (cObjName cfunName)
      , "file" .= cObjFile cfunName
      , "line" .= cObjLine cfunName
      ]

    LuaFID fid ->
      let vn = Map.lookup fid (allFunNames funs)
      in
      [ "type" .= str "Lua"
      , "name" .= (renderVisName <$> vn)
      , "file" .= join (funVisFile <$> vn)
      , "fid"  .= exportFID fid
      ]

  where str x = x :: String



exportCodeLoc :: Chunks -> CodeLoc -> JS.Value
exportCodeLoc funs cl =
  JS.object $
  case cl of
    InC CFunName { cfunName } ->
      [ "type" .= str "C"
      , "name" .= fromMaybe (cObjAddr cfunName) (cObjName cfunName)
      , "file" .= cObjFile cfunName
      , "line" .= cObjLine cfunName
      ]

    InLua fid pc ->
      let vn = Map.lookup fid (allFunNames funs)
      in
      [ "type" .= str "Lua"
      , "name" .= (renderVisName <$> vn)
      , "file" .= join (funVisFile <$> vn)
      , "pc"   .= pc
      , "fid"  .= exportFID fid
      ]

    MachSetup ->
      [ "type" .= str "(system)"
      ]

  where str x = x :: String




exportRefLoc :: Chunks -> RefLoc -> JS.Value
exportRefLoc funs = exportCodeLoc funs . refLocSite



exportFunctionValue :: Chunks -> Int -> FunctionValue -> [ JS.Pair ]
exportFunctionValue funs pc fun =
  case fun of
    CFunction CFunName { cfunName } ->
      [ "type" .= str "C"
      , "name" .= fromMaybe (cObjAddr cfunName) (cObjName cfunName)
      , "file" .= cObjFile cfunName
      , "line" .= cObjLine cfunName
      ]

    LuaFunction fid _ ->
      let vn = Map.lookup fid (allFunNames funs)
      in
      [ "type" .= str "Lua"
      , "name" .= (renderVisName <$> vn)
      , "file" .= join (funVisFile <$> vn)
      , "pc"   .= pc
      , "fid"  .= exportFID fid
      ]

  where str x = x :: String

renderVisName :: FunVisName -> Text
renderVisName f = fromMaybe (locName f) (funVisName f)
  where
  locName FunVisName { funVisLineStart, funVisLineEnd } =
    Text.pack ("function at " ++ show funVisLineStart ++ "--" ++
                                 show funVisLineEnd)

getFunctionName :: Chunks -> FunId -> Maybe Text
getFunctionName funs fid = renderVisName <$> Map.lookup fid (allFunNames funs)


-- | Assumes that we are paused, so reading the IO refs will give a consistent
-- view of the machine state.
exportExecEnv :: Chunks -> Int -> ExecEnv -> ExportM JS.Value
exportExecEnv funs pc
  env@ExecEnv { execStack, execUpvals
              , execFunction, execVarargs } =

  do vs <- io $ do n <- SV.size execStack
                   traverse (SV.get execStack) [0..n-1]

     let noCode = (Nothing, \_ -> Nothing, \_ -> Nothing)
         (code,locNames,upNames) =
            case execFunction of
              LuaFunction fid fun ->
                ( Just (exportFun funs fid)
                , lookupLocalName fun pc . Reg
                , \x -> debugInfoUpvalues (funcDebug fun) Vector.!? x
                )
              CFunction{}       -> noCode


     regVs <- zipWithM (exportNamed (VP_Register env) locNames)
                       [ 0 .. ] vs
     uVs   <- zipWithM (exportNamed (VP_Upvalue env) upNames)
                       [ 0 .. ] (Vector.toList execUpvals)

     vAs <- zipWithM (\n -> named (VP_Varargs env n) Nothing) [0..] =<< io (readIORef execVarargs)
     return $ JS.object $ [ "registers" .= regVs
                          , "upvalues"  .= uVs
                          , "varargs"   .= vAs
                          , "code"      .= code
                          ] ++ exportFunctionValue funs pc execFunction

  where
  named path nm v =
    do vjs <- exportValue funs path v
       return $ JS.object [ "name" .= (nm :: Maybe String)
                                     , "val"  .= vjs ]

  exportNamed pathCon names n ref =
    do val <- io $ readIORef ref
       named (pathCon n) (fmap unpackUtf8 (names n)) val



-- | Returns the specific function, and the source for the whole file.
lookupFun :: Chunks -> FunId -> Maybe (Source,Function)
lookupFun funs fid =
  case funIdList fid of
    [] -> Nothing
    x : xs -> do info <- Map.lookup x (topLevelChunks funs)
                 f <- go xs (chunkFunction info)
                 return (chunkSource info,f)

  where
  go [] f       = return f
  go (x : xs) f = go xs =<< funcProtos f Vector.!? x

funIdParent :: FunId -> Maybe FunId
funIdParent (FunId []) = Nothing
funIdParent (FunId (_:xs)) = Just (FunId xs)

-- | Merge together the source lines of a function with their corresponding
-- opcodes.
-- XXX: Only works when there is debug info
exportFun :: Chunks -> FunId -> Maybe JS.Value
exportFun funs fid0 =
  do fi@(_,f0) <- lookupFun funs fid0
     let subs = subFunLines f0

     return $ JS.object
       [ "chunk" .= getRoot fid0
       , "name"  .= getFunctionName funs fid0
       , "parent" .= fmap exportFID (funIdParent fid0)
       , "lines" .=
           [ JS.object
              [ "line"    .= lNum
              , "text"    .= l
              , "opcodes" .= map exportOp (Map.toList ops)
              ] | (lNum,(l,ops)) <- insertOmitted $
                                    Map.toList $
                                    addFunOpCodes subs (fid0,f0) $
                                    blankLines subs fi ]
       ]
  where
  insertOmitted ls =
    case ls of
      [] -> []
      (l1,x) : more -> (show l1, x) : dots ++ insertOmitted more
        where
        dots = case more of
                 [] -> []
                 (l2,_) : _ ->
                   let diff = l2 - l1
                   in if diff == 1 then [] else
                      if diff == 2 then [ (show (l1 + 1), omit) ] else
                        [ (show (l1 + 1) ++ ".." ++ show (l2 - 1), omit) ]
        omit = (omittedLine, Map.empty)



  exportOp ((pth,opc),txt) = JS.object [ "path"   .= exportFID pth
                                       , "opcode" .= opc
                                       , "text"   .= txt
                                       ]

  -- Compute the ranges of direct subfunctions, sorted by start line.
  subFunLines = sortBy (compare `on` fst)
              . map (\p -> (funcLineDefined p, funcLastLineDefined p))
              . Vector.toList
              . funcProtos

  -- If an opcode ends up in the range of a subfunction, say that it is
  -- at the beginning of the function.  This could happen for opcodes that
  -- have to do with creating the funciton.
  opLine subs x =
    case subs of
      []                       -> x
      (from,to) : more
        | x < from             -> x
        | x >= from && x <= to -> from
        | otherwise            -> opLine more x

  blankLines subs fi@(src,_) =
    Map.fromList
      [ (l, (fromMaybe omittedLine (srcLines src Vector.!? (l-1)), Map.empty))
                            | l <- nub $ map (opLine subs) (srcLineNums fi) ]

  srcLineNums (src,f0)
    -- The main function is special
    | funcLineDefined f0 == 0 =
        let n = Vector.foldl' max (Vector.length (srcLines src))
                   (debugInfoLines (funcDebug f0)) -- handles empty debug info
        in [ 1 .. n  ]
    | otherwise               = [ funcLineDefined f0 .. funcLastLineDefined f0 ]


  addFunOpCodes subs (pth,fun) m =
    foldr (mkOpCode subs (pth,fun)) m [ 0 .. Vector.length (funcCode fun) - 1 ]

  mkOpCode subs (pth,fun) i = Map.adjust
                           (addOp (prepOp (pth,fun) i))
                           (opLine subs
                              (debugInfoLines (funcDebug fun) Vector.! i))

  addOp (k,o) ((ln,ops))  = (ln,Map.insert k o ops)

  prepOp (pth,fun) i   = ((pth,i), show (ppOpCode fun i))


hexString :: ByteString -> Maybe String
hexString bs0
  | B.null bs0 = Nothing
  | otherwise    = Just (combine (intersperse (showChar ' ') (aux bs0)) "")
  where
  combine = foldr (.) id

  aux :: ByteString -> [ShowS]
  aux bs
    | B.null bs = []
    | otherwise = combine (showByte <$> B.unpack a) : aux b
    where
    (a,b) = B.splitAt 4 bs

showByte :: Word8 -> ShowS
showByte x
  | x < 16 = showChar '0' . showHex x
  | otherwise = showHex x
