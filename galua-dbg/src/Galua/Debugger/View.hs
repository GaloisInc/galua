{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, NamedFieldPuns #-}
module Galua.Debugger.View
  ( exportDebugger, exportFun, expandExportable, expandSubtable
  , watchExportable, unwatchExportable
  , exportV
  , importBreakLoc
  , exportBreakLoc
  , analyze
  ) where

import Galua.CObjInfo(CObjInfo(..))
import Galua.Debugger
import Galua.Debugger.PrettySource (omittedLine,lineToJSON)
import Galua.Debugger.Trie
import Galua.Debugger.View.Utils
import Galua.Mach
import Galua.MachUtils(VMState(..))
import Galua.Number
import Galua.LuaString
import Galua.Value
import Galua.FunValue
import Galua.DlInfo
import Galua.Names.Find(exprName)
import Galua.Names.Eval(nameInScope)
import Galua.Reference
import Galua.Debugger.Console
import Galua.Debugger.View.Analysis(exportResult)
import qualified Galua.Util.Table as Tab
import qualified Galua.Util.SizedVector as SV
import qualified Galua.Util.SmallVec as SMV
import Galua.Util.IOURef
import Galua.Code
import Galua.Debugger.CommandQueue
import Galua.Pretty(pp)

import qualified Galua.Micro.AST         as Analysis
import qualified Galua.Micro.Type.Primitives  as Analysis
import qualified Galua.Micro.Type.Import as Analysis
import qualified Galua.Micro.Type.Value  as Analysis
import qualified Galua.Micro.Type.Eval   as Analysis

import Language.Lua.StringLiteral (constructStringLiteral)

import qualified Data.Aeson as JS
import qualified Data.Aeson.Types as JS
import           Data.Aeson (toJSON, (.=))
import           Data.List(sortBy,nub,intersperse)
import           Data.Foldable(toList)
import           Data.Function(on)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as IOVector
import           Data.Maybe(fromMaybe)
import           Data.Either(partitionEithers)
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
import           Foreign.C.String
import           Foreign.Ptr (nullPtr, nullFunPtr)

import           HexFloatFormat (doubleToHex)


-- Max nubmber of entries in a table to export at once
tableChunk :: Int
tableChunk = 20

newtype ExportM a = ExportM (ReaderT (Maybe AllocRef)
                            (StateT ExportableState IO) a)
                    deriving (Functor,Applicative,Monad)

io :: IO a -> ExportM a
io = ExportM . inBase

-- | Assumes that we are holding the lock
runExportM :: Debugger -> ExportM a -> IO a
runExportM Debugger { dbgStateVM, dbgExportable } (ExportM m) =
  do s      <- readIORef dbgExportable
     mms    <- readIORef dbgStateVM
     let vm = case mms of
                Running vm' _  -> Just vm'
                RunningInC vm' -> Just vm'
                _              -> Nothing

     (a,s1) <- runStateT s $ runReaderT (vmAllocRef <$> vm) m
     writeIORef dbgExportable s1
     return a


newThing :: Exportable -> ExportM Integer
newThing x = ExportM $ sets $ \rw ->
  let i = expNextThing rw
  in ( i, rw { expNextThing = 1 + i
             , expClosed = Map.insert i x (expClosed rw)
             })

lookupThing :: Integer -> ExportM (Maybe Exportable)
lookupThing n = ExportM $ do ExportableState { expClosed } <- get
                             return (Map.lookup n expClosed)

setExpandedThread :: Reference Thread -> ExportM ()
setExpandedThread r = ExportM $ sets_ $ \rw ->
  rw { openThreads = Set.insert (referenceId r) (openThreads rw) }

getLiveExpandedThreads :: ExportM [Reference Thread]
getLiveExpandedThreads = ExportM $
  do mb <- ask
     case mb of
       Nothing ->
         sets $ \rw -> ([], rw { openThreads = Set.empty })

       Just aref ->
         do ids <- openThreads <$> get
            let lkp x = do mbA <- lookupRef aref x
                           return (case mbA of
                                     Nothing -> Left x
                                     Just a  -> Right a)
            eiths <- inBase (mapM lkp (Set.toList ids))
            let (bad,good) = partitionEithers eiths
            unless (null bad) $
              sets_ $ \rw -> rw { openThreads = Set.difference ids
                                                  (Set.fromList bad) }
            return good

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
     brkErr  <- readIOURef dbgBreakOnError
     idle    <- readIORef dbgIdleReason
     brks    <- exportBreaks dbg
     modifyIORef' dbgExportable $ \r -> newExportableState
                                          { openThreads = openThreads r }
     (jsWatches, jsSt, jsIdle, jsLines)
       <- runExportM dbg $
         do jsWatches <- traverse (exportValuePath funs)
                                  (watchListToList watches)
            jsSt      <- exportVMState funs st
            jsIdle <- exportIdleReason funs idle
            jsLines <- mapM (exportPrintedLine funs) outs
            return (jsWatches, jsSt, jsIdle, jsLines)

     cnt <- getCommandCount dbgCommand
     return $ JS.object [ "sources" .= exportSources
                                          (Map.toList (topLevelChunks funs))
                        , "breakPoints" .= brks
                        , "state"       .= jsSt
                        , "breakOnError".= brkErr
                        , "watches"     .= jsWatches
                        , "prints"      .= jsLines
                        , "idle"        .= jsIdle
                        , "stateCounter".= cnt
                        ]
  where Debugger { dbgSources, dbgExportable, dbgIdleReason,
                   dbgStateVM, dbgWatches, dbgBreakOnError,
                   dbgCommand } = dbg

exportV :: Debugger -> ValuePath -> Value -> IO JS.Value
exportV dbg path v =
  do chunks <- readIORef (dbgSources dbg)
     runExportM dbg (exportValue chunks path v)

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



exportPrintedLine :: Chunks -> Line -> ExportM JS.Value
exportPrintedLine funs Line { lineCount, lineBody, lineType } =
  do ws <- mapM (exportPrintedWord funs) lineBody
     return ( JS.object [ "num"   .= lineCount
                        , "words" .= ws
                        , "type"  .= (case lineType of
                                        InputLine  -> "input" :: Text
                                        OutputLine -> "output")
                        ] )

exportPrintedWord :: Chunks -> LineWord -> ExportM JS.Value
exportPrintedWord funs w =
  case w of
    TextWord t  -> return (JS.object [ tag "text", "word" .=  t ])
    ValueWord v -> do js <- exportValue funs VP_None v
                      return (JS.object [ tag "value", "value" .= js ])


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
                    do let menv = vmMachineEnv vm
                           globalTable = machGlobals menv
                       metas <- readIORef (machMetatablesRef menv)
                       (cid,gid,glob) <- Analysis.importClosure metas globalTable r
                       srcs <- readIORef (dbgSources dbg)
                       let funs = allMicroFuns srcs
                           args = Analysis.initLuaArgList
                           prims = Analysis.buildPrimMap gid glob
                           res  = Analysis.analyze funs prims cid args glob
                           txt  = show (pp res)
                       save "out" funs
                       writeFile "imported.txt" (show (cid, gid, glob))
                       writeFile "va.txt" txt
                       return $ Just $ exportResult res

                  _ -> return Nothing


           _ -> return Nothing

  where
  save pre x = sequence_
                    [ writeFile (dotFile pre fid)
                                (show $ Analysis.ppDot $ Analysis.functionCode fu) |
                                              (fid,fu) <- Map.toList x ]

  dotFile pre x = pre ++ "_" ++ funIdString x ++ ".dot"


unwatchExportable :: Debugger -> Int -> IO ()
unwatchExportable dbg n =
  whenStable dbg False $
    modifyIORef'  (dbgWatches dbg) (watchListRemove n)

watchExportable :: Debugger -> Integer -> IO (Maybe JS.Value)
watchExportable dbg n =
  whenStable dbg False $
     runExportM dbg $
       do mb <- lookupThing n
          case mb of
            Just (ExportableValue p _) ->
                do (funs,ip) <- io $ do ws <- readIORef dbgWatches
                                        let (i,ws1) = watchListExtend p ws
                                        writeIORef dbgWatches $! ws1
                                        funs <- readIORef dbgSources
                                        return (funs,(i,p))
                   Just <$> exportValuePath funs ip
            _ -> return Nothing
  where
  Debugger { dbgSources, dbgWatches } = dbg

expandExportable :: Debugger -> Integer -> IO (Maybe JS.Value)
expandExportable dbg n =
  whenStable dbg False $
  do funs <- readIORef dbgSources
     runExportM dbg $
       do mb   <- lookupThing n
          for mb $ \thing ->
            case thing of
              ExportableValue p v -> expandValue funs p v
              ExportableStackFrame pc env ->
                exportExecEnv funs pc (StackFrameExecEnv n) env

  where
  Debugger { dbgSources } = dbg



expandSubtable :: Debugger -> Integer -> Integer -> IO (Maybe JS.Value)
expandSubtable dbg n from =
  whenStable dbg False $
  do funs <- readIORef dbgSources
     runExportM dbg $
       do mb   <- lookupThing n
          case mb of
            Nothing -> return Nothing
            Just thing ->
              case thing of
                ExportableValue p (Table r) ->
                 Just <$> exportTable funs (fromInteger from) tableChunk p (referenceVal r)

                _ -> return Nothing

  where
  Debugger { dbgSources } = dbg




exportBreaks :: Debugger -> IO JS.Value
exportBreaks Debugger { dbgBreaks, dbgSources } =
  do funs <- readIORef dbgSources
     brks <- readIORef dbgBreaks
     return $ toJSON $ map (exportBreakLoc funs) $ Map.toList brks

exportBreakLoc :: Chunks -> ((Int,FunId),Maybe BreakCondition) -> JS.Value
exportBreakLoc funs ((op,fid),c) =
  JS.object [ "name" .= getFunctionName funs fid
            , "file" .= fmap unpackUtf8 (funcSource =<< fun)
            , "op"   .= op
            , "line" .= ((`lookupLineNumber` op) =<< fun)
            , "id"   .= exportBreakLocStr (op,fid)
            , "fid"  .= exportFID fid
            , "cond" .= fmap brkText c
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

exportValuePath :: Chunks -> (Int, ValuePath) -> ExportM JS.Value
exportValuePath funs (uid,path) =
  do v <- exportValue funs path . fromMaybe Nil =<< io (getValue path)
     return $ JS.object
       [ "name" .= showValuePath path
       , "id"   .= uid
       , "val"  .= v
       ]


exportVMState :: Chunks -> VMState -> ExportM JS.Value
exportVMState funs vms =
  case vms of

    FinishedOk vs       ->
      do js <- mapM (exportValue funs VP_None) (SMV.toList vs)
         return $ JS.object [ tag "finished", "result" .= js ]

    FinishedWithError v ->
      do js <- exportValue funs VP_None v
         return $ JS.object [ tag "error", "error"  .= js ]

    Running vm next ->
      do jsVM <- exportVM funs vm next
         return $ JS.object [ tag "running", "vm" .= jsVM ]

    RunningInC vm ->
      do jsVM <- exportVM funs vm WaitForC
         return $ JS.object [ tag "running", "vm" .= jsVM ]



exportValue :: Chunks -> ValuePath -> Value -> ExportM JS.Value
exportValue funs path val =
  case val of
    Nil         -> simple [ tag "nil", "text" .= str "nil" ]
    Bool b      -> simple [ tag "bool"
                          , "text" .= (if b then str "true" else "false") ]

    Number b    -> simple [ tag "number", "text" .= numberToString b
                                        , "alt"  .= hexNumber b ]
    String b'   -> let b = toByteString b'
                   in simple [ tag "string", "text" .= constructStringLiteral (L.fromStrict b)
                                           , "alt"  .= hexString b ]
    LightUserData p -> simple [ tag "light_user_data", "text" .= show p ]

    Table r    -> ref r [ tag "table", "text" .= prettyRef r ]

    Closure r  ->
      do let fs = exportFunctionValue funs (-1) (cloFun (referenceVal r))
         ref r (fs ++ [ tag "closure"
                      , "text" .= prettyRef r
                      , "id"   .= show (referenceId r)
                      ])

    UserData r ->
      do mbName <- io $ do mb <- readIORef (userDataMeta (referenceVal r))
                           maybe (return Nothing) lookupMetaName mb
         ref r [ tag "user_data", "text" .= prettyRef r, "name" .= mbName ]
    Thread r ->
      do stat <- io (getThreadField threadStatus r)
         ref r [ tag "thread"
               , "text"   .= prettyRef r
               , "status" .= exportThreadStatus stat ]
  where
  str x     = x :: String
  origin r  = Just (exportRefLoc funs (referenceLoc r))
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
    Table r    -> exportRef r (exportTable funs 0 tableChunk)
    UserData r -> exportRef r exportUserData
    Closure r  -> exportRef r (exportClosure funs)
    Thread r   -> do setExpandedThread r
                     exportThread funs Nothing r
    _          -> exportValue funs path val

  where exportRef r how = how path (referenceVal r)


exportThread :: Chunks -> Maybe NextStep -> Reference Thread -> ExportM JS.Value
exportThread funs mbNext th =
  do let eid = ThreadExecEnv (referenceId th)
     eenv  <- io $ getThreadField stExecEnv th
     stat  <- io $ getThreadField threadStatus th
     pc    <- case mbNext of
                Just (Goto pc) -> return pc
                _              -> io $ getThreadPC th
     hdlrs <- io $ getThreadField stHandlers th
     stack <- io $ getThreadField stStack th

     env   <- exportExecEnv funs pc eid eenv
     stackOut <- mapM (exportStackFrameShort funs) (toList stack)
     hs    <- mapM (exportHandler funs) hdlrs
     cur   <- exportCallStackFrameShort funs pc eenv mbNext
     return $ JS.object
                [ tag "thread"
                , "name"     .= prettyRef th
                , "status"   .= exportThreadStatus stat
                , "pc"       .= pc
                , "stack"    .= (cur : stackOut)
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



exportTable :: Chunks -> Int -> Int -> ValuePath -> Table -> ExportM JS.Value
exportTable funs from len path t =
  do ents  <- io (Tab.tableToList t)
     tabSz <- io (Tab.tableLen t)
     let sz       = max 0 (tabSz - from)
         visElems = take len $ drop from $ sortBy (compare `on` fst) ents
         missing  = max 0 (sz - len)
     js <- mapM entry visElems
     let pairs = [ tag "table", "values" .= js ]
     mb <- io (Tab.getTableMeta t)
     ref <- case mb of
              Table r  ->
                 do i <- newThing (ExportableValue (VP_MetaTable path)
                                                   (Table r))
                    return [ "text" .= prettyRef r, "ref" .= i ]
              _ -> return []

     return (JS.object ( [ "startIx" .= from
                         , "missing" .= missing ]
                         ++ pairs ++ ref
                       ))
  where
  entry (k,v) = do j1 <- exportValue funs (VP_Key path k) k
                   j2 <- exportValue funs (VP_Field path k) v
                   return (JS.object [ "key" .= j1, "value" .= j2 ])


exportUserData :: ValuePath -> UserData -> ExportM JS.Value
exportUserData path MkUserData { userDataMeta } =
  do mb <- io (readIORef userDataMeta)
     ref <- case mb of
              Nothing -> return []
              Just r  -> do i <- newThing (ExportableValue (VP_MetaTable path) (Table r))
                            return [ "text" .= prettyRef r, "ref" .= i ]
     return $ JS.object
            $ tag "user_data" : ref

exportClosure :: Chunks -> ValuePath -> Closure -> ExportM JS.Value
exportClosure funs path MkClosure { cloFun, cloUpvalues } =
  do (fs,uNames) <-
        case luaOpCodes cloFun of
          Just (_,func)->
            return ([ tag "lua-fun" ], debugInfoUpvalues (funcDebug func))


          _ -> return ([ tag "c-fun" ], Vector.empty)

     let exportU n v =
          do j <- exportValue funs (VP_CUpvalue path n) v
             return (JS.object [ "name" .= fmap unpackUtf8 (uNames Vector.!? n)
                               , "val"  .= j ])

     vs <- io $ fmap Vector.toList $ mapM readIORef =<< Vector.freeze cloUpvalues
     js <- zipWithM exportU [ 0 .. ] vs
     return $ JS.object $ ("upvalues" .= js) : fs




exportVM :: Chunks -> VM -> NextStep -> ExportM JS.Value
exportVM funs vm next =
  do t  <- exportThread funs (Just next) (vmCurThread vm)
     openTs <- do ids <- getLiveExpandedThreads
                  forM ids $ \r -> do js <- exportThread funs Nothing r
                                      return (Text.pack (prettyRef r), js)
     ts <- mapM (exportValue funs VP_None . Thread) (toList (vmBlocked vm))
     let menv = vmMachineEnv vm
     let actualRegistry = Table (machRegistry menv)
     registry <- exportValue funs (VP_Registry actualRegistry) actualRegistry
     return (JS.object [ "thread"   .= t
                       , "openThreads" .= JS.object openTs
                       , "blocked"  .= ts
                       , "registry" .= registry
                       ])



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
    CallFrame pc env _ _ -> exportCallStackFrameShort funs pc env Nothing

exportCallStackFrameShort :: Chunks -> Int -> ExecEnv -> Maybe NextStep -> ExportM JS.Value
exportCallStackFrameShort funs pc env mbnext =
  do ref <- newThing (ExportableStackFrame pc env)
     st  <- case env of
              ExecInC cenv -> io (readIORef (cExecApiCall cenv))
              ExecInLua {} -> return NoApiCall

     let phase = case mbnext of
                   Just ApiStart{}   -> ApiCallStarting
                   Just (ApiEnd res) -> ApiCallEnding res
                   _                 -> ApiCallRunning

     apiInfo <- case mbnext of
                  Just (ApiStart api _) -> exportApiCall api phase
                  _ -> case st of
                         NoApiCall -> pure []
                         ApiCallAborted api  -> exportApiCall api phase
                         ApiCallActive api   -> exportApiCall api phase
     return (JS.object ( apiInfo ++
                         tag "call"
                       : "ref"    .= ref
                       : exportFunctionValue funs pc (execFun env)))

data ApiCallPhase = ApiCallStarting | ApiCallRunning | ApiCallEnding (Maybe PrimArgument)

exportApiCall :: ApiCall -> ApiCallPhase -> ExportM [ JS.Pair ]
exportApiCall api phase =
  do args <- traverse exportPrimArg (apiCallArgs api)
     cresult <- case phase of
                  ApiCallEnding mb -> traverse exportPrimArg mb
                  _                -> return Nothing
     pure [ "method" .= apiCallMethod api
          , "args"   .= args
          , "phase"  .= case phase of
                          ApiCallStarting -> "starting" :: Text
                          ApiCallRunning  -> "running"
                          ApiCallEnding _ -> "ending"
          , "cresult" .= cresult
          , "return" .= JS.object (exportCObjInfo (apiCallReturn api))
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

exportCObjInfo :: CObjInfo -> [JS.Pair]
exportCObjInfo coi =
  [ "type" .= ("C"::Text)
  , "name" .= fromMaybe (cObjAddr coi) (cObjName coi)
  , "file" .= cObjFile coi
  , "line" .= cObjLine coi
  ]

exportFunctionValue :: Chunks -> Int -> FunctionValue -> [ JS.Pair ]
exportFunctionValue funs pc fun =
  case funValueName fun of
    CFID CFunName { cfunName } -> exportCObjInfo cfunName

    LuaFID fid ->
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
exportExecEnv :: Chunks -> Int -> ExecEnvId -> ExecEnv -> ExportM JS.Value
exportExecEnv funs pc eid eenv =
  case eenv of
    ExecInLua lenv -> exportLuaExecEnv funs pc eid lenv
    ExecInC cenv -> exportCExecEnv funs eid cenv


exportCExecEnv :: Chunks -> ExecEnvId -> CExecEnv -> ExportM JS.Value
exportCExecEnv funs _eid env@CExecEnv
    { cExecStack = execStack
    , cExecUpvals = execUpvals
    }  =
  do vs <- io $ do n <- SV.size execStack
                   traverse (SV.get execStack) [0..n-1]

     let eenv      = ExecInC env

     regVs <- zipWithM (exportNamed return (VP_Register eenv)) [ 0 .. ] vs
     uVs   <- zipWithM (exportNamed readIORef (VP_Upvalue eenv)) [ 0 .. ]
                    =<< io (Vector.toList <$> Vector.freeze execUpvals)

     return $ JS.object $ [ "registers" .= regVs
                          , "upvalues"  .= uVs
                          , "varargs"   .= ([] :: [String])
                          , "code"      .= (Nothing :: Maybe String)
                          ] ++ exportCObjInfo (cfunName (cExecFunction env))
  where
  exportNamed getVal pathCon n ref =
    do val <- io (getVal ref)
       vjs <- exportValue funs (pathCon n) val
       return $ JS.object [ "name" .= (Nothing :: Maybe String)
                          , "val"  .= vjs ]




-- | Assumes that we are paused, so reading the IO refs will give a consistent
-- view of the machine state.
exportLuaExecEnv :: Chunks -> Int -> ExecEnvId -> LuaExecEnv -> ExportM JS.Value
exportLuaExecEnv funs pc eid
  env@LuaExecEnv { luaExecRegs = execStack
                 , luaExecVarress = vrRef
                , luaExecUpvals = execUpvals
                , luaExecFID = fid
                , luaExecFunction = fun
                , luaExecVarargs = execVarargs } =

  do vs <- io $ do let n = IOVector.length execStack
                       upTo end =
                          forM [0..end-1] $ \i ->
                            readIORef =<< IOVector.unsafeRead execStack i

                   vrs <- readIORef vrRef
                   case vrs of
                     NoVarResults -> upTo n
                     VarResults (Reg c) vs -> (++ SMV.toList vs) <$> upTo c

     let code      = Just (exportFun funs (Just pc) (Just eid) fid)
         locNames  = lookupLocalName fun pc . Reg
         upNames x = debugInfoUpvalues (funcDebug fun) Vector.!? x
         eenv      = ExecInLua env

     regVs <- zipWithM (exportNamed (VP_Register eenv) locNames)
                       [ 0 .. ] vs

     uVs   <- zipWithM (exportNamed (VP_Upvalue eenv) upNames)
                       [ 0 .. ]
          =<< io (do v <- Vector.freeze execUpvals
                     v1 <- mapM readIORef v
                     return (Vector.toList v1))

     vAs <- zipWithM (\n -> named (VP_Varargs eenv n) Nothing) [0..] =<<
                                                    io (readIORef execVarargs)

     return $ JS.object $ [ "registers" .= regVs
                          , "upvalues"  .= uVs
                          , "varargs"   .= vAs
                          , "code"      .= code
                          ] ++ exportFunctionValue funs pc (LuaFunction fid fun)

  where
  named path nm v =
    do vjs <- exportValue funs path v
       return $ JS.object [ "name" .= (nm :: Maybe String)
                          , "val"  .= vjs ]

  exportNamed pathCon names n val =
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
  go (x : xs) f = go xs =<< funcNested f Vector.!? x


-- | Merge together the source lines of a function with their corresponding
-- opcodes.
-- XXX: Only works when there is debug info
exportFun :: Chunks -> Maybe Int -> Maybe ExecEnvId -> FunId -> Maybe JS.Value
exportFun funs mbPc mbEid fid0 =
  do fi@(topSrc,f0) <- lookupFun funs fid0
     let subs = subFunLines f0
         inScope nid = fromMaybe False $
                       do nm <- Map.lookup nid (srcNames topSrc)
                          return (nameInScope f0 mbPc (exprName nm))


     return $ JS.object
       [ "chunk"    .= getRoot fid0
       , "name"     .= getFunctionName funs fid0
       , "parent"   .= fmap exportFID (funIdParent fid0)
       , "context"  .= JS.object [ "pc"  .= mbPc
                                 , "eid" .= (exportExecEnvId <$> mbEid)
                                 ]
       , "lines" .=
           [ JS.object
              [ "line"    .= lNum
              , "text"    .= lineToJSON inScope l
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
              . funcNested

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

hexNumber :: Number -> String
hexNumber (Double d) = doubleToHex d
hexNumber (Int    i)
  | i < 0     = "-0x"++showHex (-i) ""
  | otherwise =  "0x"++showHex   i  ""
