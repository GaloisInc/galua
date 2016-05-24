{-# LANGUAGE OverloadedStrings, NamedFieldPuns, TemplateHaskell, CPP #-}
module Galua.Debugger.Server () where

import           Language.Lua.Bytecode.FunId

import           Galua.Debugger.Options(Options(..), defaultOptions)
import           Galua.Debugger
import           Galua.Debugger.View
                  (exportDebugger,watchExportable,expandExportable,
                      exportFun,importBreakLoc,exportBreakLoc)
import           Galua.Debugger.EmbedDirectory (embedDirectory)
import qualified Galua.Value as G
import           Galua.Number(parseNumber)
import           Galua.LuaString

import           Config
import           Config.Lens
import           Config.GetOpt

import           Snap.Http.Server (httpServe)
import           Snap.Http.Server.Config (Config, optDescrs, defaultConfig
                                         , setPort, getPort )
import           Snap.Util.FileServe(serveDirectory, defaultMimeTypes)
import           Snap.Core (Snap)
import qualified Snap.Core as Snap
import           Control.Monad.IO.Class(liftIO)

import           Control.Applicative((<|>))
import           Control.Monad(unless,void)
import           Control.Exception (throwIO, catch)
import           Control.Concurrent(forkIO)
import           Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.HashMap.Strict as HashMap
import           Data.IORef
import           Data.List
import           Data.Monoid
import           Data.Functor.Constant
import           Data.Foldable
import           Data.Text (Text)
import           Data.Text.Read(decimal)
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Text.Encoding(decodeUtf8With)
import           Data.Text.Encoding.Error(lenientDecode)
import           Data.Maybe(fromMaybe)
import qualified Data.Aeson as JS
import           Text.Read(readMaybe)
import           System.Environment (getProgName)
import           System.IO (hPutStrLn, stderr)
import           System.IO.Error (isDoesNotExistError)
import           System.FilePath (takeExtension, takeDirectory, (</>))
import           System.Console.GetOpt

import           Foreign(Ptr,nullPtr)
import           Foreign.C(CInt(..))



foreign export ccall "galua_newstate_dbg"
  startServerC :: CInt -> IO (Ptr ())

startServerC :: CInt {- offset for port -} -> IO (Ptr ())
startServerC portOffset = startServer (fromIntegral portOffset)

startServer :: Int -> IO (Ptr ())
startServer portOffset =
  do config      <- getConfig
     (cptr, dbg) <- newEmptyDebugger (dbgGaluaOptions config)
     st          <- newIORef dbg

     let routes = apiRoutes st
         snapConfig = incrementPort portOffset
                    $ dbgSnapConfig config

     void $ forkIO $ httpServe snapConfig
            $ Snap.route routes
          <|> serveDirectory "ui"
          <|> Snap.route [(path, sendFileBytes (B8.unpack path) content)
                                             | (path,content) <- staticContent]
          <|> Snap.path "" (Snap.redirect "index.html")

     runNonBlock dbg

     return cptr



staticContent :: [(ByteString, ByteString)]
staticContent =
  $(let p :: FilePath -> Bool
        p fp = any (`isSuffixOf` fp)
                   [".woff", ".woff2", ".ttf", ".otf",
                    ".png", ".min.js", ".min.css", ".html",
                    "thread.js", "values.js", "debugger.js", "code.js", "code.css"]
    in embedDirectory p (takeDirectory __FILE__ </>
                            ".." </> ".." </>
                            ".." </> ".." </> "ui"))

apiRoutes :: IORef Debugger -> [(ByteString, Snap ())]
apiRoutes st =
  let cmd f   = f =<< liftIO (readIORef st)
      ioCmd f = liftIO (f =<< readIORef st)
  in [ ("/function", cmd snapGetFunction)
     , ("/view",     cmd snapGetState)
     , ("/poll",     cmd snapPoll)

     , ("/step",         ioCmd stepInto)
     , ("/stepOver",     ioCmd stepOver)
     , ("/stepLine",     ioCmd stepIntoLine)
     , ("/stepOverLine", ioCmd stepOverLine)
     , ("/stepOutOf",    ioCmd stepOutOf)

     , ("/run",          ioCmd run)
     , ("/pause",        ioCmd pause)

     , ("/addBreakPoint",    cmd snapAddBreakPoint)
     , ("/removeBreakPoint", cmd snapRemoveBreakPoint)
     , ("/clearBreakPoints", ioCmd clearBreakPoints)

     , ("/goto", cmd snapGoto)

     , ("/expand", cmd snapExpand)
     , ("/watch", cmd snapWatch)

     , ("/breakOnErr", cmd snapBreakOnError)

     , ("/setValue", cmd snapSetValue)

     -- , ("/analyze", cmd snapAnalyze)

     ]

sendFileBytes :: FilePath -> ByteString -> Snap ()
sendFileBytes name bs =
  do for_ (HashMap.lookup (takeExtension name) defaultMimeTypes)
          (Snap.modifyResponse . Snap.setContentType)
     Snap.writeBS bs

snapBreakOnError :: Debugger -> Snap ()
snapBreakOnError dbg =
  do on <- boolParam "enabled"
     liftIO (writeIORef (dbgBreakOnError dbg) on)

snapWatch :: Debugger -> Snap ()
snapWatch dbg =
  do n  <- natParam "id"
     mb <- liftIO (watchExportable dbg n)
     case mb of
       Nothing -> notFound
       Just js -> sendJSON js

snapSetValue :: Debugger -> Snap ()
snapSetValue dbg =
  do n   <- natParam "id"
     txt <- bsParam "value"
     val <- case txt of
              "nil"   -> return G.Nil
              "true"  -> return (G.Bool True)
              "false" -> return (G.Bool False)
              _ | Just n' <- parseNumber (B8.unpack txt) -> return (G.Number n')
                -- XXX: Currently we parse these as Haskell strings...
              _ | Just s <- readMaybe (B8.unpack txt) -> liftIO (G.String <$> fromByteString (B8.pack s))
                | otherwise -> badInput "Invalid value"
     liftIO (setPathValue dbg n val)
     snapGetState dbg

snapExpand :: Debugger -> Snap ()
snapExpand dbg =
  do n  <- natParam "id"
     mb <- liftIO (expandExportable dbg n)
     case mb of
       Nothing -> notFound
       Just js -> sendJSON js




snapGoto :: Debugger -> Snap ()
snapGoto st =
  do txt <- textParam "loc"
     case importBreakLoc (Text.unpack txt) of
       Nothing        -> badInput "Invalid location."
       Just (pc,_fun) -> liftIO (goto pc st)


snapAddBreakPoint :: Debugger -> Snap ()
snapAddBreakPoint st =
  do txt <- textParam "loc"
     case importBreakLoc (Text.unpack txt) of
       Nothing  -> badInput "Invalid location."
       Just loc -> do liftIO (addBreakPoint st loc)
                      funs <- liftIO (readIORef (dbgSources st))
                      sendJSON (exportBreakLoc funs loc)

snapRemoveBreakPoint :: Debugger -> Snap ()
snapRemoveBreakPoint st =
  do txt <- textParam "loc"
     case importBreakLoc (Text.unpack txt) of
       Nothing  -> badInput "Invalid location."
       Just loc -> liftIO (removeBreakPoint st loc)


snapClearBreakPoints :: Debugger -> Snap ()
snapClearBreakPoints st =
  liftIO (clearBreakPoints st)




snapGetState :: Debugger -> Snap ()
snapGetState st = sendJSON =<< liftIO (exportDebugger st)

snapPoll :: Debugger -> Snap ()
snapPoll st =
  do timeout <- natParam "timeout"
     command <- natParam "command"
     newCommand <-liftIO (poll st (fromIntegral command) (fromIntegral timeout))
     sendJSON (JS.toJSON newCommand)

snapGetFunction :: Debugger -> Snap ()
snapGetFunction st =
  do txt <- textParam "fid"
     sources <- liftIO (readIORef (dbgSources st))
     case funIdFromString (Text.unpack txt) of
       Just fid | Just js <- exportFun sources fid -> sendJSON js
       _ -> notFound

{-
snapAnalyze :: Debugger -> Snap ()
snapAnalyze = return () -- XXX
-}


--------------------------------------------------------------------------------

-- | Send a JSON value.
sendJSON :: JS.Value -> Snap ()
sendJSON a =
  do Snap.modifyResponse (Snap.setHeader "content-type" "application/json")
     Snap.writeLBS (JS.encode a)

-- | Get a bytestring parameter.
bsParam :: ByteString -> Snap ByteString
bsParam p =
  do mb <- Snap.getParam p
     case mb of
       Just x  -> return x
       Nothing -> badInput ("Missing parameter: " `BS.append` p)

-- | Get a text parameter
textParam :: ByteString -> Snap Text
textParam p = decodeUtf8With lenientDecode `fmap` bsParam p

-- | Get a natural number parameter
natParam :: ByteString -> Snap Integer
natParam p =
  do txt <- textParam p
     case decimal txt of
       Right (a,t) | Text.null t -> return a
       _ -> badInput ("Malformed natural-number parameter: " `BS.append` p)

-- | Get a booelan parameter, values @true@ or @false@.
boolParam :: ByteString -> Snap Bool
boolParam p =
  do bs <- bsParam p
     if bs == "true"
        then return True
        else if bs == "false"
               then return False
               else badInput ("Malformed boolean parameter: " `BS.append` p)




-- | Finish with "Bad user input"
badInput :: ByteString -> Snap a
badInput msg =
  Snap.finishWith (Snap.setResponseStatus 400 msg Snap.emptyResponse)

-- | Finish with "Not found"
notFound :: Snap a
notFound = Snap.finishWith (Snap.setResponseStatus 404 "Not Found"
                                                      Snap.emptyResponse)

errLocked :: Snap a
errLocked = Snap.finishWith (Snap.setResponseStatus 423 "Locked"
                                                      Snap.emptyResponse)


maybeReadFile :: FilePath -> IO (Maybe Text)
maybeReadFile fp =
  fmap Just (Text.readFile fp)
  `catch` \e ->
      if isDoesNotExistError e
        then return Nothing
        else throwIO e

--------------------------------------------------------------

data DebugConfig = DebugConfig
  { dbgSnapConfig :: Config Snap ()
  , dbgGaluaOptions :: Options
  }

defaultDebugConfig :: DebugConfig
defaultDebugConfig = DebugConfig defaultConfig defaultOptions

getConfig :: IO DebugConfig
getConfig =
  do mbConfigFile <- maybeReadFile "config.txt"
     case mbConfigFile of
       Nothing -> return defaultDebugConfig
       Just configFile ->
         do configValue <- case parse configFile of
                             Left e -> fail ("config file error: " ++ e)
                             Right configValue -> return configValue
            snapConfig <- case preview (key "http") configValue of
                            Nothing -> return defaultConfig
                            Just v -> valueToSnapConfig v
            breakpoints <- case getBreakpoints configValue of
                             Left e -> fail ("config file error: " ++ e)
                             Right bs -> return bs
            print breakpoints
            return DebugConfig
              { dbgSnapConfig = snapConfig
              , dbgGaluaOptions = Options
                  { optBreakPoints = breakpoints
                  }
              }

getBreakpoints :: Value -> Either String (Map FilePath [Int])
getBreakpoints v =
  case preview (key "breakpoints") v of
    Nothing -> Right Map.empty
    Just (List xs) -> Map.fromListWith (++) <$> mapM parseBreakpoint xs
    Just _ -> Left "Bad breakpoints section"

parseBreakpoint :: Value -> Either String (FilePath, [Int])
parseBreakpoint (List (Text fp : lineNumVals)) =
  do lineNums <- mapM getNumber lineNumVals
     return (Text.unpack fp, lineNums)

getNumber :: Value -> Either String Int
getNumber (Number _ n) = Right (fromIntegral n)
getNumber _            = Left "Bad line number"

valueToSnapConfig :: Value -> IO (Config Snap ())
valueToSnapConfig v =
  case configValueGetOpt (optDescrs defaultConfig) v of
    (fs,[]) ->
      case sequence fs of
        Nothing -> fail "http configuration settings incorrect"
        Just fs' -> return (mconcat (defaultConfig : fs'))
    (_,es) ->
      do mapM_ (hPutStrLn stderr) es
         fail "configuration file failure"

-- | Find the first element visted by a Traversal
preview ::
  ((a -> Constant (First a) a) -> b -> Constant (First a) b) ->
  b -> Maybe a
preview l x = getFirst (getConstant (l (Constant . First . Just) x))
{-# INLINE preview #-}

incrementPort :: Int -> Config Snap () -> Config Snap ()
incrementPort n c = setPort (basePort + n) c
  where
  basePort = fromMaybe 8000 (getPort c)

