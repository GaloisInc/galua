{-# LANGUAGE NamedFieldPuns, RecordWildCards, OverloadedStrings #-}
module Galua.Debugger.Types where

import           Data.Text(Text)
import           Data.Text.Encoding(decodeUtf8)
import           Data.Text.Read(decimal)
import           Data.Vector(Vector)
import           Data.Map(Map)
import           Data.IntMap(IntMap)
import           Data.Set(Set)
import           Data.ByteString(ByteString)
import           Data.IORef(IORef)
import           Data.List(unfoldr)
import           Data.Maybe(fromMaybe)
import           Data.Word(Word64)


import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B

import           Control.Concurrent

import           Galua.Code
import           Galua.Mach
import           Galua.Value
import           Galua.Names.Find(LocatedExprName(..))
import           Galua.Util.IOURef

import qualified Galua.Spec.AST as Spec
import qualified Galua.Spec.Parser as Spec

import           Galua.Debugger.PrettySource (lexChunk,Line,NameId)
import           Galua.Debugger.Options
import           Galua.Debugger.NameHarness


data ExecState = ExecState
  { dbgStatus      :: {-# UNPACK #-} !(IORef VMState)
    -- ^ This is to observe execution: we write, they read.

  , dbgResume     :: {-# UNPACK #-} !(MVar (VM,NextStep,StepMode))
    -- ^ When we pause, we take this MVar.  To resume execution,
    -- someone notifies us by putting what to do next here.
    -- We take, they put.


  , dbgStepMode   :: {-# UNPACK #-} !(IORef StepMode)
    -- ^ Save the execution mode across C calls

  , dbgBreaks    :: {-# UNPACK #-}
                     !(IORef (Map (Int,FunId) (Maybe BreakCondition)))
    -- ^ Static break points.  The key of the map is the break point,
    -- the value is an optional condition.  The breakpoint will only
    -- be active if the condition is true.

  , dbgBreakOnError :: {-# UNPACK #-} !(IOURef Bool)
    -- ^ Should we stop automatically, when we encounter an error.

  , dbgInterrupt :: {-# UNPACK #-} !(IOURef Bool)
    -- ^ Set this `true` to interrupt the debugger.

  , dbgPollState :: !PollState
    -- ^ Notify clients when we've stopped.
  }

data PollState = PollState
  { dbgClients          :: {-# UNPACK #-}!(MVar [MVar ()])
    -- ^ Ping these mvars when we stop.


  , dbgInterruptCounter :: {-# UNPACK #-}!(IOURef Word64)
    -- ^ Counte how many times we've stopped.  This is so the polling
    -- interface can notice that we've stopped and redraw itself.
    -- Perhaps there is a better way to achieve this?
  }


{- The fields in this are mutable, so that the interpreter can modify them
easily, without having to pass the state aroud.  For example, when we
load a new module, the interpreter uses an IO action, which modifies 
dbgSource. -}
data Debugger = Debugger
  { dbgSources   :: !(IORef Chunks)
    {- ^ Source code for chunks and corresponding parsed functions.
          The functions are also in the VM state, but that's only available
          while the machine is running. -}



  , dbgExec      :: !(ExecState)
  , dbgMachEnv   :: !MachineEnv


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
  , brkActive :: IORef (Maybe Bool)
    -- ^ Did we alerady check this condition.
  }

newtype GlobalTypeMap = GTM (Map ByteString GlobalTypeEntry)
data GlobalTypeEntry  = GlobalType [SpecType] | GlobalNamespace GlobalTypeMap


data VMState  = PausedInLua !VM !NextStep !IdleReason
              | RunningInLua
              | RunningInC !VM              -- ^ VM as it was when we left
              | NotYetRunning               -- ^ Initial state

data DebuggerCommand =
    StartExec StepMode (MVar ())
    -- ^ Switch exetion to the given mode, or start executing if we were idle.

  | AddClient (MVar ())





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
        protos    = zip [ 0 .. ] (Vector.toList (funcNested f))
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

  | StepOutOp
    -- ^ Finish evaluating the current code, step over function calls

  | Stop
    -- ^ Evaluate until the closest safe place to stop.

  | StepOut StepMode
    -- ^ Evaluate until we return from the current function.
    -- After that, procdeed with the given mode.


  | StepOverLine {-# UNPACK #-} !Int
    -- ^ Evaluate while we are on this line number.
    -- If we encoutner functions, do not stop until they return
    -- or some other condition caused us to stop.

  | StepIntoLine {-# UNPACK #-} !Int
    -- ^ Evaluate while we are on this line number.
    -- ^ If we encoutner a function-call, then we stop at the beginning
    -- of the called function.

  | StepOutYield StepMode
    -- ^ Evaluate until the current thread yeilds (or something else
    -- causes us to stop).
    -- After that, proceed with the given mode.

  deriving (Eq, Show)








