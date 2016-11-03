module Galua.Debugger.NameHarness
  ( execEnvForStatement
  , executeStatementOnVM
  )
  where

import           Control.Monad.IO.Class (liftIO)
import           Control.Exception (Exception, throwIO)
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as B8
import           Data.IORef (newIORef)
import           Data.List (unfoldr, mapAccumR, intercalate)
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import           Galua.FunValue (funValueCode, luaFunction, FunCode(..))
import           Galua.Reference (readRef, writeRef)
import           Galua.Mach (NextStep(PrimStep), StackFrame(CallFrame), Thread(..), VM(vmCurThread), ApiCallStatus(NoApiCall), ExecEnv(..), parseLua)
import qualified Galua.Util.SizedVector as SV
import qualified Galua.Util.Stack as Stack
import           Galua.Value (Value(Nil), prettyValue)
import           Language.Lua.Bytecode
import           Language.Lua.Bytecode.FunId (noFun)
import           Language.Lua.Bytecode.Debug (lookupLocalName)
import qualified System.Clock as Clock

data HarnessParams = HarnessParams
   { harnessLocals :: [Maybe String]
   , harnessUpvals :: [String]
   }

data ParseError = ParseError String
  deriving (Show)

instance Exception ParseError

-- | Can throw 'ParseError'
harnessFunction ::
  HarnessParams ->
  String      {- ^ statement                    -} ->
  IO Function {- ^ function for given statement -}
harnessFunction params statement =
  do res <- parseLua Nothing (L8.pack (makeHarness params statement))
     case res of
       Left e                    -> throwIO (ParseError e)
       Right (Chunk _ outerFunc) ->
         case funcProtos outerFunc Vector.!? 0 of
           Nothing        -> throwIO (ParseError "PANIC: Bad harness")
           Just innerFunc -> return innerFunc

makeHarness ::
  HarnessParams ->
  String {- ^ statement -} ->
  String {- ^ lua code  -}
makeHarness params statement = unlines $
    [ "local " ++ n | n <- harnessUpvals params ] ++

    -- The name _ doesn't shadow anything because it's
    -- not in scope until after the function is defined
    ("return function(" ++ makeFunctionArgs params ++ ")"):

    -- Define the local variables in order to enforce register names
    (if null (harnessUpvals params)
        then []
        else ["  do"
             ,"    local _ = " ++ intercalate "," (harnessUpvals params)
             ,"  end"]) ++

    [ "  " ++ statement
    , "end"
    ]

makeFunctionArgs :: HarnessParams -> String
makeFunctionArgs params = intercalate "," (names ++ ["..."])
  where

    names = snd (mapAccumR aux (0, Set.empty) (harnessLocals params))

    aux :: (Int, Set String) -> Maybe String -> ( (Int, Set String), String )
    aux (freshNum, seen) (Just x)
      | Set.notMember x seen = ( (freshNum, Set.insert x seen), x )
    aux (freshNum, seen) _   = ( (freshNum+1, seen), "SOFRESH"++show freshNum )


computeParams ::
  Function {- ^ context to run statement in -} ->
  Int      {- ^ program counter             -} ->
  HarnessParams
computeParams func pc = HarnessParams
  { harnessLocals = unfoldr aux 0
  , harnessUpvals = Vector.toList $ fmap B8.unpack
                  $ debugInfoUpvalues $ funcDebug func
        -- XXX: UTF-8
  }
  where
    getLocalName = lookupLocalName func pc

    -- checking for space is our approximation of determining
    -- what is a valid local variable name and what is a compiler
    -- generated debug name for a for loop state variable
    checkName n
      | B8.elem ' ' n = Nothing
      | otherwise     = Just (B8.unpack n)

    aux i = do name <- getLocalName (Reg i)
               return (checkName name, i+1)

paramsForExecEnv ::
  ExecEnv {- ^ execution environment -} ->
  Int     {- ^ program counter       -} ->
  HarnessParams
paramsForExecEnv env pc =
  case funValueCode (execFunction env) of
    LuaOpCodes func -> computeParams func pc
    CCode {}        -> HarnessParams { harnessLocals = [], harnessUpvals = [] }

execEnvForStatement ::
  ExecEnv {- ^ parent context         -} ->
  Int     {- ^ parent program counter -} ->
  String  {- ^ statement              -} ->
  IO ExecEnv
execEnvForStatement env pc statement =
  do let params = paramsForExecEnv env pc
     func       <- harnessFunction params statement
     apiCallRef <- newIORef NoApiCall
     now        <- Clock.getTime Clock.ProcessCPUTime
     stack      <- SV.shallowCopy (execStack env)
     return env
       { execStack    = stack
       , execFunction = luaFunction noFun func
       , execClosure  = Nil -- used for debug API
       , execApiCall  = apiCallRef
       , execCreateTime = now
       , execChildTime  = 0
       }

executeStatementOnVM :: VM -> NextStep -> String -> IO ()
executeStatementOnVM vm next statement =
  do th <- readRef (vmCurThread vm)
     env <- execEnvForStatement (stExecEnv th) (stPC th) statement
     -- XXX: Install error handler
     -- XXX: Install breakpoint
     -- XXX: Display result in UI
     let resume res = PrimStep (next <$ liftIO (mapM_ (putStrLn . prettyValue) res))
         frame = CallFrame (stPC th) (stExecEnv th) Nothing resume
     writeRef (vmCurThread vm)
         th { stExecEnv = env
            , stStack   = Stack.push frame (stStack th)
            }
