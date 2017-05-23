module Galua.Debugger.NameHarness
  ( CompiledStatment
  , compileStatementForLocation
  , executeCompiledStatment
  , executeStatementInContext
  , ParseError(..)
  )
  where

import           Control.Exception (Exception, try, throwIO)
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as B8
import           Data.IORef (IORef, newIORef)
import           Data.Foldable (forM_)
import           Data.List (unfoldr, mapAccumR, intercalate)
import           Data.Maybe (catMaybes)
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import           Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as IOVector
import           Galua.FunValue (funValueCode, FunCode(..))
import           Galua.LuaString (fromByteString)
import           Galua.Mach
import qualified Galua.Util.SizedVector as SV
import           Galua.Util.SizedVector (SizedVector)
import qualified Galua.Util.Stack as Stack
import           Galua.Value (Value(String,Table,Nil))
import           Galua.Code

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
  do let harness = makeHarness params statement
     res <- parseLua Nothing (L8.pack harness)
     case res of
       Left e                    -> throwIO (ParseError e)
       Right (Chunk _ outerFunc) ->
         case funcNested outerFunc Vector.!? 0 of
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
    ("return function(" ++ args ++ ")"):

    -- Define the local variables in order to enforce register names
    [ "  do"
    , "    local _ = " ++ intercalate "," upvals
    , "  end"
    , "  " ++ statement
    , "end"
    ]
  where
    upvals
      | "_ENV" `elem` harnessUpvals params = harnessUpvals params
      | otherwise                          = harnessUpvals params ++ ["_ENV"]

    args = intercalate "," (catMaybes (harnessLocals params) ++ ["..."])


computeParams ::
  Function {- ^ context to run statement in -} ->
  Int      {- ^ program counter             -} ->
  HarnessParams
computeParams func pc =
  HarnessParams
  { harnessLocals = names
  , harnessUpvals = Vector.toList $ fmap B8.unpack
                  $ debugInfoUpvalues $ funcDebug func
        -- XXX: UTF-8
  }
  where
    names = snd
          $ mapAccumR aux Set.empty
          $ unfoldr genLocal 0

    aux :: Set String -> Maybe String -> ( Set String, Maybe String )
    aux seen n@(Just x)
      | Set.notMember x seen = ( Set.insert x seen, n       )
    aux seen _               = ( seen             , Nothing )

    getLocalName = lookupLocalName func pc

    -- checking for space is our approximation of determining
    -- what is a valid local variable name and what is a compiler
    -- generated debug name for a for loop state variable
    checkName n
      | B8.elem ' ' n = Nothing
      | otherwise     = Just (B8.unpack n)

    genLocal i =
        do name <- getLocalName (Reg i)
           return (checkName name, i+1)

paramsForFunCode :: FunCode -> Int -> HarnessParams
paramsForFunCode code pc =
  case code of
    LuaOpCodes func -> computeParams func pc
    CCode {}        -> HarnessParams { harnessLocals = [], harnessUpvals = [] }



data CompiledStatment = CompiledStatment
  { csFunc    :: Function
  , csParams  :: HarnessParams
  }

execEnvForCompiledStatment ::
  IORef Value ->
  ExecEnv         {- ^ parent context             -} ->
  CompiledStatment                                   ->
  IO ExecEnv
execEnvForCompiledStatment globals env stat =
  do stack <- case env of
                ExecInLua lenv -> prepareLuaStack stat (luaExecRegs lenv)
                ExecInC cenv   -> prepareCStack stat (cExecStack cenv)
     ups   <- do oldUps <- Vector.freeze (execUpvals env)
                 Vector.thaw (Vector.snoc oldUps globals)
     vas   <- case env of
                ExecInLua lenv -> return $! luaExecVarargs lenv
                ExecInC {}    -> newIORef []

     vrs <- newIORef NoVarResults

     return $! ExecInLua LuaExecEnv
        { luaExecRegs     = stack
        , luaExecVarress  = vrs
        , luaExecUpvals   = ups
        , luaExecVarargs  = vas
        , luaExecCode     = funcCode (csFunc stat)
        , luaExecClosure  = Nil
        , luaExecFID      = noFun
        , luaExecFunction = csFunc stat
        }


-- | Prepare a statement for execution at the specific program location.
-- XXX: Handle parser exeception
compileStatementForLocation ::
  FunCode  {- ^ Context function -}                   ->
  Int      {- ^ Program counter (used only in Lua) -} ->
  String   {- ^ Statement text  -}                    ->
  IO CompiledStatment
compileStatementForLocation fun pc statText =
  do let ps = paramsForFunCode fun pc
     func <- harnessFunction ps statText
     return CompiledStatment { csFunc = func, csParams = ps }


prepareCStack ::
  CompiledStatment  ->
  SizedVector Value {- ^ parent's stack     -} ->
  IO (IOVector (IORef Value))
prepareCStack stat parentStack =
  do let sz = funcMaxStackSize (csFunc stat)
     stack <- IOVector.new sz

     let locals  = harnessLocals (csParams stat)
         visible = catMaybes (zipWith (<$) [0..] locals)
         lenVis  = length visible

     forM_ (zip [ 0 .. ] visible) $ \(i,x) ->
        IOVector.write stack i =<< newIORef =<< SV.get parentStack x

     forM_ [ lenVis .. sz - 1 ] $ \i ->
        IOVector.write stack i =<< newIORef Nil

     return stack



prepareLuaStack ::
  CompiledStatment                                     ->
  IOVector (IORef Value) {- ^ parent's stack     -} ->
  IO (IOVector (IORef Value))
prepareLuaStack stat parentStack =
  do let sz = funcMaxStackSize (csFunc stat)
     stack <- IOVector.new sz

     let locals  = harnessLocals (csParams stat)
         visible = catMaybes (zipWith (<$) [0..] locals)
         lenVis  = length visible :: Int

     forM_ (zip [ 0 .. ] visible) $ \(i,x) ->
        IOVector.write stack i =<< IOVector.read parentStack x

     forM_ [ lenVis .. sz - 1 ] $ \i ->
        IOVector.write stack i =<< newIORef Nil

     return stack



executeCompiledStatment ::
  VM                  {- ^ For globals, and the currently executing thread -} ->
  ExecEnv             {- ^ Lookup free variables here -} ->
  CompiledStatment    {- ^ "Code pointer" -} ->
  ([Value] -> IO NextStep) {- ^ Do this when we return -} ->
  IO (NextStep, VM)
executeCompiledStatment vm cenv cs resume =
  do globRef <- newIORef (Table (machGlobals (vmMachineEnv vm)))
     env     <- execEnvForCompiledStatment globRef cenv cs
     let th = vmCurThread vm

     let thEnv = vmCurExecEnv vm
     thPc       <- getThreadPC th
     thStack    <- getThreadField stStack th
     thHandlers <- getThreadField stHandlers th

     let recover e  = resume [e]
         frame      = CallFrame thPc thEnv (Just recover) resume

     setThreadField stExecEnv th env
     setThreadField stHandlers th (DefaultHandler : thHandlers)
     setThreadField stStack th (Stack.push frame thStack)

     return (Goto 0, vm { vmCurExecEnv = env } )




executeStatementInContext ::
  VM -> Int -> ExecEnv -> String -> ([Value] -> IO NextStep) -> IO (NextStep,VM)
executeStatementInContext vm pc env statement resume =
  do let fun = funValueCode (execFun env)
     res <- try (compileStatementForLocation fun pc statement)
     case res of
       Left (ParseError e) ->
          do b <- fromByteString (B8.pack e)
             next <- resume [String b]
             return (next, vm)
       Right cs -> executeCompiledStatment vm env cs resume


