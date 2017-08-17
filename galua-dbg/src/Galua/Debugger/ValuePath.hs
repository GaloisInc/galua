module Galua.Debugger.ValuePath
  ( ValuePath(..)
  , showValuePath
  , getValue
  , setValue
  ) where

import           Data.IORef(IORef,modifyIORef',writeIORef,readIORef)
import qualified Data.Vector.Mutable as IOVector
import           MonadLib(ExceptionT,runExceptionT,raise,lift,when)

import Galua.Value
import Galua.Mach
import qualified Galua.Util.SizedVector as SV


{- | A value path identifies a value that may be of interest to the user.
Note that this is different from the reference of a value, as 'ValuePath'
could refer to a specific register, or a field of a table.
Furthermore, value paths depend on the context (i.e., stack frame), so
we can distinguish between "register 1" in one stack frame and "register 1"
in another. -}
data ValuePath
  = VP_Field     ValuePath Value -- ^ path to value indexed by given key
  | VP_Key       ValuePath Value -- ^ path to actual key
  | VP_CUpvalue  ValuePath Int   -- ^ path to upvalues of a closure
  | VP_MetaTable ValuePath       -- ^ path in the metatable of a value
  | VP_Register ExecEnv Int      -- ^ a register in a stack frame
  | VP_Upvalue  ExecEnv Int      -- ^ up value in stack frame
  | VP_Varargs  ExecEnv Int      -- ^ entry in the var-args.
  | VP_Registry Value            -- ^ a location in the registry.
  | VP_None                      -- ^ refers to nothing.


-- | Render a value path to a human readable form.
showValuePath :: ValuePath -> String
showValuePath = flip go ""
  where
  go (VP_Key p v) = go p
                  . showChar '{' . showString (prettyValue v) . showChar '}'
  go (VP_Field p v) = go p
                    . showChar '[' . showString (prettyValue v) . showChar ']'
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

      VP_Register eenv n ->
        case eenv of
          ExecInLua lenv ->
            do mb <- lift (getMb lenv n)
               case mb of
                 Just r  -> lift (readIORef r)
                 Nothing -> raise ()

          ExecInC cenv ->
            do mb <- lift (SV.getMaybe (cExecStack cenv) n)
               case mb of
                 Just r  -> return r
                 Nothing -> raise ()

      VP_Upvalue eenv n ->
        let us = execUpvals eenv
        in if 0 <= n && n < IOVector.length us
             then lift (readIORef =<< IOVector.read us n)
             else raise ()

      VP_Varargs eenv n ->
        case eenv of
          ExecInLua lenv ->
             do varargs <- lift (readIORef (luaExecVarargs lenv))
                case drop n varargs of
                  r:_ -> return r
                  []  -> raise ()
          _ -> raise ()

      VP_Registry registry -> return registry

      VP_None -> raise ()




-- | Set the value associated with a particular path.
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

  setReg n eenv =
    case eenv of
      ExecInLua lenv ->
       do mb <- getMb lenv n
          case mb of
            Just r  -> writeIORef r v
            Nothing -> return ()

      ExecInC cenv -> SV.setMaybe (cExecStack cenv) n v

  setUVal n env =
    let uvs = execUpvals env
    in when (0 <= n && n < IOVector.length uvs) $
          do r <- IOVector.read uvs n
             writeIORef r v

  setVArg n env =
    case env of
      ExecInLua lenv ->
        modifyIORef' (luaExecVarargs lenv) $ \varargs ->
          case splitAt n varargs of
            (a,_:b) -> a++v:b
            _       -> varargs

      _ -> return ()


getMb :: LuaExecEnv -> Int -> IO (Maybe (IORef Value))
getMb lenv n = if n >= IOVector.length (luaExecRegs lenv) || n < 0
                    then return Nothing
                    else Just <$> IOVector.unsafeRead (luaExecRegs lenv) n

