{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Galua.CApi where

import Control.Concurrent (MVar, takeMVar)
import Control.Concurrent.STM (TMVar, atomically, putTMVar)
import Control.Exception
import Control.Monad (replicateM_,when,unless)
import Control.Monad.IO.Class
import Data.Int
import Data.IORef
import Data.Foldable (toList, for_, traverse_)
import Data.Functor (void)
import Data.Traversable (for)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as Vector
import           Data.Vector (Vector)
import qualified Data.Vector.Mutable as IOVector
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.StablePtr
import Foreign.Storable
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Unsafe as U
import qualified Data.ByteString.Char8 as B8
import System.IO
import System.IO.Unsafe (unsafeInterleaveIO)

import Language.Lua.Bytecode
import Language.Lua.Bytecode.Parser
import Language.Lua.Bytecode.Debug
import Language.Lua.Bytecode.FunId


import qualified Galua.Util.SizedVector as SV

import Galua.Arguments
import Galua.Mach
import Galua.Number
import Galua.Overloading
import Galua.CObjInfo
import Galua.Reference
import Galua.Value
import Galua.LuaString
import Galua.FunValue
import qualified Galua.Util.IOVector as IOVector

#include "lua.h"

type Lua_Number = (#type lua_Number)
type Lua_Integer = (#type lua_Integer)
type Lua_KContext = (#type lua_KContext)
type Lua_KFunction = FunPtr (Ptr () -> CInt -> Lua_KContext -> IO CInt)
-- typedef int (*lua_KFunction) (lua_State *L, int status, lua_KContext ctx);
type Lua_Alloc = FunPtr (Ptr () -> Ptr () -> CSize -> CSize -> IO (Ptr ()))
-- typedef void * (*lua_Alloc) (void *ud, void *ptr, size_t osize, size_t nsize);

luaOK, luaYIELD, luaERRRUN :: CInt
luaOK = (#const LUA_OK)
luaYIELD = (#const LUA_YIELD)
luaERRRUN = (#const LUA_ERRRUN)

--------

type EntryPoint a =
  Ptr () {- ^ stable pointer -} ->
  Ptr () {- ^ return address -} ->
  a

type Reentry m =
  String            {- ^ entry name -} ->
  [PrimArgument]    {- ^ entry arguments -} ->
  Ptr ()            {- ^ stable pointer -} ->
  Ptr ()            {- ^ return address -} ->
  (SV.SizedVector (IORef Value) -> m (Maybe PrimArgument))
                    {- ^ operation continuation producing optional C return -} ->
  IO CInt

reentryIO :: Reentry IO
reentryIO label args l r k = reentry label args l r (liftIO . k)

-- | Resume execution of the machine upon entry from the C API
reentry :: Reentry Mach
reentry label args l r k =
  do res <- try body
     case res of
       Right x -> return x
       Left (SomeException e) ->
         do hPutStrLn stderr (displayException e)
            return (-1)
  where
    body =
      do ext <- deRefLuaState l
         returnObjInfo <- unsafeInterleaveIO $ do f <- cfunInfoFun
                                                  f (castPtrToFunPtr r)
         atomically (putTMVar
           (extLuaStateLuaServer ext)
           (CReEntry label returnObjInfo args (wrapWithStack (extLuaStateThreadId ext) k)))
         cServiceLoop l (extLuaStateCServer ext) (extLuaStateLuaServer ext)

wrapWithStack :: Int -> (SV.SizedVector (IORef Value) -> Mach a) -> Mach a
wrapWithStack threadId k =
  do Just threadRef <- machLookupRef threadId
     eenv <- getThreadField stExecEnv threadRef
     k (execStack eenv)

result :: (MonadIO m, CArg a) => Ptr a -> a -> m (Maybe PrimArgument)
result ptr res =
  do liftIO (poke ptr res)
     return (Just (cArg res))

push :: MonadIO m => SV.SizedVector (IORef a) -> a -> m ()
push args x = liftIO (SV.push args =<< newIORef x)

pop :: SV.SizedVector (IORef Value) -> Mach Value
pop stack =
  do vs <- liftIO (popN stack 1)
     case vs of
       [v] -> return v
       _   -> luaError "missing argument"

popN :: MonadIO m => SV.SizedVector (IORef Value) -> Int -> m [Value]
popN stack n = liftIO $
  do len <- SV.size stack
     let n' = max 0 (min n len)
     vs <- for [len-n' .. len-1] $ \i ->
             readIORef =<< SV.get stack i
     SV.shrink stack n'
     return vs

stackToList :: SV.SizedVector (IORef a) -> IO [a]
stackToList stack =
  do n  <- SV.size stack
     for [0 .. n-1 ] $ \i -> readIORef =<< SV.get stack i


stackFromList :: SV.SizedVector (IORef a) -> [a] -> IO ()
stackFromList stack vs =
  do n <- SV.size stack
     SV.shrink stack n
     for_ vs $ push stack

cServiceLoop :: Ptr () -> MVar CNextStep -> TMVar CCallState -> IO CInt
cServiceLoop l resultMVar interpMVar =
  do res <- takeMVar resultMVar
                `catch` \SomeException{} -> return CAbort
     case res of
       CAbort  -> return 1
       CResume -> return 0
       CCallback cfun ->
         do resultN <- capi_entry cfun l

            case resultN of
              -1 -> return () -- normal error unwind
              -2 -> fail "C functions called from Lua must return non-negative number"
              _ | resultN < 0 -> fail "Panic: capi_entry had invalid return value"
                | otherwise -> atomically (putTMVar interpMVar (CReturned (fromIntegral resultN)))

            cServiceLoop l resultMVar interpMVar

foreign import ccall "galua_capi_entry" capi_entry ::
  CFun -> Ptr () -> IO CInt

deRefLuaState :: Ptr () -> IO ExternalLuaState
deRefLuaState statePtr =
  do stabPtr <- peek (castPtr statePtr)
     let stab = castPtrToStablePtr stabPtr
     deRefStablePtr stab

class Storable a => CArg a where cArg :: a -> PrimArgument
instance CArg CSize      where cArg = PrimIntArg . fromIntegral
instance CArg CInt       where cArg = PrimIntArg . fromIntegral
instance CArg Double     where cArg = PrimDoubleArg
instance CArg Int32      where cArg = PrimIntArg . fromIntegral
instance CArg Int64      where cArg = PrimIntArg . fromIntegral
instance CArg (FunPtr a) where cArg = PrimFunPtrArg . castFunPtr
instance CArg (Ptr a)    where cArg = PrimPtrArg . castPtr

-- | Null-terminated C String. This method is outside of the CArg
-- class to require it to be explicitly chosen.
cstringArg0 :: CString -> PrimArgument
cstringArg0 = PrimCStringArg

-- | Null-terminated C String. This method is outside of the CArg
-- class to require it to be explicitly chosen.
cstringArg :: CStringLen -> PrimArgument
cstringArg = PrimCStringLenArg

--------------------------------------------------------------------------------

foreign export ccall lua_error_hs :: EntryPoint (IO CInt)

lua_error_hs :: EntryPoint (IO CInt)
lua_error_hs l r = reentry "lua_error" [] l r $ \args ->
  do n <- liftIO (SV.size args)
     e <- if n == 0 then return Nil else pop args
     machThrow e

--------------------------------------------------------------------------------
-- Stack push functions
--------------------------------------------------------------------------------

foreign export ccall lua_pushlightuserdata_hs :: EntryPoint (Ptr () -> IO CInt)

lua_pushlightuserdata_hs :: EntryPoint (Ptr () -> IO CInt)
lua_pushlightuserdata_hs l r u =
  reentryIO "lua_pushlightuserdata" [cArg u] l r $ \stack ->
    Nothing <$ push stack (LightUserData u)

foreign export ccall lua_pushnumber_hs :: EntryPoint (Lua_Number -> IO CInt)

-- | [-0, +1, -]
lua_pushnumber_hs :: EntryPoint (Lua_Number -> IO CInt)
lua_pushnumber_hs l r n =
  reentryIO "lua_pushnumber" [cArg n] l r $ \stack ->
    Nothing <$ push stack (Number (Double (realToFrac n)))

foreign export ccall lua_pushnil_hs :: EntryPoint (IO CInt)

-- | [-0, +1, -]
lua_pushnil_hs :: EntryPoint (IO CInt)
lua_pushnil_hs l r =
  reentryIO "lua_pushnil" [] l r $ \args ->
    Nothing <$ push args Nil

foreign export ccall lua_pushinteger_hs :: EntryPoint (Lua_Integer -> IO CInt)

-- | [-0, +1, -]
lua_pushinteger_hs :: EntryPoint (Lua_Integer -> IO CInt)
lua_pushinteger_hs l r n =
  reentryIO "lua_pushinteger" [cArg n] l r $ \stack ->
    Nothing <$ push stack (Number (fromIntegral n))

foreign export ccall lua_pushboolean_hs :: EntryPoint (CInt -> IO CInt)

-- | [-0, +1, -]
lua_pushboolean_hs :: EntryPoint (CInt -> IO CInt)
lua_pushboolean_hs l r b =
  reentryIO "lua_pushboolean" [cArg b] l r $ \stack ->
    Nothing <$ push stack (Bool (b /= 0))

foreign export ccall lua_pushstring_hs :: EntryPoint (CString -> Ptr CString -> IO CInt)

-- | [-0, +1, m]
lua_pushstring_hs :: EntryPoint (CString -> Ptr CString -> IO CInt)
lua_pushstring_hs l r ptr out =
  reentryIO "lua_pushstring" [cstringArg0 ptr] l r $ \args ->

    result out =<<
      if nullPtr == ptr

       then do push args Nil
               return nullPtr

       else do str <- peekLuaString0 ptr
               push args (String str)
               luaStringPtr str

foreign export ccall lua_pushlstring_hs
  :: EntryPoint (CString -> CSize -> Ptr CString -> IO CInt)

-- | [-0, +1, m]
lua_pushlstring_hs :: EntryPoint (CString -> CSize -> Ptr CString -> IO CInt)
lua_pushlstring_hs l r ptr sz out =
  reentryIO "lua_pushlstring" [cstringArg (ptr, fromIntegral sz)] l r $ \args ->
  do str <- peekLuaString (ptr, fromIntegral sz)
     -- Note that the output string must be null terminated!!
     push args (String str)
     result out =<< luaStringPtr str

foreign export ccall lua_pushvalue_hs :: EntryPoint (CInt -> IO CInt)

-- | [-0, +1, -]
lua_pushvalue_hs :: EntryPoint (CInt -> IO CInt)
lua_pushvalue_hs l r ix =
  reentry "lua_pushvalue" [cArg ix] l r $ \args ->
  do x <- valueArgument (fromIntegral ix) args
     liftIO (push args x)
     return Nothing

foreign export ccall lua_pushcclosure_hs :: EntryPoint (CFun -> CInt -> IO CInt)

-- [-nup, +1, e]
lua_pushcclosure_hs :: EntryPoint (CFun -> CInt -> IO CInt)
lua_pushcclosure_hs l r func nup =
  reentry "lua_pushcclosure" [cArg func, cArg nup] l r $ \args ->
  do upvals <- popN args (fromIntegral nup)
     info   <- machLookupCFun func
     vs     <- liftIO (Vector.thaw =<< traverse newIORef (Vector.fromList upvals))
     c      <- machNewClosure (cFunction CFunName { cfunName = info
                                                  , cfunAddr = func}) vs
     push args (Closure c)
     return Nothing

foreign export ccall lua_tocfunction_hs
  :: EntryPoint (CInt -> Ptr CFun -> IO CInt)

-- [-0, +0, -]
lua_tocfunction_hs :: EntryPoint (CInt -> Ptr CFun -> IO CInt)
lua_tocfunction_hs l r ix out =
  reentry "lua_tocfunction" [cArg ix] l r $ \args ->
  do x <- valueArgument (fromIntegral ix) args
     res <- case x of
              Closure cref ->
                case funValueName (cloFun (referenceVal cref)) of
                  CFID cfun -> return (cfunAddr cfun)
                  _         -> return nullFunPtr
              _ -> return nullFunPtr
     result out res

foreign export ccall lua_iscfunction_hs :: EntryPoint (CInt -> Ptr CInt -> IO CInt)

-- [-0, +0, -]
lua_iscfunction_hs :: EntryPoint (CInt -> Ptr CInt -> IO CInt)
lua_iscfunction_hs l r ix out =
  reentry "lua_iscfunction" [cArg ix] l r $ \args ->
  do x <- valueArgument (fromIntegral ix) args
     res <- case x of
              Closure cref ->
                case funValueName (cloFun (referenceVal cref)) of
                  CFID {} -> return 1
                  _       -> return 0
              _ -> return 0
     result out res

--------------------------------------------------------------------------------

foreign export ccall lua_type_hs ::
  EntryPoint (CInt -> Ptr CInt -> IO CInt)

-- | [-0, +0, -]
lua_type_hs :: EntryPoint (CInt -> Ptr CInt -> IO CInt)
lua_type_hs l r ix out =
  reentry "lua_type" [cArg ix] l r $ \args ->
  do mb <- valueArgumentOpt (fromIntegral ix) args
     result out $
       case mb of
         Just x  -> lua_type_int x
         Nothing -> (#const LUA_TNONE)

lua_type_int :: Value -> CInt
lua_type_int v =
  case valueType v of
    NilType      -> (#const LUA_TNIL)
    NumberType   -> (#const LUA_TNUMBER)
    BoolType     -> (#const LUA_TBOOLEAN)
    StringType   -> (#const LUA_TSTRING)
    TableType    -> (#const LUA_TTABLE)
    FunctionType -> (#const LUA_TFUNCTION)
    UserDataType -> (#const LUA_TUSERDATA)
    LightUserDataType -> (#const LUA_TLIGHTUSERDATA)
    ThreadType   -> (#const LUA_TTHREAD)

--------------------------------------------------------------------------------

foreign export ccall lua_settop_hs :: EntryPoint (CInt -> IO CInt)

-- | [-?, +?, -]
lua_settop_hs :: EntryPoint (CInt -> IO CInt)
lua_settop_hs l r ix =
  reentryIO "lua_settop" [cArg ix] l r $ \stack ->
  do oldLen <- SV.size stack
     let i1 = fromIntegral ix
         newLen | i1 < 0    = oldLen + i1 + 1
                | otherwise = i1
     if oldLen <= newLen
       then replicateM_ (newLen - oldLen) (SV.push stack =<< newIORef Nil)
       else SV.shrink stack (oldLen - newLen)
     return Nothing

foreign export ccall lua_gettop_hs :: EntryPoint (Ptr CInt -> IO CInt)

-- | [-?, +?, -]
lua_gettop_hs :: EntryPoint (Ptr CInt -> IO CInt)
lua_gettop_hs l r out =
  reentryIO "lua_gettop" [] l r $ \stack ->
    do n <- SV.size stack
       result out (fromIntegral n)

foreign export ccall lua_rotate_hs :: EntryPoint (CInt -> CInt -> IO CInt)

lua_rotate_hs :: EntryPoint (CInt -> CInt -> IO CInt)
lua_rotate_hs l r idx n =
  reentryIO "lua_rotate" [cArg idx, cArg n] l r $ \args ->
    Nothing <$ rotateHelper (fromIntegral idx) (fromIntegral n) args

rotateHelper :: Int -> Int -> SV.SizedVector a -> IO ()
rotateHelper idx n stack =
  do len <- SV.size stack

     let idx' | idx > 0   = idx - 1
              | otherwise = len + idx

         elts = len - idx'

     SV.rotateSubset stack idx' elts n

foreign export ccall lua_absindex_hs :: EntryPoint (CInt -> Ptr CInt -> IO CInt)

-- | [-0, +0, -]
lua_absindex_hs :: EntryPoint (CInt -> Ptr CInt -> IO CInt)
lua_absindex_hs l r cidx out =
  reentryIO "lua_absindex" [cArg cidx] l r $ \stack ->
  do n <- SV.size stack
     let idx = fromIntegral cidx
         idx' | isPseudo idx || idx > 0 = idx
              | otherwise               = n + idx + 1
     result out (fromIntegral idx')

--------------------------------------------------------------------------------

foreign export ccall lua_settable_hs :: EntryPoint (CInt -> IO CInt)

-- | [-2, +0, e]
lua_settable_hs :: EntryPoint (CInt -> IO CInt)
lua_settable_hs l r ix =
  reentry "lua_settable" [cArg ix] l r $ \args ->
  do t <- valueArgument (fromIntegral ix) args
     kv <- liftIO (popN args 2)
     case kv of
       [k,v] -> Nothing <$ mach3 m__newindex t k v
       _ -> luaError "lua_settable: bad arguments"

--------------------------------------------------------------------------------

foreign export ccall lua_next_hs :: EntryPoint (CInt -> Ptr CInt -> IO CInt)

-- | [-0,+0,-]
lua_next_hs :: EntryPoint (CInt -> Ptr CInt -> IO CInt)
lua_next_hs l r idx out =
  reentry "lua_next" [cArg idx] l r $ \args ->
  do t <- tableArgument (fromIntegral idx) args
     p <- pop args
     liftIO $
       do res <- tableNext t p
          case res of
            Nothing    -> result out 0
            Just (x,y) -> do push args x
                             push args y
                             result out 1

--------------------------------------------------------------------------------

foreign export ccall lua_copy_hs :: EntryPoint (CInt -> CInt -> IO CInt)

-- | [-0,+0,-]
lua_copy_hs :: EntryPoint (CInt -> CInt -> IO CInt)
lua_copy_hs l r fromidx toidx =
  reentry "lua_copy" [cArg fromidx, cArg toidx] l r $ \args ->
  do x <- valueArgument (fromIntegral fromidx) args
     assign (fromIntegral toidx) x args
     return Nothing

--------------------------------------------------------------------------------

foreign export ccall lua_isnumber_hs :: EntryPoint (CInt -> Ptr CInt -> IO CInt)

-- | [-0, +0, -]
lua_isnumber_hs :: EntryPoint (CInt -> Ptr CInt -> IO CInt)
lua_isnumber_hs l r ix out =
  reentry "lua_isnumber" [cArg ix] l r $ \args ->
  do v <- valueArgument(fromIntegral ix) args
     result out $ case valueNumber v of
                    Just {} -> 1
                    Nothing -> 0

foreign export ccall lua_isuserdata_hs
  :: EntryPoint (CInt -> Ptr CInt -> IO CInt)

-- | [-0, +0, -]
lua_isuserdata_hs :: EntryPoint (CInt -> Ptr CInt -> IO CInt)
lua_isuserdata_hs l r ix out =
  reentry "lua_isuserdata" [cArg ix] l r $ \args ->
  do v <- valueArgument (fromIntegral ix) args
     result out $ case v of
                    UserData{}      -> 1
                    LightUserData{} -> 1
                    _               -> 0

foreign export ccall lua_isstring_hs
  :: EntryPoint (CInt -> Ptr CInt -> IO CInt)

-- | [-0, +0, -]
lua_isstring_hs :: EntryPoint (CInt -> Ptr CInt -> IO CInt)
lua_isstring_hs l r ix out =
  reentry "lua_isstring" [cArg ix] l r $ \args ->
  do v <- valueArgument (fromIntegral ix) args
     result out $ case v of
                    String{} -> 1
                    Number{} -> 1
                    _        -> 0

foreign export ccall lua_isinteger_hs
  :: EntryPoint (CInt -> Ptr CInt -> IO CInt)

-- | [-0, +0, -]
lua_isinteger_hs :: EntryPoint (CInt -> Ptr CInt -> IO CInt)
lua_isinteger_hs l r ix out =
  reentry "lua_isinteger" [cArg ix] l r $ \args ->
  do v <- valueArgument (fromIntegral ix) args
     result out $ case v of
                    Number Int{} -> 1
                    _            -> 0

foreign export ccall lua_stringtonumber_hs
  :: EntryPoint (CString -> Ptr CSize -> IO CInt)

lua_stringtonumber_hs :: EntryPoint (CString -> Ptr CSize -> IO CInt)
lua_stringtonumber_hs l r p out =
  reentryIO "lua_stringtonumber" [cstringArg0 p] l r $ \args ->
    do str <- peekCAString p
       case parseNumber str of
         Nothing -> do result out 0
         Just n  -> do push args (Number n)
                       result out (fromIntegral (1 + length str))


foreign export ccall lua_tointegerx_hs
  :: EntryPoint (CInt -> Ptr CInt -> Ptr Lua_Integer -> IO CInt)

-- | [-0, +0, -]
lua_tointegerx_hs :: EntryPoint (CInt -> Ptr CInt -> Ptr Lua_Integer -> IO CInt)
lua_tointegerx_hs l r ix isnum out =
  reentry "lua_integerx" [cArg ix, cArg isnum] l r $ \args ->
  do v <- valueArgument (fromIntegral ix) args
     liftIO $ case valueInt v of
                Just i  -> pokeNotNull isnum 1 >> result out (fromIntegral i)
                Nothing -> pokeNotNull isnum 0 >> result out 0

foreign export ccall lua_toboolean_hs
  :: EntryPoint (CInt -> Ptr CInt -> IO CInt)

-- | [-0, +0, -]
lua_toboolean_hs :: EntryPoint (CInt -> Ptr CInt -> IO CInt)
lua_toboolean_hs l r ix out =
  reentry "lua_toboolean" [cArg ix] l r $ \args ->
  do b <- boolArgument (fromIntegral ix) args
     result out (if b then 1 else 0)

foreign export ccall lua_tonumberx_hs
  :: EntryPoint (CInt -> Ptr CInt -> Ptr Lua_Number -> IO CInt)

-- | [-0, +0, -]
lua_tonumberx_hs :: EntryPoint (CInt -> Ptr CInt -> Ptr Lua_Number -> IO CInt)
lua_tonumberx_hs l r ix isnum out =
  reentry "lua_tonumberx" [cArg ix, cArg isnum] l r $ \args ->
  do v <- valueArgument (fromIntegral ix) args
     liftIO $ case numberToDouble <$> valueNumber v of
                Just d  -> do pokeNotNull isnum 1
                              result out d
                Nothing -> do pokeNotNull isnum 0
                              result out 0

foreign export ccall lua_tolstring_hs
  :: EntryPoint (CInt -> Ptr CSize -> Ptr CString -> IO CInt)

-- | [-0, +0, m]
lua_tolstring_hs :: EntryPoint (CInt -> Ptr CSize -> Ptr CString -> IO CInt)
lua_tolstring_hs l r arg len out =
  reentry "lua_tolstring" [cArg arg, cArg len] l r $ \args ->
  do let idx = fromIntegral arg
     mb <- valueArgument idx args
     case mb of

       String str ->
         do liftIO $ do pokeNotNull len (fromIntegral (luaStringLen str))
                        result out =<< luaStringPtr str

       Number n ->
         do str <- liftIO $
                do str <- fromByteString (B8.pack (numberToString n))
                   pokeNotNull len (fromIntegral (luaStringLen str))
                   return str

            assign idx (String str) args
            liftIO (result out =<< luaStringPtr str)

       _ -> liftIO $
         do pokeNotNull len 0
            result out nullPtr

foreign export ccall lua_touserdata_hs
  :: EntryPoint (CInt -> Ptr (Ptr ()) -> IO CInt)

-- | [-0, +0, -]
lua_touserdata_hs :: EntryPoint (CInt -> Ptr (Ptr ()) -> IO CInt)
lua_touserdata_hs l r arg out =
  reentry "lua_touserdata" [cArg arg] l r $ \args ->
  do v <- valueArgument (fromIntegral arg) args
     ptr <- case v of
              UserData uref ->
                do let fptr = userDataPtr (referenceVal uref)
                   return (unsafeForeignPtrToPtr fptr)
              LightUserData ptr -> return ptr
              _ -> return nullPtr
     result out ptr

--------------------------------------------------------------------------------

foreign export ccall lua_getfield_hs
  :: EntryPoint (CInt -> CString -> Ptr CInt -> IO CInt)

-- [-0, +1, e]
lua_getfield_hs :: EntryPoint (CInt -> CString -> Ptr CInt -> IO CInt)
lua_getfield_hs l r ix k out =
  reentry "lua_getfield" [cArg ix, cstringArg0 k] l r $ \args ->
  do t <- valueArgument (fromIntegral ix) args
     key <- liftIO (peekLuaString0 k)
     v <- mach2 m__index t (String key)
     liftIO $
       do push args v
          result out (lua_type_int v)

foreign export ccall lua_setfield_hs
  :: EntryPoint (CInt -> CString -> IO CInt)

-- | [-1, +0, e]
lua_setfield_hs :: EntryPoint (CInt -> CString -> IO CInt)
lua_setfield_hs l r ix k =
  reentry "lua_setfield" [cArg ix, cstringArg0 k] l r $ \args ->
  do t <- valueArgument (fromIntegral ix) args
     v <- pop args
     key <- liftIO (peekLuaString0 k)
     mach3 m__newindex t (String key) v
     return Nothing

foreign export ccall lua_createtable_hs :: EntryPoint (CInt -> CInt -> IO CInt)

-- | [-0, +1, e]
lua_createtable_hs :: EntryPoint (CInt -> CInt -> IO CInt)
lua_createtable_hs l r na nh =
  reentry "lua_createtable" [cArg na, cArg nh] l r $ \args ->
  do t <- machNewTable (fromIntegral na) (fromIntegral nh)
     push args (Table t)
     return Nothing

foreign export ccall lua_rawgeti_hs
  :: EntryPoint (CInt -> Lua_Integer -> Ptr CInt -> IO CInt)

lua_rawgeti_hs :: EntryPoint (CInt -> Lua_Integer -> Ptr CInt -> IO CInt)
lua_rawgeti_hs l r ix n out =
  reentry "lua_rawgeti" [cArg ix, cArg n] l r $ \args ->
  do t <- tableArgument (fromIntegral ix) args
     liftIO $
       do v <- getTableRaw t (Number (fromIntegral n))
          push args v
          result out (lua_type_int v)

foreign export ccall lua_rawgetp_hs
  :: EntryPoint (CInt -> Ptr () -> Ptr CInt -> IO CInt)

lua_rawgetp_hs :: EntryPoint (CInt -> Ptr () -> Ptr CInt -> IO CInt)
lua_rawgetp_hs l r ix p out =
  reentry "lua_rawgetp" [cArg ix, cArg p] l r $ \args ->
  do t <- tableArgument (fromIntegral ix) args
     liftIO $
       do v <- getTableRaw t (LightUserData p)
          push args v
          result out (lua_type_int v)

foreign export ccall lua_rawget_hs
  :: EntryPoint (CInt -> Ptr CInt -> IO CInt)

lua_rawget_hs :: EntryPoint (CInt -> Ptr CInt -> IO CInt)
lua_rawget_hs l r ix out =
  reentry "lua_rawget" [cArg ix] l r $ \args ->
  do t <- tableArgument (fromIntegral ix) args
     k <- pop args
     liftIO $
       do v <- getTableRaw t k
          push args v
          result out (lua_type_int v)

foreign export ccall lua_geti_hs
  :: EntryPoint (CInt -> Lua_Integer -> Ptr CInt -> IO CInt)

lua_geti_hs :: EntryPoint (CInt -> Lua_Integer -> Ptr CInt -> IO CInt)
lua_geti_hs l r ix n out =
  reentry "lua_geti" [cArg ix, cArg n] l r $ \args ->
  do t <- valueArgument (fromIntegral ix) args
     v <- mach2 m__index t (Number (fromIntegral n))
     liftIO $
       do push args v
          result out (lua_type_int v)

foreign export ccall lua_gettable_hs
  :: EntryPoint (CInt -> Ptr CInt -> IO CInt)

lua_gettable_hs :: EntryPoint (CInt -> Ptr CInt -> IO CInt)
lua_gettable_hs l r ix out =
  reentry "lua_gettable" [cArg ix] l r $ \args ->
  do t <- valueArgument (fromIntegral ix) args
     k <- pop args
     v <- mach2 m__index t k
     liftIO $
       do push args v
          result out (lua_type_int v)

foreign export ccall lua_rawset_hs
  :: EntryPoint (CInt -> IO CInt)

lua_rawset_hs :: EntryPoint (CInt -> IO CInt)
lua_rawset_hs l r ix =
  reentry "lua_rawset" [cArg ix] l r $ \args ->
  do t <- tableArgument (fromIntegral ix) args
     kv <- liftIO (popN args 2)
     case kv of
       [Number (Double nan),_] | isNaN nan -> luaError "Invalid table index NaN"
       [Nil                ,_]             -> luaError "Invalid table index nil"
       [k,v]                               -> Nothing <$ setTableRaw t k v
       _                                   -> luaError "lua_rawset: missing arguments"

foreign export ccall lua_seti_hs
  :: EntryPoint (CInt -> Lua_Integer -> IO CInt)

lua_seti_hs :: EntryPoint (CInt -> Lua_Integer -> IO CInt)
lua_seti_hs l r ix n =
  reentry "lua_seti" [cArg ix, cArg n] l r $ \args ->
  do t <- valueArgument (fromIntegral ix) args
     v <- pop args
     mach3 m__newindex t (Number (fromIntegral n)) v
     return Nothing

foreign export ccall lua_rawseti_hs
  :: EntryPoint (CInt -> Lua_Integer -> IO CInt)

lua_rawseti_hs :: EntryPoint (CInt -> Lua_Integer -> IO CInt)
lua_rawseti_hs l r ix n =
  reentry "lua_rawseti" [cArg ix, cArg n] l r $ \args ->
  do t <- tableArgument (fromIntegral ix) args
     v <- pop args
     setTableRaw t (Number (fromIntegral n)) v
     return Nothing

foreign export ccall lua_rawsetp_hs
  :: EntryPoint (CInt -> Ptr () -> IO CInt)

lua_rawsetp_hs :: EntryPoint (CInt -> Ptr () -> IO CInt)
lua_rawsetp_hs l r ix p =
  reentry "lua_rawsetp" [cArg ix, cArg p] l r $ \args ->
  do t <- tableArgument (fromIntegral ix) args
     v <- pop args
     setTableRaw t (LightUserData p) v
     return Nothing

------------------------------------------------------------------------

foreign export ccall lua_rawequal_hs
  :: EntryPoint (CInt -> CInt -> Ptr CInt -> IO CInt)

lua_rawequal_hs :: EntryPoint (CInt -> CInt -> Ptr CInt -> IO CInt)
lua_rawequal_hs l r ix1 ix2 out =
  reentry "lua_rawequal" [cArg ix1, cArg ix2] l r $ \args ->
  do x <- valueArgumentOpt (fromIntegral ix1) args
     y <- valueArgumentOpt (fromIntegral ix2) args
     result out $
       case (x,y) of
          (Just a, Just b) | a == b -> 1
          _                         -> 0

------------------------------------------------------------------------

foreign export ccall lua_compare_hs
  :: EntryPoint (CInt -> CInt -> CInt -> Ptr CInt -> IO CInt)

-- [-0, +0, e]
lua_compare_hs :: EntryPoint (CInt -> CInt -> CInt -> Ptr CInt -> IO CInt)
lua_compare_hs l r ix1 ix2 op out =
  reentry "lua_compare" [cArg ix1, cArg ix2, cArg op] l r $ \args ->
  -- Note: Also returns 0 if any of the indices is not valid.
  do mbx <- valueArgumentOpt (fromIntegral ix1) args
     mby <- valueArgumentOpt (fromIntegral ix2) args
     case (mbx,mby) of
       (Just x, Just y) ->
         do f <- case op of
                   #{const LUA_OPEQ} -> return $ mach2 m__eq
                   #{const LUA_OPLT} -> return $ mach2 m__lt
                   #{const LUA_OPLE} -> return $ mach2 m__le
                   _                 -> luaError "lua_compare: bad operator"
            res <- f x y
            result out (if res then 1 else 0)
       _ -> result out 0

------------------------------------------------------------------------

foreign export ccall lua_arith_hs :: EntryPoint (CInt -> IO CInt)

-- [-(2|1), +1, e]
lua_arith_hs :: EntryPoint (CInt -> IO CInt)
lua_arith_hs l r opNum =
  reentry "lua_arith" [cArg opNum] l r $
    (Nothing <$) .
    case opNum of
      #{const LUA_OPADD}  -> op2 m__add
      #{const LUA_OPSUB}  -> op2 m__sub
      #{const LUA_OPMUL}  -> op2 m__mul
      #{const LUA_OPDIV}  -> op2 m__div
      #{const LUA_OPIDIV} -> op2 m__idiv
      #{const LUA_OPMOD}  -> op2 m__mod
      #{const LUA_OPPOW}  -> op2 m__pow
      #{const LUA_OPUNM}  -> op1 m__unm
      #{const LUA_OPBAND} -> op2 m__band
      #{const LUA_OPBOR}  -> op2 m__bor
      #{const LUA_OPBXOR} -> op2 m__bxor
      #{const LUA_OPSHL}  -> op2 m__shl
      #{const LUA_OPSHR}  -> op2 m__shr
      #{const LUA_OPBNOT} -> op1 m__bnot
      _                   -> \_ -> luaError "lua_arith: bad operator"
  where
  op1 f args = do x <- pop args
                  push args =<< mach1 f x

  op2 f args = do y <- pop args
                  x <- pop args
                  push args =<< mach2 f x y


foreign export ccall lua_getmetatable_hs
  :: EntryPoint (CInt -> Ptr CInt -> IO CInt)

lua_getmetatable_hs :: EntryPoint (CInt -> Ptr CInt -> IO CInt)
lua_getmetatable_hs l r ix out =
  reentry "lua_getmetatable" [cArg ix] l r $ \args ->
  do v <- valueArgument (fromIntegral ix) args
     mt <- machD1 valueMetatable v
     case mt of
       Nothing -> do result out 0
       Just m  -> do push args (Table m)
                     result out 1

------------------------------------------------------------------------

foreign export ccall lua_newuserdata_hs
  :: EntryPoint (CSize -> Ptr (Ptr ()) -> IO CInt)

-- | [-0, +1, e]
lua_newuserdata_hs :: EntryPoint (CSize -> Ptr (Ptr ()) -> IO CInt)
lua_newuserdata_hs l r sz out =
  reentry "lua_newuserdata" [cArg sz] l r $ \args ->
  do let sz' = fromIntegral sz
     fptr <- liftIO (mallocForeignPtrBytes sz')
     u <- machNewUserData fptr sz'

     garbage <- getsMachEnv machGarbage
     liftIO (addRefFinalizer u (atomicModifyIORef garbage (\xs -> (UserData u : xs, ()))))

     push args (UserData u)
     result out (unsafeForeignPtrToPtr fptr)

foreign export ccall lua_getuservalue_hs
  :: EntryPoint (CInt -> Ptr CInt -> IO CInt)

lua_getuservalue_hs :: EntryPoint (CInt -> Ptr CInt -> IO CInt)
lua_getuservalue_hs l r ix out =
  reentry "lua_getuservalue" [cArg ix] l r $ \args ->
  do v <- valueArgument (fromIntegral ix) args
     case v of
       UserData uref ->
          do uservalue <- liftIO (readIORef (userDataValue (referenceVal uref)))
             push args uservalue
             result out (lua_type_int uservalue)
       _ -> luaError "lua_getuservalue: argument is not full userdata"

foreign export ccall lua_setuservalue_hs
  :: EntryPoint (CInt -> IO CInt)

lua_setuservalue_hs :: EntryPoint (CInt -> IO CInt)
lua_setuservalue_hs l r ix =
  reentry "lua_setuservalue" [cArg ix] l r $ \args ->
  do v         <- valueArgument (fromIntegral ix) args
     uservalue <- pop args
     case v of
       UserData uref ->
         liftIO (Nothing <$ writeIORef (userDataValue (referenceVal uref)) uservalue)
       _ -> luaError "lua_setuservalue: argument is not full userdata"

------------------------------------------------------------------------

lua_multret :: CInt
lua_multret = (#const LUA_MULTRET)

foreign export ccall lua_callk_hs
  :: EntryPoint (CInt -> CInt -> Lua_KContext -> Lua_KFunction -> IO CInt)

-- [-(nargs+1), +nresults, e]
lua_callk_hs :: EntryPoint (CInt -> CInt -> Lua_KContext -> Lua_KFunction -> IO CInt)
lua_callk_hs l r narg nresult ctx k =
  reentry "lua_callk" [cArg narg, cArg nresult, cArg ctx, cArg k] l r $ \args ->
  do fxs <- popN args (fromIntegral narg + 1)
     case fxs of
       f:xs ->
         do rs <- mach2 m__call f xs
            traverse_ (push args)
                $ if nresult == lua_multret
                    then rs
                    else take (fromIntegral nresult) (rs ++ repeat Nil)
            return Nothing
       _ -> luaError "lua_callk: invalid narg"

foreign export ccall lua_pcallk_hs
  :: EntryPoint (CInt -> CInt -> CInt -> Ptr CInt -> IO CInt)

-- | [-0, +1, e]
lua_pcallk_hs ::
  EntryPoint (CInt -> CInt -> CInt -> Ptr CInt -> IO CInt)
lua_pcallk_hs l r narg nresult msgh out =
  reentry "lua_callk" [cArg narg, cArg nresult, cArg msgh] l r $ \args ->
  do h <- if msgh == 0
            then return DefaultHandler
            else FunHandler <$> functionArgument (fromIntegral msgh) args
     fxs <- popN args (fromIntegral narg + 1)
     case fxs of
       f:xs ->
         do (f',xs') <- mach2 resolveFunction f xs
            res <- machTry h f' xs'
            case res of
              Left e ->
                do push args e
                   result out luaERRRUN
              Right rs ->
                do traverse_ (push args)
                     $ if nresult == lua_multret
                           then rs
                           else take (fromIntegral nresult) (rs ++ repeat Nil)
                   result out luaOK
       _ -> luaError "lua_pcallk: invalid narg"

------------------------------------------------------------------------

foreign export ccall lua_setmetatable_hs :: EntryPoint (CInt -> IO CInt)

-- | [-1, +0, e]
lua_setmetatable_hs :: EntryPoint (CInt -> IO CInt)
lua_setmetatable_hs l r idx =
  reentry "lua_setmetatable" [cArg idx] l r $ \args ->
  do v <- valueArgument (fromIntegral idx) args
     m <- pop args
     case m of -- must be non-empty becuase previous line worked
       Nil     -> machD2 setMetatable Nothing v
       Table t -> machD2 setMetatable (Just t) v
       _       -> luaError "lua_setmetatable expected table argument"
     return Nothing

------------------------------------------------------------------------

foreign export ccall lua_len_hs :: EntryPoint (CInt -> IO CInt)

-- | [-0, +1, e]
lua_len_hs :: EntryPoint (CInt -> IO CInt)
lua_len_hs l r idx =
  reentry "lua_len" [cArg idx] l r $ \args ->
  do v <- valueArgument (fromIntegral idx) args
     len <- mach1 m__len v
     liftIO (push args len)
     return Nothing

foreign export ccall lua_rawlen_hs :: EntryPoint (CInt -> Ptr CSize -> IO CInt)

-- | [-0, +0, –]
lua_rawlen_hs :: EntryPoint (CInt -> Ptr CSize -> IO CInt)
lua_rawlen_hs l r idx out =
  reentry "lua_rawlen" [cArg idx] l r $ \args ->
  do v <- valueArgument (fromIntegral idx) args
     liftIO $
       do len <- case v of
                   String xs  -> return (luaStringLen xs)
                   Table t    -> tableLen t
                   UserData u -> return (userDataSize (referenceVal u))
                   _          -> return 0
          result out (fromIntegral len)

------------------------------------------------------------------------

-- int lua_load (lua_State *L, lua_Reader reader, void *data, const char *chunkname, const char *mode) {
foreign export ccall lua_load_hs ::
  EntryPoint (CString -> CSize -> CString -> CString -> Ptr CInt -> IO CInt)

lua_load_hs :: EntryPoint (CString -> CSize -> CString -> CString -> Ptr CInt -> IO CInt)
lua_load_hs l r chunk chunksize chunkname mode out =
  -- Note: this is an internal entry point, the C api handles the Reader
  reentry "lua_load" [cstringArg0 chunkname, cstringArg0 mode] l r
     $ \args ->
    do chunk1<- liftIO (B.packCStringLen (chunk, fromIntegral chunksize))
       name  <- if nullPtr == chunkname
                  then return Nothing
                  else liftIO (Just <$> peekCAString chunkname)
       menv  <- machCurrentEnv
       globals <- getsMachEnv machGlobals
       eclo <- chunkToClosure menv name chunk1 (Table globals)
       liftIO $ case eclo of
         Left e    -> do e' <- fromByteString (B8.pack e)
                         push args (String e')
                         result out 1
         Right clo -> do push args (Closure clo)
                         result out 0


------------------------------------------------------------------------

foreign export ccall lua_setglobal_hs :: EntryPoint (CString -> IO CInt)

lua_setglobal_hs :: EntryPoint (CString -> IO CInt)
lua_setglobal_hs l r cname =
  reentry "lua_setglobal" [cstringArg0 cname] l r $ \args ->
  do v <- pop args
     t <- getsMachEnv machGlobals
     liftIO $
       do k <- peekLuaString0 cname
          setTableRaw t (String k) v
          return Nothing

foreign export ccall lua_getglobal_hs :: EntryPoint (CString -> Ptr CInt -> IO CInt)

lua_getglobal_hs :: EntryPoint (CString -> Ptr CInt -> IO CInt)
lua_getglobal_hs l r cname out =
  reentry "lua_getglobal" [cstringArg0 cname] l r $ \args ->
  do t <- getsMachEnv machGlobals
     liftIO $
       do k <- peekLuaString0 cname
          v <- getTableRaw t (String k)
          push args v
          result out (lua_type_int v)

------------------------------------------------------------------------

foreign export ccall lua_concat_hs :: EntryPoint (CInt -> IO CInt)

-- [-n, +1, e]
lua_concat_hs :: EntryPoint (CInt -> IO CInt)
lua_concat_hs l r n =
  reentry "lua_concat" [cArg n] l r $ \args ->
  do xs <- popN args (fromIntegral n)
     res <- mach1 m__concat xs
     push args res
     return Nothing

------------------------------------------------------------------------

foreign export ccall lua_dump_hs ::
  EntryPoint (FunPtr LuaWriter -> Ptr () -> CInt -> Ptr CInt -> IO CInt)

type LuaWriter =
  Ptr () {- ^ lua_State *L  -} ->
  Ptr () {- ^ const void* p -} ->
  CSize  {- ^ size_t sz     -} ->
  Ptr () {- ^ void* ud      -} ->
  IO CInt

type Dynamic a = FunPtr a -> a
foreign import ccall "dynamic" lua_writer :: Dynamic LuaWriter

-- [-0, +0, -]
lua_dump_hs :: EntryPoint (FunPtr LuaWriter -> Ptr () -> CInt -> Ptr CInt -> IO CInt)
lua_dump_hs l r writer dat strip out =
  reentry "lua_dump" [cArg writer, cArg dat, cArg strip] l r $ \args ->
    do clo <- functionArgument (-1) args
       fun <- case luaOpCodes (cloFun (referenceVal clo)) of
                Nothing -> luaError "expected lua function" -- ?
                Just (_,fun) -> return fun
       let bytecode = dumpLuaBytecode
                        luaBytecodeMode53
                        (Chunk (length (funcUpvalues fun)) fun)
           loop [] = result out 0
           loop (x:xs) =
             U.unsafeUseAsCStringLen x $ \(ptr, len) ->
               do res <- lua_writer writer l (castPtr ptr) (fromIntegral len) dat
                  if res == 0 then
                    loop xs
                  else
                    result out (fromIntegral res)

       liftIO (loop (L.toChunks bytecode))


------------------------------------------------------------------------

pokeNotNull :: Storable a => Ptr a -> a -> IO ()
pokeNotNull ptr a = unless (ptr == nullPtr) (poke ptr a)

--------------------------------------------------------------------------------
-- Coroutines


foreign export ccall
  lua_tothread_hs :: EntryPoint (CInt -> Ptr (Ptr ()) -> IO CInt)

lua_tothread_hs :: EntryPoint (CInt -> Ptr (Ptr ()) -> IO CInt)
lua_tothread_hs l r n out =
  reentry "lua_tothread" [cArg n] l r $ \args ->
    do v <- valueArgument (fromIntegral n) args
       result out $!
         case v of
           Thread t -> unsafeForeignPtrToPtr (threadCPtr (referenceVal t))
           _        -> nullPtr

foreign export ccall
  lua_pushthread_hs :: EntryPoint (Ptr CInt -> IO CInt)

lua_pushthread_hs :: EntryPoint (Ptr CInt -> IO CInt)
lua_pushthread_hs l r out =
  reentry "lua_pushthread" [] l r $ \args ->
    do ref    <- machCurrentThread
       isMain <- machIsMainThread ref
       push args (Thread ref)
       result out (if isMain then 1 else 0)


foreign export ccall
  lua_status_hs :: EntryPoint (Ptr CInt -> IO CInt)

lua_status_hs :: EntryPoint (Ptr CInt -> IO CInt)
lua_status_hs l r out =
  reentry "lua_status" [] l r $ \_ ->
     do thread <- extToThreadRef =<< liftIO (deRefLuaState l)
        st <- getThreadField threadStatus thread

        result out $
          case st of
            ThreadSuspended{} -> luaYIELD
            ThreadNormal   {} -> luaOK
            ThreadRunning  {} -> luaOK
            ThreadNew      {} -> luaOK
            ThreadCrashed  {} -> luaERRRUN

foreign export ccall
  lua_resume_hs :: EntryPoint (Ptr () -> CInt -> Ptr CInt -> IO CInt)

lua_resume_hs :: EntryPoint (Ptr () -> CInt -> Ptr CInt -> IO CInt)
lua_resume_hs l r from nargs out =
  reentry "lua_resume" [cArg from, cArg nargs] l r $ \args ->
    do tRef <- extToThreadRef =<< liftIO (deRefLuaState l)

       st <- getThreadField threadStatus tRef
       case st of
         ThreadSuspended _ ->
            do doResume tRef args

         ThreadNew ->
            do resumeArgs <- popN args (fromIntegral nargs)

               closure <- functionArgument (-1) args
               void (pop args)

               activateThread closure resumeArgs tRef
               doResume tRef args

         _ -> do e <- liftIO (fromByteString "Thread not resumable")
                 finishWithError args (String e)

  where
  finishWithError args e =
    do push args e
       result out luaERRRUN

  doResume tRef leftover =
    do res <- machResume tRef
       case res of
         ThreadReturn rs ->
           do eenv <- getThreadField stExecEnv tRef
              liftIO (stackFromList (execStack eenv) rs)
              result out luaOK

         ThreadYield -> result out luaYIELD

         ThreadError e -> finishWithError leftover e


foreign export ccall
  lua_yieldk_hs :: EntryPoint (CInt -> Lua_KContext -> Lua_KFunction -> IO CInt)

lua_yieldk_hs :: EntryPoint (CInt -> Lua_KContext -> Lua_KFunction -> IO CInt)
lua_yieldk_hs l r nResults ctx func =
  reentry "lua_yieldk" [cArg nResults, cArg ctx, cArg func] l r $ \args ->
    do outputs <- popN args (fromIntegral nResults)

       tRef <- extToThreadRef =<< liftIO (deRefLuaState l)
       stack <- execStack <$> getThreadField stExecEnv tRef
       liftIO $ do n <- SV.size stack
                   SV.shrink stack n
                   traverse_ (push stack) outputs

       machYield

       inputs <- liftIO (stackToList stack)
       if func == nullFunPtr
         then machReturn inputs
         else fail "Panic: lua_yieldk with continuation not implemented" -- XXX


foreign export ccall
  lua_newthread_hs :: EntryPoint (Ptr (Ptr ()) -> IO CInt)

lua_newthread_hs :: EntryPoint (Ptr (Ptr ()) -> IO CInt)
lua_newthread_hs l r out =
  reentry "lua_newthread" [] l r $ \args ->
    do threadRef <- machNewThread
       push args (Thread threadRef)
       result out (unsafeForeignPtrToPtr (threadCPtr (referenceVal threadRef)))

foreign export ccall
  lua_isyieldable_hs :: EntryPoint (Ptr CInt -> IO CInt)

lua_isyieldable_hs :: EntryPoint (Ptr CInt -> IO CInt)
lua_isyieldable_hs l r out =
  reentry "lua_isyieldable" [] l r $ \_ ->
    do tRef <- extToThreadRef =<< liftIO (deRefLuaState l)

       isMain <- machIsMainThread tRef
       isYieldable <- if isMain
                           then return False
                           else isThreadRunning <$> getThreadField threadStatus tRef
       result out (if isYieldable then 1 else 0)


------------------------------------------------------------------------
-- Debug API

data LuaDebug

pokeLuaDebugCallInfo :: Ptr LuaDebug -> Ptr () -> IO ()
pokeLuaDebugCallInfo = #poke struct lua_Debug, i_ci

peekLuaDebugCallInfo :: Ptr LuaDebug -> IO (Ptr ())
peekLuaDebugCallInfo = #peek struct lua_Debug, i_ci

pokeLuaDebugSource :: Ptr LuaDebug -> CString -> IO ()
pokeLuaDebugSource = #poke struct lua_Debug, source

pokeLuaDebugShortSrc :: Ptr LuaDebug -> String -> IO ()
pokeLuaDebugShortSrc ar str =
  do let arraySize = (#const LUA_IDSIZE)
         ptr = (#ptr struct lua_Debug, short_src) ar :: CString
     pokeArray0 0 ptr (map (fromIntegral . fromEnum) (take (arraySize-1) str))

pokeLuaDebugLineDefined :: Ptr LuaDebug -> CInt -> IO ()
pokeLuaDebugLineDefined = #poke struct lua_Debug, linedefined

pokeLuaDebugLastLineDefined :: Ptr LuaDebug -> CInt -> IO ()
pokeLuaDebugLastLineDefined = #poke struct lua_Debug, lastlinedefined

pokeLuaDebugCurrentLine :: Ptr LuaDebug -> CInt -> IO ()
pokeLuaDebugCurrentLine = #poke struct lua_Debug, currentline

pokeLuaDebugIsTailCall :: Ptr LuaDebug -> CChar -> IO ()
pokeLuaDebugIsTailCall = #poke struct lua_Debug, istailcall

pokeLuaDebugName :: Ptr LuaDebug -> CString -> IO ()
pokeLuaDebugName = #poke struct lua_Debug, name

pokeLuaDebugNameWhat :: Ptr LuaDebug -> CString -> IO ()
pokeLuaDebugNameWhat = #poke struct lua_Debug, namewhat

pokeLuaDebugWhat :: Ptr LuaDebug -> CString -> IO ()
pokeLuaDebugWhat = #poke struct lua_Debug, namewhat

pokeLuaDebugNUps :: Ptr LuaDebug -> CUChar -> IO ()
pokeLuaDebugNUps = #poke struct lua_Debug, nups

pokeLuaDebugNParams :: Ptr LuaDebug -> CUChar -> IO ()
pokeLuaDebugNParams = #poke struct lua_Debug, nparams

pokeLuaDebugIsVarArg :: Ptr LuaDebug -> CChar -> IO ()
pokeLuaDebugIsVarArg = #poke struct lua_Debug, isvararg

findExecEnv :: Int -> Reference Thread -> IO (Maybe (Int,ExecEnv))
findExecEnv level thread =
  case compare level 0 of
    LT -> return Nothing
    EQ -> do pc <- getThreadField stPC thread
             eenv <- getThreadField stExecEnv thread
             return (Just (pc, eenv))
    GT -> do stack <- getThreadField stStack thread
             return $! go (level-1) (toList stack)

  where
  go 0 (CallFrame pc execEnv _ _ : _) = Just (pc, execEnv)
  go l (CallFrame {} : xs)            = go (l-1) xs
  go _ []                             = Nothing
  go l (ErrorFrame  {} : xs)          = go l xs


exportExecEnv :: (Int,ExecEnv) -> IO (Ptr ())
exportExecEnv e = castStablePtrToPtr <$> newStablePtr e -- XXX: memory leak

importExecEnv :: Ptr () -> IO (Int,ExecEnv)
importExecEnv = deRefStablePtr . castPtrToStablePtr

------------------------------------------------------------------------

foreign export ccall
  lua_getstack_hs :: EntryPoint (CInt -> Ptr LuaDebug -> Ptr CInt -> IO CInt)

lua_getstack_hs :: EntryPoint (CInt -> Ptr LuaDebug -> Ptr CInt -> IO CInt)
lua_getstack_hs l r level ar out =
  reentry "lua_getstack" [cArg level, cArg (castPtr ar :: Ptr ())] l r $ \_ ->

  do tRef <- extToThreadRef =<< liftIO (deRefLuaState l)
     liftIO $
       do mbExecEnv <- findExecEnv (fromIntegral level) tRef

          case mbExecEnv of
            -- get stack doesn't access a thread's initial stack for whatever reason
            Just execEnv | not (isNullCFunction (execFunction (snd execEnv))) ->
              do pokeLuaDebugCallInfo ar =<< exportExecEnv execEnv
                 result out 1

            _ -> result out 0


isNullCFunction :: FunctionValue -> Bool
isNullCFunction x = case funValueName x of
                      CFID name -> cfunAddr name == nullFunPtr
                      _         -> False

------------------------------------------------------------------------

foreign export ccall
  lua_getinfo_hs :: EntryPoint (CString -> Ptr LuaDebug -> Ptr CInt -> IO CInt)

lua_getinfo_hs :: EntryPoint (CString -> Ptr LuaDebug -> Ptr CInt -> IO CInt)
lua_getinfo_hs l r whatPtr ar out =
  reentry "lua_getinfo" [cstringArg0 whatPtr, cArg (castPtr ar :: Ptr ())] l r $ \args ->
  do what         <- liftIO (peekCString whatPtr)

     (pc,execEnv) <- if '>'`elem` what
                       then do th <- extToThreadRef =<< liftIO (deRefLuaState l)
                               pc <- getThreadField stPC th
                               eenv <- getThreadField stExecEnv th
                               return (pc,eenv)
                       else liftIO (importExecEnv =<< peekLuaDebugCallInfo ar)

     let luaWhat fid = if isRootFun fid then "main" else "Lua"

     let funVal = execFunction execEnv
     liftIO $
       case (funValueName funVal) of

         LuaFID fid -> liftIO $
           do let Just (_,fun) = luaOpCodes funVal
              when ('n' `elem` what) $
                do pokeLuaDebugName            ar =<< newCAString ("(FID " ++ show fid ++ ")") -- track this
                   pokeLuaDebugNameWhat        ar =<< newCAString (luaWhat fid)

              when ('S' `elem` what) $
                do pokeLuaDebugSource          ar =<< newCAString (maybe "=(unknown source)" B8.unpack (funcSource fun))
                   pokeLuaDebugShortSrc        ar (maybe "(unknown source)" B8.unpack (funcSource fun))
                   pokeLuaDebugLineDefined     ar (fromIntegral (funcLineDefined fun))
                   pokeLuaDebugLastLineDefined ar (fromIntegral (funcLastLineDefined fun))
                   pokeLuaDebugWhat            ar =<< newCAString (luaWhat fid)

              when ('l' `elem` what) $
                do pokeLuaDebugCurrentLine     ar (maybe 0 fromIntegral (lookupLineNumber fun pc))

              when ('t' `elem` what) $
                do pokeLuaDebugIsTailCall      ar 0 -- XXX: Track tail calls

              when ('u' `elem` what) $
                do pokeLuaDebugNUps            ar (fromIntegral (IOVector.length (execUpvals execEnv)))
                   pokeLuaDebugNParams         ar (fromIntegral (funcNumParams fun))
                   pokeLuaDebugIsVarArg        ar (if funcIsVararg fun then 1 else 0)


         CFID cfun ->
           do let funName = cfunName cfun
                  funNameStr = fromMaybe (cObjAddr funName) (cObjName funName)

              when ('n' `elem` what) $
                do pokeLuaDebugName            ar =<< newCAString funNameStr
                   pokeLuaDebugNameWhat        ar =<< newCAString "C"

              when ('S' `elem` what) $
                do pokeLuaDebugSource          ar =<< newCAString "=(unknown source)" -- XXX
                   pokeLuaDebugShortSrc        ar funNameStr
                   pokeLuaDebugLineDefined     ar 0 -- XXX
                   pokeLuaDebugLastLineDefined ar 0 -- XXX
                   pokeLuaDebugWhat            ar =<< newCAString "C"

              when ('l' `elem` what) $
                do pokeLuaDebugCurrentLine     ar (-1)
                       -- When no line information is available, currentline is set to -1

              when ('t' `elem` what) $
                do pokeLuaDebugIsTailCall      ar 0 -- XXX: Track tail calls

              when ('u' `elem` what) $
                do pokeLuaDebugNUps            ar (fromIntegral (IOVector.length (execUpvals execEnv)))
                   pokeLuaDebugNParams         ar 0 -- always 0 for C functions
                   pokeLuaDebugIsVarArg        ar 1 -- always true for C functions


     when ('f'`elem`what) (push args (execClosure execEnv))
     when ('L'`elem`what) (push args Nil)
     result out 1

------------------------------------------------------------------------

foreign export ccall
  lua_getlocal_hs :: EntryPoint (Ptr LuaDebug -> CInt -> Ptr CString -> IO CInt)

lua_getlocal_hs :: EntryPoint (Ptr LuaDebug -> CInt -> Ptr CString -> IO CInt)
lua_getlocal_hs l r ar n out =
  reentry "lua_getlocal" [cArg (castPtr ar :: Ptr ()), cArg n] l r $ \args ->
    Nothing <$
      if ar == nullPtr
        then getLocalFunArgs (fromIntegral n) out args
        else getLocalStackArgs (fromIntegral n) out args ar

getLocalFunArgs :: Int -> Ptr CString -> SV.SizedVector (IORef Value) -> Mach ()
getLocalFunArgs n out args =
  do clo <- functionArgument (-1) args
     void (pop args)
     liftIO $ case luaOpCodes (cloFun (referenceVal clo)) of
       Just (_,fun) ->
         case lookupLocalName fun 0 (Reg (n-1)) of
           Nothing -> poke out nullPtr
           Just bs -> poke out =<< newCAString (B8.unpack bs)--XXX: Leak
       _ -> poke out nullPtr

getLocalStackArgs :: Int -> Ptr CString -> SV.SizedVector (IORef Value) -> Ptr LuaDebug -> Mach ()
getLocalStackArgs n out args ar =
  do (pc,execEnv) <- liftIO (importExecEnv =<< peekLuaDebugCallInfo ar)

     let stack = execStack execEnv

     let ix = fromIntegral n - 1

     liftIO $ case luaOpCodes (execFunction execEnv) of
       Just (_,func)
         | Just name <- lookupLocalName func pc (Reg ix) ->
             do len <- SV.size stack
                if 0 <= ix && ix < len
                  then do cell <- SV.get stack ix
                          poke out =<< newCAString (B8.unpack name)
                          val <- readIORef cell
                          push args val

                  else do poke out nullPtr

       _ -> do poke out nullPtr

------------------------------------------------------------------------

foreign export ccall
  lua_getupvalue_hs :: EntryPoint (CInt -> CInt -> Ptr CString -> IO CInt)

lua_getupvalue_hs :: EntryPoint (CInt -> CInt -> Ptr CString -> IO CInt)
lua_getupvalue_hs l r funcindex n out =
  reentry "lua_getupvalue" [cArg funcindex, cArg n] l r $ \args ->
    do v <- valueArgument (fromIntegral funcindex) args

       let n' = fromIntegral n - 1

       result out =<<
         case v of
           Closure ref -> liftIO $
              do let clo = referenceVal ref
                     ups = cloUpvalues clo

                 mb <- IOVector.readMaybe ups n'
                 case mb of
                   Just uv ->
                     do push args =<< readIORef uv
                        newCAString (upvalueName (cloFun clo) n') -- XXX: Leak
                   Nothing -> return nullPtr

           _ -> return nullPtr

upvalueName :: FunctionValue -> Int -> String
upvalueName fv n =
  case luaOpCodes fv of
    Just (_,fun) ->
      case debugInfoUpvalues (funcDebug fun) Vector.!? n of
        Just bs -> B8.unpack bs
        Nothing -> ""
    _ -> ""

------------------------------------------------------------------------

foreign export ccall
  lua_setlocal_hs :: EntryPoint (Ptr LuaDebug -> CInt -> Ptr CString -> IO CInt)

lua_setlocal_hs :: EntryPoint (Ptr LuaDebug -> CInt -> Ptr CString -> IO CInt)
lua_setlocal_hs l r ar n out =
  reentry "lua_setlocal" [cArg (castPtr ar :: Ptr ()), cArg n] l r $ \args ->

  do (pc,execEnv) <- liftIO (importExecEnv =<< peekLuaDebugCallInfo ar)

     let stack = execStack execEnv

     let ix = fromIntegral n - 1

     result out =<<
      case luaOpCodes (execFunction execEnv) of
       Just (_,func)
         | Just name <- lookupLocalName func pc (Reg ix) ->
           do mb <- liftIO (SV.getMaybe stack ix)
              case mb of
                Nothing -> return nullPtr
                Just cell ->
                     do v <- pop args
                        liftIO (writeIORef cell v >> newCAString (B8.unpack name))

       _ -> return nullPtr

------------------------------------------------------------------------

foreign export ccall
  lua_setupvalue_hs :: EntryPoint (CInt -> CInt -> Ptr CString -> IO CInt)

lua_setupvalue_hs :: EntryPoint (CInt -> CInt -> Ptr CString -> IO CInt)
lua_setupvalue_hs l r funcindex n out =
  reentry "lua_setupvalue" [cArg funcindex, cArg n] l r $ \args ->

    do v <- valueArgument (fromIntegral funcindex) args

       let n' = fromIntegral n - 1

       result out =<<
        case v of
         Closure ref ->
            do let clo = referenceVal ref
                   ups = cloUpvalues clo

               mb <- liftIO (IOVector.readMaybe ups n')
               case mb of
                 Just uv ->
                   do x <- pop args
                      liftIO $
                        do writeIORef uv x
                           newCAString (upvalueName (cloFun clo) n') -- XXX: Leak

                 Nothing -> return nullPtr

         _ -> return nullPtr

------------------------------------------------------------------------

foreign export ccall
  lua_xmove_hs :: EntryPoint (Ptr () -> CInt -> IO CInt)

lua_xmove_hs :: EntryPoint (Ptr () -> CInt -> IO CInt)
lua_xmove_hs l r to n =
  reentry "lua_xmove" [cArg to, cArg n] l r $ \fromArgs ->

    do fromRef <- extToThreadRef =<< liftIO (deRefLuaState l)
       toRef   <- extToThreadRef =<< liftIO (deRefLuaState to)

       liftIO $
        unless (fromRef == toRef) $
          do transfer <- popN fromArgs (fromIntegral n)

             toStack <- execStack <$> getThreadField stExecEnv toRef
             traverse_ (push toStack) transfer
       return Nothing

------------------------------------------------------------------------

foreign export ccall
  lua_upvaluejoin_hs :: EntryPoint (CInt -> CInt -> CInt -> CInt -> IO CInt)

-- Make the n1-th upvalue of the Lua closure at index funcindex1 refer to
-- the n2-th upvalue of the Lua closure at index funcindex2.
-- [-0, +0, –]
lua_upvaluejoin_hs :: EntryPoint (CInt -> CInt -> CInt -> CInt -> IO CInt)
lua_upvaluejoin_hs l r f1 n1 f2 n2 =
  reentry "lua_upvaluejoin" (cArg <$> [f1,n1,f2,n2]) l r $ \args ->
    do f1' <- valueArgument (fromIntegral f1) args
       f2' <- valueArgument (fromIntegral f2) args
       let n1' = fromIntegral n1 - 1
           n2' = fromIntegral n2 - 1
       liftIO $ case (f1',f2') of
         (Closure ref1, Closure ref2) ->
           do let v1 = cloUpvalues (referenceVal ref1)
                  v2 = cloUpvalues (referenceVal ref2)

              mbRef <- IOVector.readMaybe v2 n2'
              for_ mbRef $ \ ref ->
                when (0 <= n1' && n1' < IOVector.length v1)
                     (IOVector.write v1 n1' ref)

         _ -> return ()
       return Nothing

replaceAt :: Int -> a -> Vector a -> Maybe (Vector a)
replaceAt i x v
  | 0 <= i && i < Vector.length v = Just $! v Vector.// [(i,x)]
  | otherwise = Nothing

------------------------------------------------------------------------

foreign export ccall
  lua_upvalueid_hs :: EntryPoint (CInt -> CInt -> Ptr (Ptr ()) -> IO CInt)

lua_upvalueid_hs :: EntryPoint (CInt -> CInt -> Ptr (Ptr ()) -> IO CInt)
lua_upvalueid_hs l r funcindex n _out =
  reentry "lua_upvalueid" (cArg <$> [funcindex,n]) l r $ \_ ->
    luaError "lua_upvalueid: not implemented"


foreign export ccall
  lua_gc_hs :: EntryPoint (CInt -> CInt -> Ptr CInt -> IO CInt)

lua_gc_hs :: EntryPoint (CInt -> CInt -> Ptr CInt -> IO CInt)
lua_gc_hs l r what dat out =
  reentryIO "lua_gc" [cArg what, cArg dat] l r $ \_ ->
    result out 0


foreign export ccall
  lua_setallocf_hs :: EntryPoint (Lua_Alloc -> Ptr () -> IO CInt)

lua_setallocf_hs :: EntryPoint (Lua_Alloc -> Ptr () -> IO CInt)
lua_setallocf_hs l r f ud =
  reentry "lua_setallocf" [cArg f, cArg ud] l r $ \_ -> return Nothing
  -- XXX: Not implemented, ignored


foreign export ccall
  lua_topointer_hs :: EntryPoint (CInt -> Ptr (Ptr ()) -> IO CInt)

lua_topointer_hs :: EntryPoint (CInt -> Ptr (Ptr ()) -> IO CInt)
lua_topointer_hs l r ix out =
  reentry "lua_topointer" [cArg ix] l r $ \args ->
  do mb <- valueArgument (fromIntegral ix) args
     result out $
       case mb of
         Thread   ref -> plusPtr nullPtr $ referenceId ref
         Closure  ref -> plusPtr nullPtr $ referenceId ref
         Table    ref -> plusPtr nullPtr $ referenceId ref
         UserData ref -> plusPtr nullPtr $ referenceId ref
         LightUserData ptr -> ptr
         _ -> nullPtr

------------------------------------------------------------------------

type ApiLuaClose = EntryPoint (IO CInt)

foreign export ccall lua_close_hs :: ApiLuaClose

-- [-0, +0, -]
lua_close_hs :: ApiLuaClose
lua_close_hs l r = reentry "lua_close" [] l r $ \_args ->
  do cfg <- getsMachEnv machConfig
     liftIO (Nothing <$ machOnShutdown cfg)

------------------------------------------------------------------------

type GaluaControl = EntryPoint (CString -> IO CInt)

foreign export ccall galua_control_hs :: GaluaControl

galua_control_hs :: GaluaControl
galua_control_hs l r cmdPtr = reentry "galua_control" [cstringArg0 cmdPtr] l r $ \args ->
  do cmd <- liftIO (peekCString cmdPtr)
     onQuery <- getsMachEnv (machOnQuery . machConfig)
     answer <- liftIO (onQuery cmd)
     push args answer
     return Nothing
