{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Galua.CApi where

import Control.Concurrent (MVar, putMVar, takeMVar)
import Control.Exception
import Control.Monad (replicateM_,when,unless,(<=<))
import Control.Monad.IO.Class
import Data.Int
import Data.Bits
import Data.IORef
import Data.Foldable (toList, for_, traverse_)
import Data.Functor (void)
import Data.Traversable (for)
import Data.Maybe (fromMaybe)
import qualified Data.Text.Foreign as Text
import qualified Data.Vector as Vector
import           Data.Vector (Vector)
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.StablePtr
import Foreign.Storable
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

import Language.Lua.Bytecode
import Language.Lua.Bytecode.Debug
import Language.Lua.Bytecode.FunId

import Galua.Arguments
import Galua.CObjInfo(CObjInfo(..),getCFunInfo)
import Galua.Mach
import Galua.Number
import Galua.Overloading
import Galua.Reference
import Galua.Value
import Galua.LuaString
import qualified Galua.SizedVector as SV

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

type Reentry m =
  String            {- ^ entry name -} ->
  [PrimArgument]    {- ^ entry arguments -} ->
  Ptr ()            {- ^ stable pointer -} ->
  (SV.SizedVector (IORef Value) -> m ())    {- ^ operation continuation -} ->
  IO CInt

reentryIO :: Reentry IO
reentryIO label args l k = reentry label args l (liftIO . k)

reentryAlloc :: Reentry Alloc
reentryAlloc label args l k = reentry label args l (liftAlloc . k)

-- | Resume execution of the machine upon entry from the C API
reentry :: Reentry Mach
reentry label args l k =
  do ext <- deRefLuaState l
     putMVar (extLuaStateLuaServer ext) (CReEntry label args (wrapWithStack (extLuaStateThreadId ext) k))
     cServiceLoop l (extLuaStateCServer ext) (extLuaStateLuaServer ext)

wrapWithStack :: Int -> (SV.SizedVector (IORef Value) -> Mach a) -> Mach a
wrapWithStack threadId k =
  do Just threadRef <- lookupRef threadId
     thread <- readRef threadRef
     k $ execStack $ stExecEnv thread

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

cServiceLoop :: Ptr () -> MVar CNextStep -> MVar CCallState -> IO CInt
cServiceLoop l resultMVar interpMVar =
  do result <- takeMVar resultMVar
                `catch` \SomeException{} -> return CAbort
     case result of
       CAbort  -> return 1
       CResume -> return 0
       CCallback cfun ->
         do resultN <- capi_entry cfun l

            case resultN of
              -1 -> return () -- normal error unwind
              -2 -> fail "C functions called from Lua must return non-negative number"
              _ | resultN < 0 -> fail "Panic: capi_entry had invalid return value"
                | otherwise -> putMVar interpMVar (CReturned (fromIntegral resultN))

            cServiceLoop l resultMVar interpMVar

foreign import ccall "galua_capi_entry" capi_entry ::
  CFun -> Ptr () -> IO CInt

deRefLuaState :: Ptr () -> IO ExternalLuaState
deRefLuaState statePtr =
  do stabPtr <- peek (castPtr statePtr)
     let stab = castPtrToStablePtr stabPtr
     deRefStablePtr stab

class    CArg a          where cArg :: a -> PrimArgument
instance CArg CSize      where cArg = PrimIntArg . fromIntegral
instance CArg CInt       where cArg = PrimIntArg . fromIntegral
instance CArg Double     where cArg = PrimDoubleArg
instance CArg Int32      where cArg = PrimIntArg . fromIntegral
instance CArg Int64      where cArg = PrimIntArg . fromIntegral
instance CArg (FunPtr a) where cArg = PrimFunPtrArg . castFunPtr
instance CPtrArg a => CArg (Ptr a) where cArg = cPtrArg

class    CPtrArg a       where cPtrArg :: Ptr a -> PrimArgument
instance CPtrArg ()      where cPtrArg = PrimPtrArg
instance CPtrArg CInt    where cPtrArg = PrimCIntArg
instance CPtrArg CChar   where cPtrArg = PrimCStringArg
instance CPtrArg CSize   where cPtrArg = PrimCSizeArg

--------------------------------------------------------------------------------

foreign export ccall lua_error_hs :: Ptr () -> IO CInt

lua_error_hs :: Ptr () -> IO CInt
lua_error_hs l = reentry "lua_error" [] l $ \args ->
  do n <- liftIO (SV.size args)
     e <- if n == 0 then return Nil else pop args
     machThrow e

--------------------------------------------------------------------------------
-- Stack push functions
--------------------------------------------------------------------------------

foreign export ccall lua_pushlightuserdata_hs :: Ptr () -> Ptr () -> IO CInt

lua_pushlightuserdata_hs :: Ptr () -> Ptr () -> IO CInt
lua_pushlightuserdata_hs l u =
  reentryIO "lua_pushlightuserdata" [cArg u] l $ \stack ->
    push stack (LightUserData u)

foreign export ccall lua_pushnumber_hs :: Ptr () -> Lua_Number -> IO CInt

-- | [-0, +1, -]
lua_pushnumber_hs :: Ptr () -> Lua_Number -> IO CInt
lua_pushnumber_hs l n =
  reentryIO "lua_pushnumber" [cArg n] l $ \stack ->
    push stack (Number (Double (realToFrac n)))

foreign export ccall lua_pushnil_hs :: Ptr () -> IO CInt

-- | [-0, +1, -]
lua_pushnil_hs :: Ptr () -> IO CInt
lua_pushnil_hs l =
  reentryIO "lua_pushnil" [] l $ \args ->
    push args Nil

foreign export ccall lua_pushinteger_hs :: Ptr () -> Lua_Integer -> IO CInt

-- | [-0, +1, -]
lua_pushinteger_hs :: Ptr () -> Lua_Integer -> IO CInt
lua_pushinteger_hs l n =
  reentryIO "lua_pushinteger" [cArg n] l $ \stack ->
    push stack (Number (fromIntegral n))

foreign export ccall lua_pushboolean_hs :: Ptr () -> CInt -> IO CInt

-- | [-0, +1, -]
lua_pushboolean_hs :: Ptr () -> CInt -> IO CInt
lua_pushboolean_hs l b =
  reentryIO "lua_pushboolean" [cArg b] l $ \stack ->
    push stack (Bool (b /= 0))

foreign export ccall lua_pushstring_hs :: Ptr () -> CString -> Ptr CString -> IO CInt

-- | [-0, +1, e]
lua_pushstring_hs :: Ptr () -> CString -> Ptr CString -> IO CInt
lua_pushstring_hs l ptr out =
  reentryIO "lua_pushstring" [cArg ptr] l $ \args ->
  do str <- peekLuaString0 ptr
     poke out =<< luaStringPtr str
     push args (String str)

foreign export ccall lua_pushlstring_hs
  :: Ptr () -> CString -> CSize -> Ptr CString -> IO CInt

-- | [-0, +1, e]
lua_pushlstring_hs :: Ptr () -> CString -> CSize -> Ptr CString -> IO CInt
lua_pushlstring_hs l ptr sz out =
  reentryIO "lua_pushlstring" [cArg ptr, cArg sz] l $ \args ->
  do str <- peekLuaString (ptr, fromIntegral sz)
     poke out =<< luaStringPtr str
     -- Note that the output string must be null terminated!!
     push args (String str)

foreign export ccall lua_pushvalue_hs :: Ptr () -> CInt -> IO CInt

-- | [-0, +1, -]
lua_pushvalue_hs :: Ptr () -> CInt -> IO CInt
lua_pushvalue_hs l ix =
  reentry "lua_pushvalue" [cArg ix] l $ \args ->
  do x <- valueArgument (fromIntegral ix) args
     liftIO (push args x)

foreign export ccall lua_pushcclosure_hs
  :: Ptr () -> CFun -> CInt -> IO CInt

-- [-nup, +1, e]
lua_pushcclosure_hs :: Ptr () -> CFun -> CInt -> IO CInt
lua_pushcclosure_hs l func nup =
  reentry "lua_pushcclosure" [cArg func, cArg nup] l $ \args ->
  do upvals <- popN args $ fromIntegral nup
     info   <- liftIO $ getCFunInfo func
     vs     <- liftIO $ mapM newIORef $ Vector.fromList upvals
     c      <- machNewClosure (CFunction CFunName { cfunName = info
                                                  , cfunAddr = func}) vs
     push args (Closure c)

foreign export ccall lua_tocfunction_hs
  :: Ptr () -> CInt -> Ptr CFun -> IO CInt

-- [-0, +0, -]
lua_tocfunction_hs :: Ptr () -> CInt -> Ptr CFun -> IO CInt
lua_tocfunction_hs l ix out =
  reentry "lua_tocfunction" [cArg ix] l $ \args ->
  do x <- valueArgument (fromIntegral ix) args
     res <- case x of
              Closure cref ->
                do c <- readRef cref
                   case cloFun c of
                     CFunction cfun -> return (cfunAddr cfun)
                     _ -> return nullFunPtr
              _ -> return nullFunPtr
     liftIO (poke out res)

foreign export ccall lua_iscfunction_hs :: Ptr () -> CInt -> Ptr CInt -> IO CInt

-- [-0, +0, -]
lua_iscfunction_hs :: Ptr () -> CInt -> Ptr CInt -> IO CInt
lua_iscfunction_hs l ix out =
  reentry "lua_iscfunction" [cArg ix] l $ \args ->
  do x <- valueArgument (fromIntegral ix) args
     res <- case x of
              Closure cref ->
                do c <- readRef cref
                   case cloFun c of
                     CFunction {} -> return 1
                     _ -> return 0
              _ -> return 0
     liftIO (poke out res)

--------------------------------------------------------------------------------

foreign export ccall lua_type_hs :: Ptr () -> CInt -> Ptr CInt -> IO CInt

-- | [-0, +0, -]
lua_type_hs :: Ptr () -> CInt -> Ptr CInt -> IO CInt
lua_type_hs l ix out =
  reentry "lua_type" [cArg ix] l $ \args ->
  do mb <- valueArgumentOpt (fromIntegral ix) args
     liftIO $ poke out $
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

foreign export ccall lua_settop_hs :: Ptr () -> CInt -> IO CInt

-- | [-?, +?, -]
lua_settop_hs :: Ptr () -> CInt -> IO CInt
lua_settop_hs l ix =
  reentryIO "lua_settop" [cArg ix] l $ \stack ->
  do oldLen <- SV.size stack
     let i1 = fromIntegral ix
         newLen | i1 < 0    = oldLen + i1 + 1
                | otherwise = i1
     if oldLen <= newLen
       then replicateM_ (newLen - oldLen) (SV.push stack =<< newIORef Nil)
       else SV.shrink stack (oldLen - newLen)

foreign export ccall lua_gettop_hs :: Ptr () -> Ptr CInt -> IO CInt

-- | [-?, +?, -]
lua_gettop_hs :: Ptr () -> Ptr CInt -> IO CInt
lua_gettop_hs l out =
  reentryIO "lua_gettop" [] l $ \stack ->
    do n <- SV.size stack
       poke out (fromIntegral n)

foreign export ccall lua_rotate_hs :: Ptr () -> CInt -> CInt -> IO CInt

lua_rotate_hs :: Ptr () -> CInt -> CInt -> IO CInt
lua_rotate_hs l idx n =
  reentryIO "lua_rotate" [cArg idx, cArg n] l $ \args ->
    rotateHelper (fromIntegral idx) (fromIntegral n) args

rotateHelper :: Int -> Int -> SV.SizedVector a -> IO ()
rotateHelper idx n stack =
  do len <- SV.size stack

     let idx' | idx > 0   = idx - 1
              | otherwise = len + idx

         elts = len - idx'

     SV.rotateSubset stack idx' elts n

foreign export ccall lua_absindex_hs :: Ptr () -> CInt -> Ptr CInt -> IO CInt

-- | [-0, +0, -]
lua_absindex_hs :: Ptr () -> CInt -> Ptr CInt -> IO CInt
lua_absindex_hs l cidx out =
  reentryIO "lua_absindex" [cArg cidx] l $ \stack ->
  do n <- SV.size stack
     let idx = fromIntegral cidx
         idx' | isPseudo idx || idx > 0 = idx
              | otherwise               = n + idx + 1
     poke out (fromIntegral idx')

--------------------------------------------------------------------------------

foreign export ccall lua_settable_hs :: Ptr () -> CInt -> IO CInt

-- | [-2, +0, e]
lua_settable_hs :: Ptr () -> CInt -> IO CInt
lua_settable_hs l ix =
  reentry "lua_settable" [cArg ix] l $ \args ->
  do t <- valueArgument (fromIntegral ix) args
     kv <- liftIO (popN args 2)
     case kv of
       [k,v] -> setTable t k v
       _ -> luaError "lua_settable: bad arguments"

--------------------------------------------------------------------------------

foreign export ccall lua_next_hs :: Ptr () -> CInt -> Ptr CInt -> IO CInt

-- | [-0,+0,-]
lua_next_hs :: Ptr () -> CInt -> Ptr CInt -> IO CInt
lua_next_hs l idx out =
  reentry "lua_next" [cArg idx] l $ \args ->
  do t <- tableArgument (fromIntegral idx) args
     p <- pop args
     liftIO $
       do res <- tableNext t p
          case res of
            Nothing -> poke out 0
            Just (x,y) -> do poke out 1
                             push args x
                             push args y

--------------------------------------------------------------------------------

foreign export ccall lua_copy_hs :: Ptr () -> CInt -> CInt -> IO CInt

-- | [-0,+0,-]
lua_copy_hs :: Ptr () -> CInt -> CInt -> IO CInt
lua_copy_hs l fromidx toidx =
  reentry "lua_copy" [cArg fromidx, cArg toidx] l $ \args ->
  do x <- valueArgument (fromIntegral fromidx) args
     assign (fromIntegral toidx) x args

--------------------------------------------------------------------------------

foreign export ccall lua_isnumber_hs
  :: Ptr () -> CInt -> Ptr CInt -> IO CInt

-- | [-0, +0, -]
lua_isnumber_hs :: Ptr () -> CInt -> Ptr CInt -> IO CInt
lua_isnumber_hs l ix out =
  reentry "lua_isnumber" [cArg ix] l $ \args ->
  do v <- valueArgument(fromIntegral ix) args
     liftIO $ poke out $ case valueNumber v of
                           Just {} -> 1
                           Nothing -> 0

foreign export ccall lua_isuserdata_hs
  :: Ptr () -> CInt -> Ptr CInt -> IO CInt

-- | [-0, +0, -]
lua_isuserdata_hs :: Ptr () -> CInt -> Ptr CInt -> IO CInt
lua_isuserdata_hs l ix out =
  reentry "lua_isuserdata" [cArg ix] l $ \args ->
  do v <- valueArgument (fromIntegral ix) args
     liftIO $ poke out $ case v of
                            UserData{}      -> 1
                            LightUserData{} -> 1
                            _               -> 0

foreign export ccall lua_isstring_hs
  :: Ptr () -> CInt -> Ptr CInt -> IO CInt

-- | [-0, +0, -]
lua_isstring_hs :: Ptr () -> CInt -> Ptr CInt -> IO CInt
lua_isstring_hs l ix out =
  reentry "lua_isstring" [cArg ix] l $ \args ->
  do v <- valueArgument (fromIntegral ix) args
     liftIO $ poke out $ case v of
                           String{} -> 1
                           Number{} -> 1
                           _        -> 0

foreign export ccall lua_isinteger_hs
  :: Ptr () -> CInt -> Ptr CInt -> IO CInt

-- | [-0, +0, -]
lua_isinteger_hs :: Ptr () -> CInt -> Ptr CInt -> IO CInt
lua_isinteger_hs l ix out =
  reentry "lua_isinteger" [cArg ix] l $ \args ->
  do v <- valueArgument (fromIntegral ix) args
     liftIO $ poke out $ case v of
                           Number Int{} -> 1
                           _            -> 0

foreign export ccall lua_stringtonumber_hs
  :: Ptr () -> CString -> Ptr CSize -> IO CInt

lua_stringtonumber_hs :: Ptr () -> CString -> Ptr CSize -> IO CInt
lua_stringtonumber_hs l p out =
  reentryIO "lua_stringtonumber" [cArg p] l $ \args ->
    do str <- peekCAString p
       case parseNumber str of
         Nothing -> do poke out 0
         Just n  -> do poke out (fromIntegral (1 + length str))
                       push args (Number n)


foreign export ccall lua_tointegerx_hs
  :: Ptr () -> CInt -> Ptr CInt -> Ptr Lua_Integer -> IO CInt

-- | [-0, +0, -]
lua_tointegerx_hs :: Ptr () -> CInt -> Ptr CInt -> Ptr Lua_Integer -> IO CInt
lua_tointegerx_hs l ix isnum out =
  reentry "lua_integerx" [cArg ix, cArg isnum] l $ \args ->
  do v <- valueArgument (fromIntegral ix) args
     liftIO $ case valueInt v of
                Just i  -> pokeNotNull isnum 1 >> poke out (fromIntegral i)
                Nothing -> pokeNotNull isnum 0 >> poke out 0

foreign export ccall lua_toboolean_hs
  :: Ptr () -> CInt -> Ptr CInt -> IO CInt

-- | [-0, +0, -]
lua_toboolean_hs :: Ptr () -> CInt -> Ptr CInt -> IO CInt
lua_toboolean_hs l ix out =
  reentry "lua_toboolean" [cArg ix] l $ \args ->
  do b <- boolArgument (fromIntegral ix) args
     liftIO (poke out (if b then 1 else 0))

foreign export ccall lua_tonumberx_hs
  :: Ptr () -> CInt -> Ptr CInt -> Ptr Lua_Number -> IO CInt

-- | [-0, +0, -]
lua_tonumberx_hs :: Ptr () -> CInt -> Ptr CInt -> Ptr Lua_Number -> IO CInt
lua_tonumberx_hs l ix isnum out =
  reentry "lua_tonumberx" [cArg ix, cArg isnum] l $ \args ->
  do v <- valueArgument (fromIntegral ix) args
     liftIO $ case numberToDouble <$> valueNumber v of
                Just d -> do pokeNotNull isnum 1
                             poke out d
                Nothing -> pokeNotNull isnum 0

foreign export ccall lua_tolstring_hs
  :: Ptr () -> CInt -> Ptr CSize -> Ptr CString -> IO CInt

-- | [-0, +0, m]
lua_tolstring_hs :: Ptr () -> CInt -> Ptr CSize -> Ptr CString -> IO CInt
lua_tolstring_hs l arg len out =
  reentry "lua_tolstring" [cArg arg, cArg len] l $ \args ->
  do let idx = fromIntegral arg
     mb <- valueArgument idx args
     case mb of

       String str ->
         do liftIO $ do pokeNotNull len (fromIntegral (luaStringLen str))
                        poke out =<< luaStringPtr str

       Number n ->
         do str <- liftIO $
                do str <- fromByteString (B8.pack (numberToString n))
                   pokeNotNull len (fromIntegral (luaStringLen str))
                   poke out =<< luaStringPtr str
                   return str

            assign idx (String str) args

       _ -> liftIO $
         do pokeNotNull len 0
            poke out nullPtr

foreign export ccall lua_touserdata_hs
  :: Ptr () -> CInt -> Ptr (Ptr ()) -> IO CInt

-- | [-0, +0, -]
lua_touserdata_hs :: Ptr () -> CInt -> Ptr (Ptr ()) -> IO CInt
lua_touserdata_hs l arg out =
  reentry "lua_touserdata" [cArg arg] l $ \args ->
  do v <- valueArgument (fromIntegral arg) args
     ptr <- case v of
              UserData uref ->
                do u <- readRef uref
                   let fptr = userDataPtr u
                   return (unsafeForeignPtrToPtr fptr)
              LightUserData ptr -> return ptr
              _ -> return nullPtr
     liftIO (poke out ptr)

--------------------------------------------------------------------------------

foreign export ccall lua_getfield_hs
  :: Ptr () -> CInt -> CString -> Ptr CInt -> IO CInt

-- [-0, +1, e]
lua_getfield_hs :: Ptr () -> CInt -> CString -> Ptr CInt -> IO CInt
lua_getfield_hs l ix k out =
  reentry "lua_getfield" [cArg ix, cArg k] l $ \args ->
  do t <- valueArgument (fromIntegral ix) args
     key <- liftIO (peekLuaString0 k)
     v <- indexValue t (String key)
     liftIO $
       do poke out (lua_type_int v)
          push args v

foreign export ccall lua_setfield_hs
  :: Ptr () -> CInt -> CString -> IO CInt

-- | [-1, +0, e]
lua_setfield_hs :: Ptr () -> CInt -> CString -> IO CInt
lua_setfield_hs l ix k =
  reentry "lua_setfield" [cArg ix, cArg k] l $ \args ->
  do t <- valueArgument (fromIntegral ix) args
     v <- pop args
     key <- liftIO (peekLuaString0 k)
     setTable t (String key) v

foreign export ccall lua_createtable_hs :: Ptr () -> CInt -> CInt -> IO CInt

-- | [-0, +1, e]
lua_createtable_hs :: Ptr () -> CInt -> CInt -> IO CInt
lua_createtable_hs l na nh =
  reentry "lua_createtable" [cArg na, cArg nh] l $ \args ->
  do t <- machNewTable (fromIntegral na) (fromIntegral nh)
     push args (Table t)

foreign export ccall lua_rawgeti_hs
  :: Ptr () -> CInt -> Lua_Integer -> Ptr CInt -> IO CInt

lua_rawgeti_hs :: Ptr () -> CInt -> Lua_Integer -> Ptr CInt -> IO CInt
lua_rawgeti_hs l ix n out =
  reentry "lua_rawgeti" [cArg ix, cArg n] l $ \args ->
  do t <- tableArgument (fromIntegral ix) args
     liftIO $
       do v <- getTableRaw t (Number (fromIntegral n))
          poke out (lua_type_int v)
          push args v

foreign export ccall lua_rawgetp_hs
  :: Ptr () -> CInt -> Ptr () -> Ptr CInt -> IO CInt

lua_rawgetp_hs :: Ptr () -> CInt -> Ptr () -> Ptr CInt -> IO CInt
lua_rawgetp_hs l ix p out =
  reentry "lua_rawgetp" [cArg ix, cArg p] l $ \args ->
  do t <- tableArgument (fromIntegral ix) args
     liftIO $
       do v <- getTableRaw t (LightUserData p)
          poke out (lua_type_int v)
          push args v

foreign export ccall lua_rawget_hs
  :: Ptr () -> CInt -> Ptr CInt -> IO CInt

lua_rawget_hs :: Ptr () -> CInt -> Ptr CInt -> IO CInt
lua_rawget_hs l ix out =
  reentry "lua_rawget" [cArg ix] l $ \args ->
  do t <- tableArgument (fromIntegral ix) args
     k <- pop args
     liftIO $
       do v <- getTableRaw t k
          poke out (lua_type_int v)
          push args v

foreign export ccall lua_geti_hs
  :: Ptr () -> CInt -> Lua_Integer -> Ptr CInt -> IO CInt

lua_geti_hs :: Ptr () -> CInt -> Lua_Integer -> Ptr CInt -> IO CInt
lua_geti_hs l ix n out =
  reentry "lua_geti" [cArg ix, cArg n] l $ \args ->
  do t <- valueArgument (fromIntegral ix) args
     v <- indexValue t (Number (fromIntegral n))
     liftIO $
       do poke out (lua_type_int v)
          push args v

foreign export ccall lua_gettable_hs
  :: Ptr () -> CInt -> Ptr CInt -> IO CInt

lua_gettable_hs :: Ptr () -> CInt -> Ptr CInt -> IO CInt
lua_gettable_hs l ix out =
  reentry "lua_gettable" [cArg ix] l $ \args ->
  do t <- valueArgument (fromIntegral ix) args
     k <- pop args
     v <- indexValue t k
     liftIO $
       do poke out (lua_type_int v)
          push args v

foreign export ccall lua_rawset_hs
  :: Ptr () -> CInt -> IO CInt

lua_rawset_hs :: Ptr () -> CInt -> IO CInt
lua_rawset_hs l ix =
  reentry "lua_rawset" [cArg ix] l $ \args ->
  do t <- tableArgument (fromIntegral ix) args
     kv <- liftIO (popN args 2)
     case kv of
       [Number (Double nan),_] | isNaN nan -> luaError "Invalid table index NaN"
       [Nil                ,_]             -> luaError "Invalid table index nil"
       [k,v]                               -> setTableRaw t k v
       _                                   -> luaError "lua_rawset: missing arguments"

foreign export ccall lua_seti_hs
  :: Ptr () -> CInt -> Lua_Integer -> IO CInt

lua_seti_hs :: Ptr () -> CInt -> Lua_Integer -> IO CInt
lua_seti_hs l ix n =
  reentry "lua_seti" [cArg ix, cArg n] l $ \args ->
  do t <- valueArgument (fromIntegral ix) args
     v <- pop args
     setTable t (Number (fromIntegral n)) v

foreign export ccall lua_rawseti_hs
  :: Ptr () -> CInt -> Lua_Integer -> IO CInt

lua_rawseti_hs :: Ptr () -> CInt -> Lua_Integer -> IO CInt
lua_rawseti_hs l ix n =
  reentry "lua_rawseti" [cArg ix, cArg n] l $ \args ->
  do t <- tableArgument (fromIntegral ix) args
     v <- pop args
     setTableRaw t (Number (fromIntegral n)) v

foreign export ccall lua_rawsetp_hs
  :: Ptr () -> CInt -> Ptr () -> IO CInt

lua_rawsetp_hs :: Ptr () -> CInt -> Ptr () -> IO CInt
lua_rawsetp_hs l ix p =
  reentry "lua_rawsetp" [cArg ix, cArg p] l $ \args ->
  do t <- tableArgument (fromIntegral ix) args
     v <- pop args
     setTableRaw t (LightUserData p) v

------------------------------------------------------------------------

foreign export ccall lua_rawequal_hs
  :: Ptr () -> CInt -> CInt -> Ptr CInt -> IO CInt

lua_rawequal_hs :: Ptr () -> CInt -> CInt -> Ptr CInt -> IO CInt
lua_rawequal_hs l ix1 ix2 out =
  reentry "lua_rawequal" [cArg ix1, cArg ix2] l $ \args ->
  do x <- valueArgumentOpt (fromIntegral ix1) args
     y <- valueArgumentOpt (fromIntegral ix2) args
     liftIO $ poke out $
       case (x,y) of
          (Just a, Just b) | a == b -> 1
          _                         -> 0

foreign export ccall lua_compare_hs
  :: Ptr () -> CInt -> CInt -> CInt -> Ptr CInt -> IO CInt

lua_compare_hs :: Ptr () -> CInt -> CInt -> CInt -> Ptr CInt -> IO CInt
lua_compare_hs l ix1 ix2 op out =
  reentry "lua_compare" [cArg ix1, cArg ix2, cArg op] l $ \args ->
  -- Note: Also returns 0 if any of the indices is not valid.
  do mbx <- valueArgumentOpt (fromIntegral ix1) args
     mby <- valueArgumentOpt (fromIntegral ix2) args
     ans <- case (mbx,mby) of
       (Just x, Just y) ->
         do f <- case op of
                   #{const LUA_OPEQ} -> return valueEqual
                   #{const LUA_OPLT} -> return valueLess
                   #{const LUA_OPLE} -> return valueLessEqual
                   _                 -> luaError "lua_compare: bad operator"
            result <- f x y
            return (if result then 1 else 0)
       _ -> return 0
     liftIO (poke out ans)

foreign export ccall lua_arith_hs :: Ptr () -> CInt -> IO CInt

lua_arith_hs :: Ptr () -> CInt -> IO CInt
lua_arith_hs l opNum =
  reentry "lua_arith" [cArg opNum] l $
    case opNum of
      #{const LUA_OPADD}  -> arith2 "__add" (+)
      #{const LUA_OPSUB}  -> arith2 "__sub" (-)
      #{const LUA_OPMUL}  -> arith2 "__mul" (*)
      #{const LUA_OPDIV}  -> arith2 "__div" numberDiv
      #{const LUA_OPIDIV} -> idivCase
      #{const LUA_OPMOD}  -> arith2 "__mod" numberMod
      #{const LUA_OPPOW}  -> arith2 "__pow" numberPow
      #{const LUA_OPUNM}  -> arith1 "__unm" negate
      #{const LUA_OPBAND} -> int2 "__band" (.&.)
      #{const LUA_OPBOR}  -> int2 "__bor" (.|.)
      #{const LUA_OPBXOR} -> int2 "__bxor" xor
      #{const LUA_OPSHL}  -> int2 "__shl" wordshiftL
      #{const LUA_OPSHR}  -> int2 "__shr" wordshiftR
      #{const LUA_OPBNOT} -> int1 "__bnot" complement
      _                   -> \_ -> luaError "lua_arith: bad operator"
  where
  idivCase args =
    do y <- pop args
       x <- pop args
       case (x,y) of
         (Number (Int _), Number (Int 0)) -> luaError "atempt to divide by zero"
         _ -> do z <- valueArith2 "__idiv" numberIDiv x y
                 push args z

  arith2 name op args =
    do y <- pop args
       x <- pop args
       z <- valueArith2 name op x y
       push args z

  arith1 name op args =
    do x <- pop args
       y <- valueArith1 name op x
       push args y

  int2 name op args =
    do y <- pop args
       x <- pop args
       z <- valueInt2 name op x y
       push args z

  int1 name op args =
    do x <- pop args
       y <- valueInt1 name op x
       push args y


foreign export ccall lua_getmetatable_hs
  :: Ptr () -> CInt -> Ptr CInt -> IO CInt

lua_getmetatable_hs :: Ptr () -> CInt -> Ptr CInt -> IO CInt
lua_getmetatable_hs l ix out =
  reentry "lua_getmetatable" [cArg ix] l $ \args ->
  do v <- valueArgument (fromIntegral ix) args
     mt <- valueMetatable v
     case mt of
       Nothing -> do liftIO (poke out 0)
       Just m  -> do liftIO (poke out 1)
                     push args (Table m)

------------------------------------------------------------------------

foreign export ccall lua_newuserdata_hs
  :: Ptr () -> CSize -> Ptr (Ptr ()) -> IO CInt

-- | [-0, +1, e]
lua_newuserdata_hs :: Ptr () -> CSize -> Ptr (Ptr ()) -> IO CInt
lua_newuserdata_hs l sz out =
  reentry "lua_newuserdata" [cArg sz] l $ \args ->
  do let sz' = fromIntegral sz
     fptr <- liftIO (mallocForeignPtrBytes sz')
     u <- machNewUserData fptr sz'

     garbage <- getsMachEnv machGarbage
     liftIO (addRefFinalizer u (atomicModifyIORef garbage (\xs -> (UserData u : xs, ()))))

     liftIO (poke out (unsafeForeignPtrToPtr fptr))
     push args (UserData u)

foreign export ccall lua_getuservalue_hs
  :: Ptr () -> CInt -> Ptr CInt -> IO CInt

lua_getuservalue_hs :: Ptr () -> CInt -> Ptr CInt -> IO CInt
lua_getuservalue_hs l ix out =
  reentry "lua_getuservalue" [cArg ix] l $ \args ->
  do v <- valueArgument (fromIntegral ix) args
     case v of
       UserData uref ->
          do u <- readRef uref
             uservalue <- liftIO (readIORef (userDataValue u))
             liftIO (poke out (lua_type_int uservalue))
             push args uservalue
       _ -> luaError "lua_getuservalue: argument is not full userdata"

foreign export ccall lua_setuservalue_hs
  :: Ptr () -> CInt -> IO CInt

lua_setuservalue_hs :: Ptr () -> CInt -> IO CInt
lua_setuservalue_hs l ix =
  reentry "lua_setuservalue" [cArg ix] l $ \args ->
  do v         <- valueArgument (fromIntegral ix) args
     uservalue <- pop args
     case v of
       UserData uref ->
         do u <- readRef uref
            liftIO (writeIORef (userDataValue u) uservalue)
       _ -> luaError "lua_setuservalue: argument is not full userdata"

------------------------------------------------------------------------

lua_multret :: CInt
lua_multret = (#const LUA_MULTRET)

foreign export ccall lua_callk_hs
  :: Ptr () -> CInt -> CInt -> Lua_KContext -> Lua_KFunction -> IO CInt

-- | [-0, +1, e]
lua_callk_hs :: Ptr () -> CInt -> CInt -> Lua_KContext -> Lua_KFunction -> IO CInt
lua_callk_hs l narg nresult ctx k =
  reentry "lua_callk" [cArg narg, cArg nresult, cArg ctx, cArg k] l $ \args ->
  do fxs <- popN args (fromIntegral narg + 1)
     case fxs of
       f:xs ->
         do rs <- callValue f xs
            traverse_ (push args)
                $ if nresult == lua_multret
                    then rs
                    else take (fromIntegral nresult) (rs ++ repeat Nil)
       _ -> luaError "lua_callk: invalid narg"

foreign export ccall lua_pcallk_hs
  :: Ptr () -> CInt -> CInt -> CInt -> Ptr CInt -> IO CInt

-- | [-0, +1, e]
lua_pcallk_hs ::
  Ptr () -> CInt -> CInt -> CInt -> Ptr CInt -> IO CInt
lua_pcallk_hs l narg nresult msgh out =
  reentry "lua_callk" [cArg narg, cArg nresult, cArg msgh] l $ \args ->
  do h <- if msgh == 0
            then return DefaultHandler
            else FunHandler <$> functionArgument (fromIntegral msgh) args
     fxs <- popN args (fromIntegral narg + 1)
     case fxs of
       f:xs ->
         do (f',xs') <- resolveFunction f xs
            res <- machTry h f' xs'
            case res of
              Left e ->
                do liftIO (poke out luaERRRUN)
                   push args e
              Right rs ->
                do liftIO (poke out luaOK)
                   traverse_ (push args)
                     $ if nresult == lua_multret
                           then rs
                           else take (fromIntegral nresult) (rs ++ repeat Nil)
       _ -> luaError "lua_pcallk: invalid narg"

------------------------------------------------------------------------

foreign export ccall lua_setmetatable_hs :: Ptr () -> CInt -> IO CInt

-- | [-1, +0, e]
lua_setmetatable_hs :: Ptr () -> CInt -> IO CInt
lua_setmetatable_hs l idx =
  reentry "lua_setmetatable" [cArg idx] l $ \args ->
  do v <- valueArgument (fromIntegral idx) args
     m <- pop args
     case m of -- must be non-empty becuase previous line worked
       Nil     -> setMetatable Nothing v
       Table t -> setMetatable (Just t) v
       _       -> luaError "lua_setmetatable expected table argument"

------------------------------------------------------------------------

foreign export ccall lua_len_hs :: Ptr () -> CInt -> IO CInt

-- | [-0, +1, e]
lua_len_hs :: Ptr () -> CInt -> IO CInt
lua_len_hs l idx =
  reentry "lua_len" [cArg idx] l $ \args ->
  do v <- valueArgument (fromIntegral idx) args
     len <- valueLength v
     liftIO (push args len)

foreign export ccall lua_rawlen_hs :: Ptr () -> CInt -> Ptr CSize -> IO CInt

-- | [-0, +0, –]
lua_rawlen_hs :: Ptr () -> CInt -> Ptr CSize -> IO CInt
lua_rawlen_hs l idx out =
  reentry "lua_rawlen" [cArg idx] l $ \args ->
  do v <- valueArgument (fromIntegral idx) args
     liftIO $
       do len <- case v of
                   String xs  -> return (luaStringLen xs)
                   Table t    -> tableLen t
                   UserData u -> userDataSize <$> readRef u
                   _          -> return 0
          liftIO (poke out (fromIntegral len))

------------------------------------------------------------------------

-- int lua_load (lua_State *L, lua_Reader reader, void *data, const char *chunkname, const char *mode) {
foreign export ccall lua_load_hs ::
  Ptr () -> CString -> CSize -> CString -> CString -> Ptr CInt -> IO CInt

lua_load_hs :: Ptr () -> CString -> CSize -> CString -> CString -> Ptr CInt -> IO CInt
lua_load_hs l chunk chunksize chunkname mode out =
  reentry "lua_load" [cArg chunk, cArg chunksize, cArg chunkname, cArg mode] l
     $ \args ->
    do chunk1<- liftIO (B.packCStringLen (chunk, fromIntegral chunksize))
       name  <- if nullPtr == chunkname
                  then return Nothing
                  else liftIO (Just <$> peekCAString chunkname)
       menv  <- machCurrentEnv
       globals <- getsMachEnv machGlobals
       eclo <- chunkToClosure menv name chunk1 (Table globals)
       liftIO $ case eclo of
         Left e    -> do poke out 1
                         e' <- fromByteString (B8.pack e)
                         push args (String e')
         Right clo -> do poke out 0
                         push args (Closure clo)


------------------------------------------------------------------------

foreign export ccall lua_setglobal_hs :: Ptr () -> CString -> IO CInt

lua_setglobal_hs :: Ptr () -> CString -> IO CInt
lua_setglobal_hs l cname =
  reentry "lua_setglobal" [cArg cname] l $ \args ->
  do v <- pop args
     t <- getsMachEnv machGlobals
     liftIO $
       do k <- peekLuaString0 cname
          setTableRaw t (String k) v

foreign export ccall lua_getglobal_hs :: Ptr () -> CString -> Ptr CInt -> IO CInt

lua_getglobal_hs :: Ptr () -> CString -> Ptr CInt -> IO CInt
lua_getglobal_hs l cname out =
  reentry "lua_getglobal" [cArg cname] l $ \args ->
  do t <- getsMachEnv machGlobals
     liftIO $
       do k <- peekLuaString0 cname
          v <- getTableRaw t (String k)
          poke out (lua_type_int v)
          push args v

------------------------------------------------------------------------

foreign export ccall lua_concat_hs :: Ptr () -> CInt -> IO CInt

lua_concat_hs :: Ptr () -> CInt -> IO CInt
lua_concat_hs l n =
  reentry "lua_concat" [cArg n] l $ \args ->
  do xs <- popN args (fromIntegral n)
     res <- opConcat xs
     push args res

foreign export ccall lua_dump_hs ::
  Ptr () -> FunPtr () -> Ptr () -> CInt -> Ptr CInt -> IO CInt

lua_dump_hs :: Ptr () -> FunPtr () -> Ptr () -> CInt -> Ptr CInt -> IO CInt
lua_dump_hs l writer dat strip _out =
  reentry "lua_dump" [cArg writer, cArg dat, cArg strip] l $ \_args ->
    luaError "lua_dump not implemented"

------------------------------------------------------------------------

pokeNotNull :: Storable a => Ptr a -> a -> IO ()
pokeNotNull ptr a = unless (ptr == nullPtr) (poke ptr a)

--------------------------------------------------------------------------------
-- Coroutines


foreign export ccall
  lua_tothread_hs :: Ptr () -> CInt -> Ptr (Ptr ()) -> IO CInt

lua_tothread_hs :: Ptr () -> CInt -> Ptr (Ptr ()) -> IO CInt
lua_tothread_hs st n res =
  reentry "lua_tothread" [cArg n] st $ \args ->
    do v <- valueArgument (fromIntegral n) args
       p <- case v of
          Thread t ->
            do thr <- readRef t
               return (unsafeForeignPtrToPtr (threadCPtr thr))
          _ -> return nullPtr
       liftIO (poke res p)

foreign export ccall
  lua_pushthread_hs :: Ptr () -> Ptr CInt -> IO CInt

lua_pushthread_hs :: Ptr () -> Ptr CInt -> IO CInt
lua_pushthread_hs st res =
  reentry "lua_pushthread" [] st $ \args ->
    do ref    <- machCurrentThread
       isMain <- machIsMainThread ref
       liftIO (poke res (if isMain then 1 else 0))
       push args (Thread ref)


foreign export ccall
  lua_status_hs :: Ptr () -> Ptr CInt -> IO CInt

lua_status_hs :: Ptr () -> Ptr CInt -> IO CInt
lua_status_hs l out =
  reentry "lua_status" [] l $ \_ ->
     do thread <- readRef =<< extToThreadRef =<< liftIO (deRefLuaState l)
        let statusNum =
              case threadStatus thread of
                ThreadSuspended{} -> luaYIELD
                ThreadNormal   {} -> luaOK
                ThreadRunning  {} -> luaOK
                ThreadNew      {} -> luaOK
                ThreadCrashed  {} -> luaERRRUN
        liftIO (poke out statusNum)

foreign export ccall
  lua_resume_hs :: Ptr () -> Ptr () -> CInt -> Ptr CInt -> IO CInt

lua_resume_hs :: Ptr () -> Ptr () -> CInt -> Ptr CInt -> IO CInt
lua_resume_hs l from nargs out =
  reentry "lua_resume" [cArg from, cArg nargs] l $ \args ->
    do tRef <- extToThreadRef =<< liftIO (deRefLuaState l)

       st <- getThreadStatus tRef
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
    do liftIO (poke out luaERRRUN)
       push args e

  doResume tRef leftover =
    do result <- machResume tRef
       case result of
         ThreadReturn rs ->
           do liftIO (poke out luaOK)

              thread <- readRef tRef
              let eenv = stExecEnv thread
              liftIO (stackFromList (execStack eenv) rs)

         ThreadYield ->
           do liftIO (poke out luaYIELD)

         ThreadError e -> finishWithError leftover e


foreign export ccall
  lua_yieldk_hs :: Ptr () -> CInt -> Lua_KContext -> Lua_KFunction -> IO CInt

lua_yieldk_hs :: Ptr () -> CInt -> Lua_KContext -> Lua_KFunction -> IO CInt
lua_yieldk_hs l nResults ctx func =
  reentry "lua_yieldk" [cArg nResults, cArg ctx, cArg func] l $ \args ->
    do outputs <- popN args (fromIntegral nResults)

       tRef <- extToThreadRef =<< liftIO (deRefLuaState l)
       thread <- readRef tRef
       let stack = execStack (stExecEnv thread)
       liftIO $ do n <- SV.size stack
                   SV.shrink stack n
                   traverse_ (push stack) outputs

       machYield

       inputs <- liftIO (stackToList stack)
       if func == nullFunPtr
         then machReturn inputs
         else fail "Panic: lua_yieldk with continuation not implemented" -- XXX


foreign export ccall
  lua_newthread_hs :: Ptr () -> Ptr (Ptr ()) -> IO CInt

lua_newthread_hs :: Ptr () -> Ptr (Ptr ()) -> IO CInt
lua_newthread_hs l out =
  reentry "lua_newthread" [] l $ \args ->
    do threadRef <- machNewThread
       thread    <- readRef threadRef
       liftIO (poke out (unsafeForeignPtrToPtr (threadCPtr thread)))
       push args (Thread threadRef)

foreign export ccall
  lua_isyieldable_hs :: Ptr () -> Ptr CInt -> IO CInt

lua_isyieldable_hs :: Ptr () -> Ptr CInt -> IO CInt
lua_isyieldable_hs l out =
  reentry "lua_isyieldable" [] l $ \_ ->
    do tRef <- extToThreadRef =<< liftIO (deRefLuaState l)

       isMain <- machIsMainThread tRef
       liftIO $
         do isYieldable <-
               if isMain
                 then return False
                 else isThreadRunning . threadStatus <$> readRef tRef
            poke out (if isYieldable then 1 else 0)


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

pokeLuaDebugIsTailCall :: Ptr LuaDebug -> CInt -> IO ()
pokeLuaDebugIsTailCall = #poke struct lua_Debug, istailcall

pokeLuaDebugName :: Ptr LuaDebug -> CString -> IO ()
pokeLuaDebugName = #poke struct lua_Debug, name

pokeLuaDebugNameWhat :: Ptr LuaDebug -> CString -> IO ()
pokeLuaDebugNameWhat = #poke struct lua_Debug, namewhat

pokeLuaDebugWhat :: Ptr LuaDebug -> CString -> IO ()
pokeLuaDebugWhat = #poke struct lua_Debug, namewhat

pokeLuaDebugNUps :: Ptr LuaDebug -> CInt -> IO ()
pokeLuaDebugNUps = #poke struct lua_Debug, nups

pokeLuaDebugNParams :: Ptr LuaDebug -> CInt -> IO ()
pokeLuaDebugNParams = #poke struct lua_Debug, nparams

pokeLuaDebugIsVarArg :: Ptr LuaDebug -> CInt -> IO ()
pokeLuaDebugIsVarArg = #poke struct lua_Debug, isvararg

findExecEnv :: Int -> Thread -> Maybe (Int,ExecEnv)
findExecEnv level thread =
  case compare level 0 of
    LT -> Nothing
    EQ -> Just (stPC thread, stExecEnv thread)
    GT -> go (level-1) (toList (stStack thread))

  where
  go 0 (CallFrame pc execEnv _ _ : _) = Just (pc, execEnv)
  go l (CallFrame {} : xs)            = go (l-1) xs
  go _ []                             = Nothing
  go l (ErrorFrame  {} : xs)          = go l xs


exportExecEnv :: (Int,ExecEnv) -> IO (Ptr ())
exportExecEnv e =
  do sptr <- newStablePtr e -- XXX: memory leak
     return (castStablePtrToPtr sptr)

importExecEnv :: Ptr () -> IO (Int,ExecEnv)
importExecEnv = deRefStablePtr . castPtrToStablePtr

------------------------------------------------------------------------

foreign export ccall
  lua_getstack_hs :: Ptr () -> CInt -> Ptr LuaDebug -> Ptr CInt -> IO CInt

lua_getstack_hs :: Ptr () -> CInt -> Ptr LuaDebug -> Ptr CInt -> IO CInt
lua_getstack_hs l level ar out =
  reentry "lua_getstack" [cArg level, cArg (castPtr ar :: Ptr ())] l $ \_ ->

  do tRef <- extToThreadRef =<< liftIO (deRefLuaState l)
     thread <- readRef tRef
     let mbExecEnv = findExecEnv (fromIntegral level) thread

     result <- case mbExecEnv of
       -- get stack doesn't access a thread's initial stack for whatever reason
       Just execEnv | not (isNullCFunction (execFunction (snd execEnv))) ->
         do liftIO (pokeLuaDebugCallInfo ar =<< exportExecEnv execEnv)
            return 1

       _ -> return 0

     liftIO (poke out result)

isNullCFunction :: FunctionValue -> Bool
isNullCFunction (CFunction name) = cfunAddr name == nullFunPtr
isNullCFunction _                = False

------------------------------------------------------------------------

foreign export ccall
  lua_getinfo_hs :: Ptr () -> CString -> Ptr LuaDebug -> Ptr CInt -> IO CInt

lua_getinfo_hs :: Ptr () -> CString -> Ptr LuaDebug -> Ptr CInt -> IO CInt
lua_getinfo_hs l whatPtr ar out =
  reentry "lua_getinfo" [cArg whatPtr, cArg (castPtr ar :: Ptr ())] l $ \args ->
  do (pc,execEnv) <- liftIO (importExecEnv =<< peekLuaDebugCallInfo ar)
     what          <- liftIO (peekCString whatPtr)

     let luaWhat fid = if isRootFun fid then "main" else "Lua"

     liftIO $
       case execFunction execEnv of

         LuaFunction fid fun -> liftIO $
           do when ('n' `elem` what) $
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
                do pokeLuaDebugNUps            ar (fromIntegral (Vector.length (execUpvals execEnv)))
                   pokeLuaDebugNParams         ar (fromIntegral (funcNumParams fun))
                   pokeLuaDebugIsVarArg        ar (if funcIsVararg fun then 1 else 0)
              poke out 1


         CFunction cfun ->
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
                do pokeLuaDebugNUps            ar (fromIntegral (Vector.length (execUpvals execEnv)))
                   pokeLuaDebugNParams         ar 0 -- always 0 for C functions
                   pokeLuaDebugIsVarArg        ar 1 -- always true for C functions

              poke out 1

     when ('f'`elem`what) (push args (execClosure execEnv))
     when ('L'`elem`what) (push args Nil)

------------------------------------------------------------------------

foreign export ccall
  lua_getlocal_hs :: Ptr () -> Ptr LuaDebug -> CInt -> Ptr CString -> IO CInt

lua_getlocal_hs :: Ptr () -> Ptr LuaDebug -> CInt -> Ptr CString -> IO CInt
lua_getlocal_hs l ar n out =
  reentry "lua_getlocal" [cArg (castPtr ar :: Ptr ()), cArg n] l $ \args ->
    if ar == nullPtr
      then getLocalFunArgs (fromIntegral n) out args
      else getLocalStackArgs (fromIntegral n) out args ar

getLocalFunArgs :: Int -> Ptr CString -> SV.SizedVector (IORef Value) -> Mach ()
getLocalFunArgs n out args =
  do clo <- readRef =<< functionArgument (-1) args
     void (pop args)
     liftIO $ case cloFun clo of
       LuaFunction _ fun ->
         case lookupLocalName fun 0 (Reg (n-1)) of
           Nothing -> poke out nullPtr
           Just bs -> poke out =<< newCAString (B8.unpack bs)--XXX: Leak
       CFunction{} -> poke out nullPtr

getLocalStackArgs :: Int -> Ptr CString -> SV.SizedVector (IORef Value) -> Ptr LuaDebug -> Mach ()
getLocalStackArgs n out args ar =
  do (pc,execEnv) <- liftIO (importExecEnv =<< peekLuaDebugCallInfo ar)

     let stack = execStack execEnv

     let ix = fromIntegral n - 1

     liftIO $ case execFunction execEnv of
       LuaFunction _ func
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
  lua_getupvalue_hs :: Ptr () -> CInt -> CInt -> Ptr CString -> IO CInt

lua_getupvalue_hs :: Ptr () -> CInt -> CInt -> Ptr CString -> IO CInt
lua_getupvalue_hs l funcindex n out =
  reentry "lua_getupvalue" [cArg funcindex, cArg n] l $ \args ->
    do v <- valueArgument (fromIntegral funcindex) args

       let failure = poke out nullPtr
           n' = fromIntegral n - 1

       liftIO $
         case v of
           Closure ref ->
              do clo <- readRef ref
                 case cloUpvalues clo Vector.!? n' of
                   Nothing -> failure
                   Just uv -> do push args =<< readIORef uv
                                 emptyStr <- newCAString (upvalueName (cloFun clo) n') -- XXX: Leak
                                 poke out emptyStr
           _ -> failure

upvalueName :: FunctionValue -> Int -> String
upvalueName CFunction{} _ = ""
upvalueName (LuaFunction _ fun) n =
  case debugInfoUpvalues (funcDebug fun) Vector.!? n of
    Just bs -> B8.unpack bs
    Nothing -> ""

------------------------------------------------------------------------

foreign export ccall
  lua_setlocal_hs :: Ptr () -> Ptr LuaDebug -> CInt -> Ptr CString -> IO CInt

lua_setlocal_hs :: Ptr () -> Ptr LuaDebug -> CInt -> Ptr CString -> IO CInt
lua_setlocal_hs l ar n out =
  reentry "lua_setlocal" [cArg (castPtr ar :: Ptr ()), cArg n] l $ \args ->

  do (pc,execEnv) <- liftIO (importExecEnv =<< peekLuaDebugCallInfo ar)

     let stack = execStack execEnv

     let ix = fromIntegral n - 1

     case execFunction execEnv of
       LuaFunction _ func
         | Just name <- lookupLocalName func pc (Reg ix) ->
           do mb <- liftIO (SV.getMaybe stack ix)
              case mb of
                Nothing -> liftIO (poke out nullPtr)
                Just cell ->
                     do liftIO $ poke out =<< newCAString (B8.unpack name)
                        v <- pop args
                        liftIO (writeIORef cell v)

       _ -> liftIO (poke out nullPtr)

------------------------------------------------------------------------

foreign export ccall
  lua_setupvalue_hs :: Ptr () -> CInt -> CInt -> Ptr CString -> IO CInt

lua_setupvalue_hs :: Ptr () -> CInt -> CInt -> Ptr CString -> IO CInt
lua_setupvalue_hs l funcindex n out =
  reentry "lua_setupvalue" [cArg funcindex, cArg n] l $ \args ->

    do v <- valueArgument (fromIntegral funcindex) args

       let failure = liftIO (poke out nullPtr)
           n' = fromIntegral n - 1

       case v of
         Closure ref ->
            do clo <- readRef ref
               case cloUpvalues clo Vector.!? n' of
                 Nothing -> failure
                 Just uv -> do x <- pop args
                               liftIO $
                                 do writeIORef uv x
                                    emptyStr <- newCAString (upvalueName (cloFun clo) n') -- XXX: Leak
                                    poke out emptyStr
         _ -> failure

------------------------------------------------------------------------

foreign export ccall
  lua_xmove_hs :: Ptr () -> Ptr () -> CInt -> IO CInt

lua_xmove_hs :: Ptr () -> Ptr () -> CInt -> IO CInt
lua_xmove_hs l to n =
  reentry "lua_xmove" [cArg to, cArg n] l $ \fromArgs ->

    do fromRef <- extToThreadRef =<< liftIO (deRefLuaState l)
       toRef   <- extToThreadRef =<< liftIO (deRefLuaState to)

       liftIO $
        unless (fromRef == toRef) $
          do transfer <- popN fromArgs (fromIntegral n)

             toThread <- readRef toRef
             let toStack = execStack (stExecEnv toThread)
             traverse_ (push toStack) transfer

------------------------------------------------------------------------

foreign export ccall
  lua_upvaluejoin_hs :: Ptr () -> CInt -> CInt -> CInt -> CInt -> IO CInt

-- Make the n1-th upvalue of the Lua closure at index funcindex1 refer to
-- the n2-th upvalue of the Lua closure at index funcindex2.
-- [-0, +0, –]
lua_upvaluejoin_hs :: Ptr () -> CInt -> CInt -> CInt -> CInt -> IO CInt
lua_upvaluejoin_hs l f1 n1 f2 n2 =
  reentry "lua_upvaluejoin" (cArg <$> [f1,n1,f2,n2]) l $ \args ->
    do f1' <- valueArgument (fromIntegral f1) args
       f2' <- valueArgument (fromIntegral f2) args
       let n1' = fromIntegral n1 - 1
           n2' = fromIntegral n2 - 1
       case (f1',f2') of
         (Closure ref1, Closure ref2) ->
           do clo2 <- readRef ref2
              for_ (cloUpvalues clo2 Vector.!? n2') $ \upRef ->
                do clo1 <- readRef ref1
                   for_ (replaceAt n1' upRef (cloUpvalues clo1)) $ \newUps ->
                     do let clo1' = clo1 { cloUpvalues = newUps }
                        writeRef ref1 clo1'
         _ -> return ()

replaceAt :: Int -> a -> Vector a -> Maybe (Vector a)
replaceAt i x v
  | 0 <= i && i < Vector.length v = Just $! v Vector.// [(i,x)]
  | otherwise = Nothing

------------------------------------------------------------------------

foreign export ccall
  lua_upvalueid_hs :: Ptr () -> CInt -> CInt -> Ptr (Ptr ()) -> IO CInt

lua_upvalueid_hs :: Ptr () -> CInt -> CInt -> Ptr (Ptr ()) -> IO CInt
lua_upvalueid_hs l funcindex n _out =
  reentry "lua_upvalueid" (cArg <$> [funcindex,n]) l $ \_ ->
    luaError "lua_upvalueid: not implemented"


foreign export ccall
  lua_gc_hs :: Ptr () -> CInt -> CInt -> Ptr CInt -> IO CInt

lua_gc_hs :: Ptr () -> CInt -> CInt -> Ptr CInt -> IO CInt
lua_gc_hs l what dat out =
  reentryIO "lua_gc" [cArg what, cArg dat] l $ \_ ->
    poke out 0


foreign export ccall
  lua_setallocf_hs :: Ptr () -> Lua_Alloc -> Ptr () -> IO CInt

lua_setallocf_hs :: Ptr () -> Lua_Alloc -> Ptr () -> IO CInt
lua_setallocf_hs l f ud =
  reentry "lua_setallocf" [cArg f, cArg ud] l $ \_ -> return()
  -- XXX: Not implemented, ignored


foreign export ccall
  lua_topointer_hs :: Ptr () -> CInt -> Ptr (Ptr ()) -> IO CInt

lua_topointer_hs :: Ptr () -> CInt -> Ptr (Ptr ()) -> IO CInt
lua_topointer_hs l ix out =
  reentry "lua_topointer" [cArg ix] l $ \args ->
  do mb <- valueArgument (fromIntegral ix) args
     liftIO $ poke out $
             case mb of
               Thread   ref -> plusPtr nullPtr $ referenceId ref
               Closure  ref -> plusPtr nullPtr $ referenceId ref
               Table    ref -> plusPtr nullPtr $ referenceId ref
               UserData ref -> plusPtr nullPtr $ referenceId ref
               LightUserData ptr -> ptr
               _ -> nullPtr
