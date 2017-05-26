{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Galua.CApi where

import Control.Concurrent (MVar, takeMVar, putMVar)
import Control.Concurrent.STM (TMVar, atomically, putTMVar)
import Control.Exception(catch,SomeException(..),throwIO,try,displayException)
import Control.Monad (replicateM_,when,unless)
import Data.Int
import Data.IORef
import Data.Foldable (toList, for_)
import Data.Functor (void)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as Vector
import           Data.Vector (Vector)
import qualified Data.Vector.Mutable as IOVector
import qualified Data.Text as Text
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

import Language.Lua.Bytecode.FunId


import qualified Galua.Util.SizedVector as SV

import Galua.Code
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
import           Galua.Util.SmallVec (SmallVec)
import qualified Galua.Util.SmallVec as SMV

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
  Int    {- ^ thread ID      -} ->
  Ptr () {- ^ return address -} ->
  a

type SF = SV.SizedVector (IORef Value)

-- | Resume execution of the machine upon entry from the C API
-- ^ Do IO, has access to both VM and continuation
reentryG ::
  String            {- ^ entry name -} ->
  [PrimArgument]    {- ^ entry arguments -} ->
  Ptr ()            {- ^ stable pointer -} ->
  Int               {- ^ thread ID      -} ->
  Ptr ()            {- ^ return address -} ->
  (VM -> SV.SizedVector Value -> IO NextStep)
                    {- ^ operation continuation producing optional C return -} ->
  IO CInt
reentryG = reentryGK 0 nullFunPtr

reentryGK ::
  Lua_KContext  ->
  Lua_KFunction ->
  String            {- ^ entry name -} ->
  [PrimArgument]    {- ^ entry arguments -} ->
  Ptr ()            {- ^ stable pointer -} ->
  Int               {- ^ thread ID      -} ->
  Ptr ()            {- ^ return address -} ->
  (VM -> SV.SizedVector Value -> IO NextStep)
                    {- ^ operation continuation producing optional C return -} ->
  IO CInt
reentryGK kctx kfun label args l tid r impl =
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

         let mbContinuation
                | nullFunPtr == kfun = Nothing
                | otherwise          = Just (capi_entryk kctx kfun)

             apiCall = ApiCall { apiCallMethod = label
                               , apiCallReturn = returnObjInfo
                               , apiCallArgs = args
                               , apiContinuation = mbContinuation
                               }

         atomically $ putTMVar (extLuaStateLuaServer ext)
                    $ CReEntry apiCall
                    $ \vm -> do Just threadRef <- machLookupRef vm tid
                                eenv           <- getThreadField stExecEnv threadRef
                                let stack = execCStack eenv
                                impl vm stack `catch` \(LuaX str) ->
                                    do s <- fromByteString (packUtf8 str)
                                       return $! ThrowError (String s)

         cServiceLoop (extLuaStateCServer ext) (extLuaStateLuaServer ext)




endReentry :: Maybe PrimArgument -> IO NextStep
endReentry res = return $! ApiEnd res


result :: CArg a => Ptr a -> a -> IO NextStep
result ptr res =
  do poke ptr res
     endReentry (Just (cArg res))

noResult :: IO NextStep
noResult = endReentry Nothing

push :: SV.SizedVector a -> a -> IO ()
push = SV.push

pop :: SV.SizedVector Value -> IO Value
pop stack = SV.pop stack `catch`
              \SomeException{} -> throwIO (LuaX "missing args")

popN :: SV.SizedVector Value -> Int -> IO (SmallVec Value)
popN = SV.popN

cServiceLoop :: MVar CNextStep -> TMVar CCallState -> IO CInt
cServiceLoop resultMVar interpMVar =
  do res <- takeMVar resultMVar
                `catch` \SomeException{} -> return CAbort
     case res of
       CAbort  -> return 1
       CResume -> return 0
       CCallback go ->
         do resultN <- go

            case resultN of
              -1 -> return () -- normal error unwind
              -2 -> fail "C functions called from Lua must return non-negative number"
              _ | resultN < 0 -> fail "Panic: capi_entry had invalid return value"
                | otherwise -> atomically (putTMVar interpMVar (CReturned (fromIntegral resultN)))

            cServiceLoop resultMVar interpMVar

foreign import ccall "galua_capi_entry" capi_entry ::
  CFun -> Ptr () -> IO CInt

foreign import ccall "galua_capi_entryk" capi_entryk ::
  Lua_KContext -> Lua_KFunction -> CInt -> Ptr () -> IO CInt

deRefLuaState :: Ptr () -> IO ExternalLuaState
deRefLuaState = deRefStablePtr . castPtrToStablePtr

class Storable a => CArg a where cArg :: a -> PrimArgument
instance CArg CSize      where cArg = PrimIntArg . fromIntegral
instance CArg CInt       where cArg = PrimIntArg . fromIntegral
instance CArg Double     where cArg = PrimDoubleArg
instance CArg Int32      where cArg = PrimIntArg . fromIntegral
instance CArg Int64      where cArg = PrimIntArg . fromIntegral
instance CArg Int        where cArg = PrimIntArg . fromIntegral
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
lua_error_hs l tid r = reentryG "lua_error" [] l tid r $ \_ args ->
  ThrowError <$> (pop args `catch` \SomeException{} -> return Nil)

--------------------------------------------------------------------------------
-- Stack push functions
--------------------------------------------------------------------------------

foreign export ccall lua_pushlightuserdata_hs :: EntryPoint (Ptr () -> IO CInt)

lua_pushlightuserdata_hs :: EntryPoint (Ptr () -> IO CInt)
lua_pushlightuserdata_hs l tid r u =
  reentryG "lua_pushlightuserdata" [cArg u] l tid r $ \_ stack ->
    do push stack (LightUserData u)
       noResult

foreign export ccall lua_pushnumber_hs :: EntryPoint (Lua_Number -> IO CInt)

-- | [-0, +1, -]
lua_pushnumber_hs :: EntryPoint (Lua_Number -> IO CInt)
lua_pushnumber_hs l tid r n =
  reentryG "lua_pushnumber" [cArg n] l tid r $ \_ stack ->
    do push stack (Number (Double (realToFrac n)))
       noResult

foreign export ccall lua_pushnil_hs :: EntryPoint (IO CInt)

-- | [-0, +1, -]
lua_pushnil_hs :: EntryPoint (IO CInt)
lua_pushnil_hs l tid r =
  reentryG "lua_pushnil" [] l tid r $ \_ args ->
    do push args Nil
       noResult

foreign export ccall lua_pushinteger_hs :: EntryPoint (Lua_Integer -> IO CInt)

-- | [-0, +1, -]
lua_pushinteger_hs :: EntryPoint (Lua_Integer -> IO CInt)
lua_pushinteger_hs l tid r n =
  reentryG "lua_pushinteger" [cArg n] l tid r $ \_ stack ->
    do push stack (Number (fromIntegral n))
       noResult

foreign export ccall lua_pushboolean_hs :: EntryPoint (CInt -> IO CInt)

-- | [-0, +1, -]
lua_pushboolean_hs :: EntryPoint (CInt -> IO CInt)
lua_pushboolean_hs l tid r b =
  reentryG "lua_pushboolean" [cArg b] l tid r $ \_ stack ->
    do push stack (Bool (b /= 0))
       noResult

foreign export ccall lua_pushstring_hs :: EntryPoint (CString -> Ptr CString -> IO CInt)

-- | [-0, +1, m]
lua_pushstring_hs :: EntryPoint (CString -> Ptr CString -> IO CInt)
lua_pushstring_hs l tid r ptr out =
  reentryG "lua_pushstring" [cstringArg0 ptr] l tid r $ \_ args ->

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
lua_pushlstring_hs l tid r ptr sz out =
  reentryG "lua_pushlstring" [cstringArg (ptr, fromIntegral sz)] l tid r $
  \_ args ->
  do str <- peekLuaString (ptr, fromIntegral sz)
     -- Note that the output string must be null terminated!!
     push args (String str)
     result out =<< luaStringPtr str

foreign export ccall lua_pushvalue_hs :: EntryPoint (CInt -> IO CInt)

-- | [-0, +1, -]
lua_pushvalue_hs :: EntryPoint (CInt -> IO CInt)
lua_pushvalue_hs l tid r ix =
  reentryG "lua_pushvalue" [cArg ix] l tid r $ \vm args ->
  do x <- valueArgument vm (fromIntegral ix) args
     push args x
     noResult

foreign export ccall lua_pushcclosure_hs :: EntryPoint (CFun -> CInt -> IO CInt)

-- [-nup, +1, e]
lua_pushcclosure_hs :: EntryPoint (CFun -> CInt -> IO CInt)
lua_pushcclosure_hs l tid r func nup =
  reentryG "lua_pushcclosure" [cArg func, cArg nup] l tid r $ \vm args ->
  do upvals <- popN args (fromIntegral nup)
     info   <- machLookupCFun vm func
     let upNum = SMV.length upvals
     vs <- IOVector.new upNum
     SMV.iForM_ upvals $ \i v -> IOVector.unsafeWrite vs i =<< newIORef v

     c      <- machNewClosure vm (cFunction CFunName { cfunName = info
                                                     , cfunAddr = func}) vs
     push args (Closure c)
     noResult

foreign export ccall lua_tocfunction_hs
  :: EntryPoint (CInt -> Ptr CFun -> IO CInt)

-- [-0, +0, -]
lua_tocfunction_hs :: EntryPoint (CInt -> Ptr CFun -> IO CInt)
lua_tocfunction_hs l tid r ix out =
  reentryG "lua_tocfunction" [cArg ix] l tid r $ \vm args ->
  do x <- valueArgument vm (fromIntegral ix) args
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
lua_iscfunction_hs l tid r ix out =
  reentryG "lua_iscfunction" [cArg ix] l tid r $ \vm args ->
  do x <- valueArgument vm (fromIntegral ix) args
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
lua_type_hs l tid r ix out =
  reentryG "lua_type" [cArg ix] l tid r $ \vm args ->
  do mb <- valueArgumentOpt vm (fromIntegral ix) args
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
lua_settop_hs l tid r ix =
  reentryG "lua_settop" [cArg ix] l tid r $ \_ stack ->
  do oldLen <- SV.size stack
     let i1 = fromIntegral ix
         newLen | i1 < 0    = oldLen + i1 + 1
                | otherwise = i1
     if oldLen <= newLen
       then replicateM_ (newLen - oldLen) (SV.push stack Nil)
       else SV.shrink stack (oldLen - newLen)
     noResult

foreign export ccall lua_gettop_hs :: EntryPoint (Ptr CInt -> IO CInt)

-- | [-?, +?, -]
lua_gettop_hs :: EntryPoint (Ptr CInt -> IO CInt)
lua_gettop_hs l tid r out =
  reentryG "lua_gettop" [] l tid r $ \_ stack ->
    do n <- SV.size stack
       result out (fromIntegral n)

foreign export ccall lua_rotate_hs :: EntryPoint (CInt -> CInt -> IO CInt)

lua_rotate_hs :: EntryPoint (CInt -> CInt -> IO CInt)
lua_rotate_hs l tid r idx n =
  reentryG "lua_rotate" [cArg idx, cArg n] l tid r $ \_ args ->
    do rotateHelper (fromIntegral idx) (fromIntegral n) args
       noResult

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
lua_absindex_hs l tid r cidx out =
  reentryG "lua_absindex" [cArg cidx] l tid r $ \_ stack ->
  do n <- SV.size stack
     let idx = fromIntegral cidx
         idx' | isPseudo idx || idx > 0 = idx
              | otherwise               = n + idx + 1
     result out (fromIntegral idx')

--------------------------------------------------------------------------------

foreign export ccall lua_settable_hs :: EntryPoint (CInt -> IO CInt)

-- | [-2, +0, e]
lua_settable_hs :: EntryPoint (CInt -> IO CInt)
lua_settable_hs l tid r ix =
  reentryG "lua_settable" [cArg ix] l tid r $ \vm args ->
  do t  <- valueArgument vm (fromIntegral ix) args
     kv <- popN args 2
     case SMV.isVec2 kv of
       Nothing -> luaError' "lua_settable: bad arguments"
       Just (k,v) ->
         m__newindex (machMetatablesRef (vmMachineEnv vm)) noResult t k v

--------------------------------------------------------------------------------

foreign export ccall lua_next_hs :: EntryPoint (CInt -> Ptr CInt -> IO CInt)

-- | [-0,+0,-]
lua_next_hs :: EntryPoint (CInt -> Ptr CInt -> IO CInt)
lua_next_hs l tid r idx out =
  reentryG "lua_next" [cArg idx] l tid r $ \vm args ->
  do t <- tableArgument vm (fromIntegral idx) args
     p <- pop args
     res <- tableNext t p
     case res of
       Nothing    -> result out 0
       Just (x,y) -> do push args x
                        push args y
                        result out 1

--------------------------------------------------------------------------------

foreign export ccall lua_copy_hs :: EntryPoint (CInt -> CInt -> IO CInt)

-- | [-0,+0,-]
lua_copy_hs :: EntryPoint (CInt -> CInt -> IO CInt)
lua_copy_hs l tid r fromidx toidx =
  reentryG "lua_copy" [cArg fromidx, cArg toidx] l tid r $ \vm args ->
  do x <- valueArgument vm (fromIntegral fromidx) args
     assign vm (fromIntegral toidx) x args
     noResult

--------------------------------------------------------------------------------

foreign export ccall lua_isnumber_hs :: EntryPoint (CInt -> Ptr CInt -> IO CInt)

-- | [-0, +0, -]
lua_isnumber_hs :: EntryPoint (CInt -> Ptr CInt -> IO CInt)
lua_isnumber_hs l tid r ix out =
  reentryG "lua_isnumber" [cArg ix] l tid r $ \vm args ->
  do v <- valueArgument vm (fromIntegral ix) args
     result out $ case valueNumber v of
                    Just {} -> 1
                    Nothing -> 0

foreign export ccall lua_isuserdata_hs
  :: EntryPoint (CInt -> Ptr CInt -> IO CInt)

-- | [-0, +0, -]
lua_isuserdata_hs :: EntryPoint (CInt -> Ptr CInt -> IO CInt)
lua_isuserdata_hs l tid r ix out =
  reentryG "lua_isuserdata" [cArg ix] l tid r $ \vm args ->
  do v <- valueArgument vm (fromIntegral ix) args
     result out $ case v of
                    UserData{}      -> 1
                    LightUserData{} -> 1
                    _               -> 0

foreign export ccall lua_isstring_hs
  :: EntryPoint (CInt -> Ptr CInt -> IO CInt)

-- | [-0, +0, -]
lua_isstring_hs :: EntryPoint (CInt -> Ptr CInt -> IO CInt)
lua_isstring_hs l tid r ix out =
  reentryG "lua_isstring" [cArg ix] l tid r $ \vm args ->
  do v <- valueArgument vm (fromIntegral ix) args
     result out $ case v of
                    String{} -> 1
                    Number{} -> 1
                    _        -> 0

foreign export ccall lua_isinteger_hs
  :: EntryPoint (CInt -> Ptr CInt -> IO CInt)

-- | [-0, +0, -]
lua_isinteger_hs :: EntryPoint (CInt -> Ptr CInt -> IO CInt)
lua_isinteger_hs l tid r ix out =
  reentryG "lua_isinteger" [cArg ix] l tid r $ \vm args ->
  do v <- valueArgument vm (fromIntegral ix) args
     result out $ case v of
                    Number Int{} -> 1
                    _            -> 0

foreign export ccall lua_stringtonumber_hs
  :: EntryPoint (CString -> Ptr CSize -> IO CInt)

lua_stringtonumber_hs :: EntryPoint (CString -> Ptr CSize -> IO CInt)
lua_stringtonumber_hs l tid r p out =
  reentryG "lua_stringtonumber" [cstringArg0 p] l tid r $ \_ args ->
    do str <- peekCAString p
       case parseNumber str of
         Nothing -> do result out 0
         Just n  -> do push args (Number n)
                       result out (fromIntegral (1 + length str))


foreign export ccall lua_tointegerx_hs
  :: EntryPoint (CInt -> Ptr CInt -> Ptr Lua_Integer -> IO CInt)

-- | [-0, +0, -]
lua_tointegerx_hs :: EntryPoint (CInt -> Ptr CInt -> Ptr Lua_Integer -> IO CInt)
lua_tointegerx_hs l tid r ix isnum out =
  reentryG "lua_integerx" [cArg ix, cArg isnum] l tid r $ \vm args ->
  do v <- valueArgument vm (fromIntegral ix) args
     case valueInt v of
       Just i  -> pokeNotNull isnum 1 >> result out (fromIntegral i)
       Nothing -> pokeNotNull isnum 0 >> result out 0

foreign export ccall lua_toboolean_hs
  :: EntryPoint (CInt -> Ptr CInt -> IO CInt)

-- | [-0, +0, -]
lua_toboolean_hs :: EntryPoint (CInt -> Ptr CInt -> IO CInt)
lua_toboolean_hs l tid r ix out =
  reentryG "lua_toboolean" [cArg ix] l tid r $ \vm args ->
  do b <- boolArgument vm (fromIntegral ix) args
     result out (if b then 1 else 0)

foreign export ccall lua_tonumberx_hs
  :: EntryPoint (CInt -> Ptr CInt -> Ptr Lua_Number -> IO CInt)

-- | [-0, +0, -]
lua_tonumberx_hs :: EntryPoint (CInt -> Ptr CInt -> Ptr Lua_Number -> IO CInt)
lua_tonumberx_hs l tid r ix isnum out =
  reentryG "lua_tonumberx" [cArg ix, cArg isnum] l tid r $ \vm args ->
  do v <- valueArgument vm (fromIntegral ix) args
     case numberToDouble <$> valueNumber v of
       Just d  -> do pokeNotNull isnum 1
                     result out d
       Nothing -> do pokeNotNull isnum 0
                     result out 0

foreign export ccall lua_tolstring_hs
  :: EntryPoint (CInt -> Ptr CSize -> Ptr CString -> IO CInt)

-- | [-0, +0, m]
lua_tolstring_hs :: EntryPoint (CInt -> Ptr CSize -> Ptr CString -> IO CInt)
lua_tolstring_hs l tid r arg len out =
  reentryG "lua_tolstring" [cArg arg, cArg len] l tid r $ \vm args ->
  do let idx = fromIntegral arg
     mb <- valueArgument vm idx args
     case mb of

       String str ->
         do pokeNotNull len (fromIntegral (luaStringLen str))
            result out =<< luaStringPtr str

       Number n ->
         do str <-
                do str <- fromByteString (B8.pack (numberToString n))
                   pokeNotNull len (fromIntegral (luaStringLen str))
                   return str

            assign vm idx (String str) args
            result out =<< luaStringPtr str

       _ ->
         do pokeNotNull len 0
            result out nullPtr

foreign export ccall lua_touserdata_hs
  :: EntryPoint (CInt -> Ptr (Ptr ()) -> IO CInt)

-- | [-0, +0, -]
lua_touserdata_hs :: EntryPoint (CInt -> Ptr (Ptr ()) -> IO CInt)
lua_touserdata_hs l tid r arg out =
  reentryG "lua_touserdata" [cArg arg] l tid r $ \vm args ->
  do v <- valueArgument vm (fromIntegral arg) args
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
lua_getfield_hs l tid r ix k out =
  reentryG "lua_getfield" [cArg ix, cstringArg0 k] l tid r $ \vm args ->
  do t <- valueArgument vm (fromIntegral ix) args
     key <- peekLuaString0 k
     let tabs  = machMetatablesRef (vmMachineEnv vm)
         after v = do push args v
                      result out (lua_type_int v)
     m__index tabs after t (String key)


foreign export ccall lua_setfield_hs
  :: EntryPoint (CInt -> CString -> IO CInt)

-- | [-1, +0, e]
lua_setfield_hs :: EntryPoint (CInt -> CString -> IO CInt)
lua_setfield_hs l tid r ix k =
  reentryG "lua_setfield" [cArg ix, cstringArg0 k] l tid r $ \vm args ->
  do t <- valueArgument vm (fromIntegral ix) args
     v <- pop args
     key <- peekLuaString0 k
     let tabs = machMetatablesRef (vmMachineEnv vm)
     m__newindex tabs noResult t (String key) v

foreign export ccall lua_createtable_hs :: EntryPoint (CInt -> CInt -> IO CInt)

-- | [-0, +1, e]
lua_createtable_hs :: EntryPoint (CInt -> CInt -> IO CInt)
lua_createtable_hs l tid r na nh =
  reentryG "lua_createtable" [cArg na, cArg nh] l tid r $ \vm args ->
  do t <- machNewTable vm (fromIntegral na) (fromIntegral nh)
     push args (Table t)
     noResult

foreign export ccall lua_rawgeti_hs
  :: EntryPoint (CInt -> Lua_Integer -> Ptr CInt -> IO CInt)

lua_rawgeti_hs :: EntryPoint (CInt -> Lua_Integer -> Ptr CInt -> IO CInt)
lua_rawgeti_hs l tid r ix n out =
  reentryG "lua_rawgeti" [cArg ix, cArg n] l tid r $ \vm args ->
  do t <- tableArgument vm (fromIntegral ix) args
     v <- getTableRaw t (Number (fromIntegral n))
     push args v
     result out (lua_type_int v)

foreign export ccall lua_rawgetp_hs
  :: EntryPoint (CInt -> Ptr () -> Ptr CInt -> IO CInt)

lua_rawgetp_hs :: EntryPoint (CInt -> Ptr () -> Ptr CInt -> IO CInt)
lua_rawgetp_hs l tid r ix p out =
  reentryG "lua_rawgetp" [cArg ix, cArg p] l tid r $ \vm args ->
  do t <- tableArgument vm (fromIntegral ix) args
     v <- getTableRaw t (LightUserData p)
     push args v
     result out (lua_type_int v)

foreign export ccall lua_rawget_hs
  :: EntryPoint (CInt -> Ptr CInt -> IO CInt)

lua_rawget_hs :: EntryPoint (CInt -> Ptr CInt -> IO CInt)
lua_rawget_hs l tid r ix out =
  reentryG "lua_rawget" [cArg ix] l tid r $ \vm args ->
  do t <- tableArgument vm (fromIntegral ix) args
     k <- pop args
     v <- getTableRaw t k
     push args v
     result out (lua_type_int v)

foreign export ccall lua_geti_hs
  :: EntryPoint (CInt -> Lua_Integer -> Ptr CInt -> IO CInt)

lua_geti_hs :: EntryPoint (CInt -> Lua_Integer -> Ptr CInt -> IO CInt)
lua_geti_hs l tid r ix n out =
  reentryG "lua_geti" [cArg ix, cArg n] l tid r $ \vm args ->
  do t <- valueArgument vm (fromIntegral ix) args
     let tabs = machMetatablesRef (vmMachineEnv vm)
         after v = push args v >> result out (lua_type_int v)
     m__index tabs after t (Number (fromIntegral n))

foreign export ccall lua_gettable_hs
  :: EntryPoint (CInt -> Ptr CInt -> IO CInt)

lua_gettable_hs :: EntryPoint (CInt -> Ptr CInt -> IO CInt)
lua_gettable_hs l tid r ix out =
  reentryG "lua_gettable" [cArg ix] l tid r $ \vm args ->
  do t <- valueArgument vm (fromIntegral ix) args
     k <- pop args
     let tabs = machMetatablesRef (vmMachineEnv vm)
         after v = push args v >> result out (lua_type_int v)
     m__index tabs after t k

foreign export ccall lua_rawset_hs
  :: EntryPoint (CInt -> IO CInt)

lua_rawset_hs :: EntryPoint (CInt -> IO CInt)
lua_rawset_hs l tid r ix =
  reentryG "lua_rawset" [cArg ix] l tid r $ \vm args ->
  do t <- tableArgument vm (fromIntegral ix) args
     kv <- popN args 2
     case SMV.isVec2 kv of
       Nothing -> luaError' "lua_rawset: missing arguments"
       Just (k,v) ->
         case k of
           Number (Double nan) | isNaN nan
                  -> luaError' "Invalid table index NaN"
           Nil -> luaError' "Invalid table index nil"
           _   -> setTableRaw t k v >> noResult

foreign export ccall lua_seti_hs
  :: EntryPoint (CInt -> Lua_Integer -> IO CInt)

lua_seti_hs :: EntryPoint (CInt -> Lua_Integer -> IO CInt)
lua_seti_hs l tid r ix n =
  reentryG "lua_seti" [cArg ix, cArg n] l tid r $ \vm args ->
  do t <- valueArgument vm (fromIntegral ix) args
     v <- pop args
     let tabs = machMetatablesRef (vmMachineEnv vm)
     m__newindex tabs noResult t (Number (fromIntegral n)) v

foreign export ccall lua_rawseti_hs
  :: EntryPoint (CInt -> Lua_Integer -> IO CInt)

lua_rawseti_hs :: EntryPoint (CInt -> Lua_Integer -> IO CInt)
lua_rawseti_hs l tid r ix n =
  reentryG "lua_rawseti" [cArg ix, cArg n] l tid r $ \vm args ->
  do t <- tableArgument vm (fromIntegral ix) args
     v <- pop args
     setTableRaw t (Number (fromIntegral n)) v
     noResult

foreign export ccall lua_rawsetp_hs
  :: EntryPoint (CInt -> Ptr () -> IO CInt)

lua_rawsetp_hs :: EntryPoint (CInt -> Ptr () -> IO CInt)
lua_rawsetp_hs l tid r ix p =
  reentryG "lua_rawsetp" [cArg ix, cArg p] l tid r $ \vm args ->
  do t <- tableArgument vm (fromIntegral ix) args
     v <- pop args
     setTableRaw t (LightUserData p) v
     noResult

------------------------------------------------------------------------

foreign export ccall lua_rawequal_hs
  :: EntryPoint (CInt -> CInt -> Ptr CInt -> IO CInt)

lua_rawequal_hs :: EntryPoint (CInt -> CInt -> Ptr CInt -> IO CInt)
lua_rawequal_hs l tid r ix1 ix2 out =
  reentryG "lua_rawequal" [cArg ix1, cArg ix2] l tid r $ \vm args ->
  do x <- valueArgumentOpt vm (fromIntegral ix1) args
     y <- valueArgumentOpt vm (fromIntegral ix2) args
     result out $
       case (x,y) of
          (Just a, Just b) | a == b -> 1
          _                         -> 0

------------------------------------------------------------------------

foreign export ccall lua_compare_hs
  :: EntryPoint (CInt -> CInt -> CInt -> Ptr CInt -> IO CInt)

-- [-0, +0, e]
lua_compare_hs :: EntryPoint (CInt -> CInt -> CInt -> Ptr CInt -> IO CInt)
lua_compare_hs l tid r ix1 ix2 op out =
  reentryG "lua_compare" [cArg ix1, cArg ix2, cArg op] l tid r $ \vm args ->
  -- Note: Also returns 0 if any of the indices is not valid.
  do mbx <- valueArgumentOpt vm (fromIntegral ix1) args
     mby <- valueArgumentOpt vm (fromIntegral ix2) args
     let tabs      = machMetatablesRef (vmMachineEnv vm)
         after res = result out (if res then 1 else 0)
     case (mbx,mby) of
       (Just x, Just y) ->
         case op of
           #{const LUA_OPEQ} -> m__eq tabs after x y
           #{const LUA_OPLT} -> m__lt tabs after x y
           #{const LUA_OPLE} -> m__le tabs after x y
           _                 -> luaError' "lua_compare: bad operator"

       _ -> result out 0

------------------------------------------------------------------------

foreign export ccall lua_arith_hs :: EntryPoint (CInt -> IO CInt)

-- [-(2|1), +1, e]
lua_arith_hs :: EntryPoint (CInt -> IO CInt)
lua_arith_hs l tid r opNum =
  reentryG "lua_arith" [cArg opNum] l tid r $ \vm args ->
    let tabs    = machMetatablesRef (vmMachineEnv vm)
        after z = push args z >> noResult
        op1 f   = do x <- pop args
                     f tabs after x
        op2 f   = do x <- pop args
                     y <- pop args
                     f tabs after x y
    in case opNum of
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
      _                   -> luaError' "lua_arith: bad operator"


foreign export ccall lua_getmetatable_hs
  :: EntryPoint (CInt -> Ptr CInt -> IO CInt)

lua_getmetatable_hs :: EntryPoint (CInt -> Ptr CInt -> IO CInt)
lua_getmetatable_hs l tid r ix out =
  reentryG "lua_getmetatable" [cArg ix] l tid r $ \vm args ->
  do v  <- valueArgument vm (fromIntegral ix) args
     mt <- valueMetatable (machMetatablesRef (vmMachineEnv vm)) v
     case mt of
       Nothing -> do result out 0
       Just m  -> do push args (Table m)
                     result out 1

------------------------------------------------------------------------

foreign export ccall lua_newuserdata_hs
  :: EntryPoint (CSize -> Ptr (Ptr ()) -> IO CInt)

-- | [-0, +1, e]
lua_newuserdata_hs :: EntryPoint (CSize -> Ptr (Ptr ()) -> IO CInt)
lua_newuserdata_hs l tid r sz out =
  reentryG "lua_newuserdata" [cArg sz] l tid r $ \vm args ->
  do let sz' = fromIntegral sz
     fptr <- mallocForeignPtrBytes sz'
     u    <- machNewUserData vm fptr sz'
     let garbage = machGarbage (vmMachineEnv vm)
     addRefFinalizer u (atomicModifyIORef garbage (\xs -> (UserData u : xs, ())))

     push args (UserData u)
     result out (unsafeForeignPtrToPtr fptr)

foreign export ccall lua_getuservalue_hs
  :: EntryPoint (CInt -> Ptr CInt -> IO CInt)

lua_getuservalue_hs :: EntryPoint (CInt -> Ptr CInt -> IO CInt)
lua_getuservalue_hs l tid r ix out =
  reentryG "lua_getuservalue" [cArg ix] l tid r $ \vm args ->
  do v <- valueArgument vm (fromIntegral ix) args
     case v of
       UserData uref ->
          do uservalue <- readIORef (userDataValue (referenceVal uref))
             push args uservalue
             result out (lua_type_int uservalue)
       _ -> luaError' "lua_getuservalue: argument is not full userdata"

foreign export ccall lua_setuservalue_hs
  :: EntryPoint (CInt -> IO CInt)

lua_setuservalue_hs :: EntryPoint (CInt -> IO CInt)
lua_setuservalue_hs l tid r ix =
  reentryG "lua_setuservalue" [cArg ix] l tid r $ \vm args ->
  do v         <- valueArgument vm (fromIntegral ix) args
     uservalue <- pop args
     case v of
       UserData uref ->
         do writeIORef (userDataValue (referenceVal uref)) uservalue
            noResult
       _ -> luaError' "lua_setuservalue: argument is not full userdata"

------------------------------------------------------------------------

lua_multret :: CInt
lua_multret = (#const LUA_MULTRET)

foreign export ccall lua_callk_hs
  :: EntryPoint (CInt -> CInt -> Lua_KContext -> Lua_KFunction -> IO CInt)

-- [-(nargs+1), +nresults, e]
lua_callk_hs :: EntryPoint (CInt -> CInt -> Lua_KContext -> Lua_KFunction -> IO CInt)
lua_callk_hs l tid r narg nresult ctx k =
  reentryGK ctx k
     "lua_callk" [cArg narg, cArg nresult, cArg ctx, cArg k] l tid r $
  \vm args ->
  do fxs <- popN args (fromIntegral narg + 1)
     case SMV.uncons fxs of
       Nothing -> luaError' "lua_callk: invalid narg"
       Just (f,xs) ->
         let tabs = machMetatablesRef (vmMachineEnv vm)
             after rs =
               do if nresult == lua_multret
                    then SMV.forM_ rs (push args)
                    else SMV.padForM_ rs (fromIntegral nresult) Nil (push args)
                  noResult
         in m__call tabs after f xs


type ApiLuaPcallk = EntryPoint
  (CInt -> CInt -> CInt -> Lua_KContext -> Lua_KFunction -> Ptr CInt -> IO CInt)

foreign export ccall lua_pcallk_hs :: ApiLuaPcallk

-- | [-0, +1, e]
lua_pcallk_hs :: ApiLuaPcallk
lua_pcallk_hs l tid r narg nresult msgh ctx k out =
  reentryGK ctx k
     "lua_pcallk" [cArg narg, cArg nresult, cArg msgh, cArg ctx, cArg k] l tid r $ \vm args ->
  do h <- if msgh == 0
            then return DefaultHandler
            else FunHandler <$> functionArgument vm (fromIntegral msgh) args
     fxs <- popN args (fromIntegral narg + 1)
     case SMV.uncons fxs of
       Nothing -> luaError' "lua_pcallk: invalid narg"
       Just (f,xs) ->
         let tabs = machMetatablesRef (vmMachineEnv vm)
             afterResolve (f',xs') = return $! FunCall f' xs' (Just hdlr) ifOK

             ifOK rs =
               do if nresult == lua_multret
                     then SMV.forM_ rs (push args)
                     else SMV.padForM_ rs (fromIntegral nresult) Nil (push args)
                  result out luaOK

             ifErr e =
               do push args e
                  result out luaERRRUN

             hdlr = Handler
                     { handlerType = h
                     , handlerK = ifErr
                     }

         in resolveFunction tabs afterResolve f xs



------------------------------------------------------------------------

foreign export ccall lua_setmetatable_hs :: EntryPoint (CInt -> IO CInt)

-- | [-1, +0, e]
lua_setmetatable_hs :: EntryPoint (CInt -> IO CInt)
lua_setmetatable_hs l tid r idx =
  reentryG "lua_setmetatable" [cArg idx] l tid r $ \vm args ->
  do v <- valueArgument vm (fromIntegral idx) args
     m <- pop args
     let tabs = machMetatablesRef (vmMachineEnv vm)
     case m of -- must be non-empty becuase previous line worked
       Nil     -> setMetatable tabs Nothing v >> noResult
       Table t -> setMetatable tabs (Just t) v >> noResult
       _       -> luaError' "lua_setmetatable expected table argument"

------------------------------------------------------------------------

foreign export ccall lua_len_hs :: EntryPoint (CInt -> IO CInt)

-- | [-0, +1, e]
lua_len_hs :: EntryPoint (CInt -> IO CInt)
lua_len_hs l tid r idx =
  reentryG "lua_len" [cArg idx] l tid r $ \vm args ->
  do v <- valueArgument vm (fromIntegral idx) args
     let tabs = machMetatablesRef (vmMachineEnv vm)
         after len = push args len >> noResult
     m__len tabs after v

foreign export ccall lua_rawlen_hs :: EntryPoint (CInt -> Ptr CSize -> IO CInt)

-- | [-0, +0, –]
lua_rawlen_hs :: EntryPoint (CInt -> Ptr CSize -> IO CInt)
lua_rawlen_hs l tid r idx out =
  reentryG "lua_rawlen" [cArg idx] l tid r $ \vm args ->
  do v   <- valueArgument vm (fromIntegral idx) args
     len <- case v of
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
lua_load_hs l tid r chunk chunksize chunkname mode out =
  -- Note: this is an internal entry point, the C api handles the Reader
  reentryG "lua_load" [cstringArg0 chunkname, cstringArg0 mode] l tid r
     $ \vm args ->
    do chunk1<- B.packCStringLen (chunk, fromIntegral chunksize)
       name  <- if nullPtr == chunkname
                  then return Nothing
                  else Just <$> peekCAString chunkname
       let menv    = vmMachineEnv vm
           globals = machGlobals menv
       eclo <- chunkToClosure vm name chunk1 (Table globals)
       case eclo of
         Left e    -> do e' <- fromByteString (B8.pack e)
                         push args (String e')
                         result out 1
         Right clo -> do push args (Closure clo)
                         result out 0


------------------------------------------------------------------------

foreign export ccall lua_setglobal_hs :: EntryPoint (CString -> IO CInt)

lua_setglobal_hs :: EntryPoint (CString -> IO CInt)
lua_setglobal_hs l tid r cname =
  reentryG "lua_setglobal" [cstringArg0 cname] l tid r $ \vm args ->
  do v <- pop args
     let t = machGlobals (vmMachineEnv vm)
     k <- peekLuaString0 cname
     setTableRaw t (String k) v
     noResult

foreign export ccall lua_getglobal_hs :: EntryPoint (CString -> Ptr CInt -> IO CInt)

lua_getglobal_hs :: EntryPoint (CString -> Ptr CInt -> IO CInt)
lua_getglobal_hs l tid r cname out =
  reentryG "lua_getglobal" [cstringArg0 cname] l tid r $ \vm args ->
  do let t = machGlobals (vmMachineEnv vm)
     k <- peekLuaString0 cname
     v <- getTableRaw t (String k)
     push args v
     result out (lua_type_int v)

------------------------------------------------------------------------

foreign export ccall lua_concat_hs :: EntryPoint (CInt -> IO CInt)

-- [-n, +1, e]
lua_concat_hs :: EntryPoint (CInt -> IO CInt)
lua_concat_hs l tid r n =
  reentryG "lua_concat" [cArg n] l tid r $ \vm args ->
  do xs <- popN args (fromIntegral n)
     let tabs = machMetatablesRef (vmMachineEnv vm)
         after res = push args res >> noResult
     m__concat tabs after xs

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
lua_dump_hs l tid r writer dat strip out =
  reentryG "lua_dump" [cArg writer, cArg dat, cArg strip] l tid r $ \vm args ->
    do clo <- functionArgument vm (-1) args
       (_,fun) <- case luaOpCodes (cloFun (referenceVal clo)) of
                Nothing -> throwIO (LuaX "expected lua function") -- ?
                Just fun -> return fun
       let bytecode = dumpLuaBytecode fun
           loop [] = result out 0
           loop (x:xs) =
             U.unsafeUseAsCStringLen x $ \(ptr, len) ->
               do res <- lua_writer writer l (castPtr ptr) (fromIntegral len) dat
                  if res == 0 then
                    loop xs
                  else
                    result out (fromIntegral res)

       loop (L.toChunks bytecode)


------------------------------------------------------------------------

pokeNotNull :: Storable a => Ptr a -> a -> IO ()
pokeNotNull ptr a = unless (ptr == nullPtr) (poke ptr a)

--------------------------------------------------------------------------------
-- Coroutines


foreign export ccall
  lua_tothread_hs :: EntryPoint (CInt -> Ptr (Ptr ()) -> IO CInt)

lua_tothread_hs :: EntryPoint (CInt -> Ptr (Ptr ()) -> IO CInt)
lua_tothread_hs l tid r n out =
  reentryG "lua_tothread" [cArg n] l tid r $ \vm args ->
    do v <- valueArgument vm (fromIntegral n) args
       result out $
         case v of
           Thread t -> unsafeForeignPtrToPtr (threadCPtr (referenceVal t))
           _        -> nullPtr

foreign export ccall
  lua_pushthread_hs :: EntryPoint (Ptr CInt -> IO CInt)

lua_pushthread_hs :: EntryPoint (Ptr CInt -> IO CInt)
lua_pushthread_hs l tid r out =
  reentryG "lua_pushthread" [] l tid r $ \vm args ->
    do let curT   = vmCurThread vm
           isMain = machIsMainThread vm curT
       push args (Thread curT)
       result out (if isMain then 1 else 0)


foreign export ccall
  lua_status_hs :: EntryPoint (Ptr CInt -> IO CInt)

lua_status_hs :: EntryPoint (Ptr CInt -> IO CInt)
lua_status_hs l tid r out =
  reentryG "lua_status" [] l tid r $ \vm _ ->
     do thread <- extToThreadRef vm tid
        st     <- getThreadField threadStatus thread

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
lua_resume_hs l tid r from nargs out =
  reentryG "lua_resume" [cArg from, cArg nargs] l tid r $ \vm args ->
    do tRef <- extToThreadRef vm tid

       st <- getThreadField threadStatus tRef
       case st of
         ThreadSuspended _ ->
            do doResume tRef args

         ThreadNew ->
            do resumeArgs <- popN args (fromIntegral nargs)

               closure <- functionArgument vm (-1) args
               void (pop args)

               activateThread closure resumeArgs tRef
               doResume tRef args

         _ -> do e <- fromByteString "Thread not resumable"
                 finishWithError args (String e)

  where
  finishWithError args e =
    do push args e
       result out luaERRRUN

  doResume tRef leftover =
    return $ Resume tRef $ \res ->
      case res of
        ThreadReturn rs ->
          do eenv <- getThreadField stExecEnv tRef
             SV.resetTo (execCStack eenv) rs
             result out luaOK

        ThreadYield -> result out luaYIELD

        ThreadError e -> finishWithError leftover e


foreign export ccall
  lua_yieldk_hs :: EntryPoint (CInt -> Lua_KContext -> Lua_KFunction -> IO CInt)

lua_yieldk_hs :: EntryPoint (CInt -> Lua_KContext -> Lua_KFunction -> IO CInt)
lua_yieldk_hs l tid r nResults ctx func =
  reentryGK ctx func
     "lua_yieldk" [cArg nResults, cArg ctx, cArg func] l tid r $ \vm args ->
    do outputs <- popN args (fromIntegral nResults)

       tRef  <- extToThreadRef vm tid
       stack <- execCStack <$> getThreadField stExecEnv tRef
       SV.resetTo stack outputs
       return $ Yield $!
            if nullFunPtr == func
                then do xs <- SV.getAll stack
                        return $! FunReturn xs
                else do let token = unsafeForeignPtrToPtr (threadCPtr (referenceVal tRef))
                        putMVar (machCServer (vmMachineEnv vm))
                                (CCallback (capi_entryk ctx func luaYIELD token))
                        return WaitForC


foreign export ccall
  lua_newthread_hs :: EntryPoint (Ptr (Ptr ()) -> IO CInt)

lua_newthread_hs :: EntryPoint (Ptr (Ptr ()) -> IO CInt)
lua_newthread_hs l tid r out =
  reentryG "lua_newthread" [] l tid r $ \vm args ->
    do threadRef <- machNewThread vm
       push args (Thread threadRef)
       result out (unsafeForeignPtrToPtr (threadCPtr (referenceVal threadRef)))

foreign export ccall
  lua_isyieldable_hs :: EntryPoint (Ptr CInt -> IO CInt)

lua_isyieldable_hs :: EntryPoint (Ptr CInt -> IO CInt)
lua_isyieldable_hs l tid r out =
  reentryG "lua_isyieldable" [] l tid r $ \vm _ ->
    do tRef <- extToThreadRef vm tid

       let isMain = machIsMainThread vm tRef
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
    EQ -> do pc <- getThreadPC thread
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
lua_getstack_hs l tid r level ar out =
  reentryG "lua_getstack" [cArg level, cArg (castPtr ar :: Ptr ())] l tid r $ \vm _ ->

  do tRef <- extToThreadRef vm tid
     mbExecEnv <- findExecEnv (fromIntegral level) tRef

     case mbExecEnv of
       -- get stack doesn't access a thread's initial stack for whatever reason
       Just execEnv | not (atBottomOfStack (snd execEnv)) ->
         do pokeLuaDebugCallInfo ar =<< exportExecEnv execEnv
            result out 1

       _ -> result out 0


atBottomOfStack :: ExecEnv -> Bool
atBottomOfStack env =
  case env of
    ExecInLua {} -> False
    ExecInC cenv -> cfunAddr (cExecFunction cenv) == nullFunPtr


------------------------------------------------------------------------

foreign export ccall
  lua_getinfo_hs :: EntryPoint (CString -> Ptr LuaDebug -> Ptr CInt -> IO CInt)

lua_getinfo_hs :: EntryPoint (CString -> Ptr LuaDebug -> Ptr CInt -> IO CInt)
lua_getinfo_hs l tid r whatPtr ar out =
  reentryG "lua_getinfo" [cstringArg0 whatPtr, cArg (castPtr ar :: Ptr ())] l tid r $ \vm args ->
  do what         <- peekCString whatPtr

     (pc,execEnv) <- if '>'`elem` what
                       then do th <- extToThreadRef vm tid
                               pc <- getThreadPC th
                               eenv <- getThreadField stExecEnv th
                               return (pc,eenv)
                       else importExecEnv =<< peekLuaDebugCallInfo ar

     let luaWhat fid = if isRootFun fid then "main" else "Lua"

     case execEnv of


       ExecInLua lenv ->

         do let fid = luaExecFID lenv
                fun = luaExecFunction lenv
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


       ExecInC cenv ->
         do let funName    = cfunName (cExecFunction cenv)
                funNameStr = Text.unpack
                           $ fromMaybe (cObjAddr funName) (cObjName funName)

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


     when ('f' `elem` what) (push args (execClosure execEnv))
     when ('L' `elem` what) (push args Nil)
     result out 1

------------------------------------------------------------------------

foreign export ccall
  lua_getlocal_hs :: EntryPoint (Ptr LuaDebug -> CInt -> Ptr CString -> IO CInt)

lua_getlocal_hs :: EntryPoint (Ptr LuaDebug -> CInt -> Ptr CString -> IO CInt)
lua_getlocal_hs l tid r ar n out =
  reentryG "lua_getlocal" [cArg (castPtr ar :: Ptr ()), cArg n] l tid r $ \vm args ->
   do if ar == nullPtr
        then getLocalFunArgs vm (fromIntegral n) out args
        else getLocalStackArgs (fromIntegral n) out args ar
      noResult

getLocalFunArgs :: VM -> Int -> Ptr CString -> SV.SizedVector Value -> IO ()
getLocalFunArgs vm n out args =
  do clo <- functionArgument vm (-1) args
     void (pop args)
     case luaOpCodes (cloFun (referenceVal clo)) of
       Just (_,fun) ->
         case lookupLocalName fun 0 (Reg (n-1)) of
           Nothing -> poke out nullPtr
           Just bs -> poke out =<< newCAString (B8.unpack bs)--XXX: Leak
       _ -> poke out nullPtr

getLocalStackArgs :: Int -> Ptr CString -> SV.SizedVector Value -> Ptr LuaDebug -> IO ()
getLocalStackArgs n out args ar =
  do (pc,execEnv) <- importExecEnv =<< peekLuaDebugCallInfo ar

     let stack = execCStack execEnv

     let ix = fromIntegral n - 1


     case execEnv of
       ExecInLua LuaExecEnv { luaExecFunction = func }
         | Just name <- lookupLocalName func pc (Reg ix) ->
             do len <- SV.size stack
                if 0 <= ix && ix < len
                  then do val <- SV.get stack ix
                          poke out =<< newCAString (B8.unpack name)
                          push args val

                  else do poke out nullPtr

       _ -> do poke out nullPtr

------------------------------------------------------------------------

foreign export ccall
  lua_getupvalue_hs :: EntryPoint (CInt -> CInt -> Ptr CString -> IO CInt)

lua_getupvalue_hs :: EntryPoint (CInt -> CInt -> Ptr CString -> IO CInt)
lua_getupvalue_hs l tid r funcindex n out =
  reentryG "lua_getupvalue" [cArg funcindex, cArg n] l tid r $ \vm args ->
    do v <- valueArgument vm (fromIntegral funcindex) args

       let n' = fromIntegral n - 1

       result out =<<
         case v of
           Closure ref ->
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
lua_setlocal_hs l tid r ar n out =
  reentryG "lua_setlocal" [cArg (castPtr ar :: Ptr ()), cArg n] l tid r $ \_ args ->

  do (pc,execEnv) <- importExecEnv =<< peekLuaDebugCallInfo ar

     let stack = execCStack execEnv

     let ix = fromIntegral n - 1

     result out =<<
      case execEnv of
        ExecInLua LuaExecEnv { luaExecFunction = func }
         | Just name <- lookupLocalName func pc (Reg ix) ->
           do sz <- SV.size stack
              if 0 <= ix && ix < sz
                then do v <- pop args
                        SV.set stack ix v
                        newCAString (B8.unpack name)
                else return nullPtr

        _ -> return nullPtr

------------------------------------------------------------------------

foreign export ccall
  lua_setupvalue_hs :: EntryPoint (CInt -> CInt -> Ptr CString -> IO CInt)

lua_setupvalue_hs :: EntryPoint (CInt -> CInt -> Ptr CString -> IO CInt)
lua_setupvalue_hs l tid r funcindex n out =
  reentryG "lua_setupvalue" [cArg funcindex, cArg n] l tid r $ \vm args ->

    do v <- valueArgument vm (fromIntegral funcindex) args

       let n' = fromIntegral n - 1

       result out =<<
        case v of
         Closure ref ->
            do let clo = referenceVal ref
                   ups = cloUpvalues clo

               mb <- IOVector.readMaybe ups n'
               case mb of
                 Just uv ->
                   do x <- pop args
                      writeIORef uv x
                      newCAString (upvalueName (cloFun clo) n') -- XXX: Leak

                 Nothing -> return nullPtr

         _ -> return nullPtr

------------------------------------------------------------------------

foreign export ccall
  lua_xmove_hs :: EntryPoint (Int -> CInt -> IO CInt)

lua_xmove_hs :: EntryPoint (Int -> CInt -> IO CInt)
lua_xmove_hs l tid r to n =
  reentryG "lua_xmove" [cArg to, cArg n] l tid r $ \vm fromArgs ->

    do fromRef <- extToThreadRef vm tid
       toRef   <- extToThreadRef vm to

       unless (fromRef == toRef) $
         do transfer <- popN fromArgs (fromIntegral n)

            toStack <- execCStack <$> getThreadField stExecEnv toRef
            SMV.forM_ transfer (push toStack)
       noResult

------------------------------------------------------------------------

foreign export ccall
  lua_upvaluejoin_hs :: EntryPoint (CInt -> CInt -> CInt -> CInt -> IO CInt)

-- Make the n1-th upvalue of the Lua closure at index funcindex1 refer to
-- the n2-th upvalue of the Lua closure at index funcindex2.
-- [-0, +0, –]
lua_upvaluejoin_hs :: EntryPoint (CInt -> CInt -> CInt -> CInt -> IO CInt)
lua_upvaluejoin_hs l tid r f1 n1 f2 n2 =
  reentryG "lua_upvaluejoin" (cArg <$> [f1,n1,f2,n2]) l tid r $ \vm args ->
    do f1' <- valueArgument vm (fromIntegral f1) args
       f2' <- valueArgument vm (fromIntegral f2) args
       let n1' = fromIntegral n1 - 1
           n2' = fromIntegral n2 - 1
       case (f1',f2') of
         (Closure ref1, Closure ref2) ->
           do let v1 = cloUpvalues (referenceVal ref1)
                  v2 = cloUpvalues (referenceVal ref2)

              mbRef <- IOVector.readMaybe v2 n2'
              for_ mbRef $ \ ref ->
                when (0 <= n1' && n1' < IOVector.length v1)
                     (IOVector.write v1 n1' ref)

         _ -> return ()
       noResult

replaceAt :: Int -> a -> Vector a -> Maybe (Vector a)
replaceAt i x v
  | 0 <= i && i < Vector.length v = Just $! v Vector.// [(i,x)]
  | otherwise = Nothing

------------------------------------------------------------------------

foreign export ccall
  lua_upvalueid_hs :: EntryPoint (CInt -> CInt -> Ptr (Ptr ()) -> IO CInt)

lua_upvalueid_hs :: EntryPoint (CInt -> CInt -> Ptr (Ptr ()) -> IO CInt)
lua_upvalueid_hs l tid r funcindex n _out =
  reentryG "lua_upvalueid" (cArg <$> [funcindex,n]) l tid r $ \_ _ ->
    luaError' "lua_upvalueid: not implemented"


foreign export ccall
  lua_gc_hs :: EntryPoint (CInt -> CInt -> Ptr CInt -> IO CInt)

lua_gc_hs :: EntryPoint (CInt -> CInt -> Ptr CInt -> IO CInt)
lua_gc_hs l tid r what dat out =
  reentryG "lua_gc" [cArg what, cArg dat] l tid r $ \_ _ ->
    result out 0


foreign export ccall
  lua_setallocf_hs :: EntryPoint (Lua_Alloc -> Ptr () -> IO CInt)

lua_setallocf_hs :: EntryPoint (Lua_Alloc -> Ptr () -> IO CInt)
lua_setallocf_hs l tid r f ud =
  reentryG "lua_setallocf" [cArg f, cArg ud] l tid r $ \_ _ -> noResult
  -- XXX: Not implemented, ignored


foreign export ccall
  lua_topointer_hs :: EntryPoint (CInt -> Ptr (Ptr ()) -> IO CInt)

lua_topointer_hs :: EntryPoint (CInt -> Ptr (Ptr ()) -> IO CInt)
lua_topointer_hs l tid r ix out =
  reentryG "lua_topointer" [cArg ix] l tid r $ \vm args ->
  do mb <- valueArgument vm (fromIntegral ix) args
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
lua_close_hs l tid r = reentryG "lua_close" [] l tid r $ \vm _args ->
  do let cfg = machConfig (vmMachineEnv vm)
     machOnShutdown cfg
     noResult

------------------------------------------------------------------------

type GaluaControl = EntryPoint (CString -> IO CInt)

foreign export ccall galua_control_hs :: GaluaControl

galua_control_hs :: GaluaControl
galua_control_hs l tid r cmdPtr =
  reentryG "galua_control" [cstringArg0 cmdPtr] l tid r $ \vm args ->
  do cmd <- peekCString cmdPtr
     let onQuery = machOnQuery (machConfig (vmMachineEnv vm))
     answer <- onQuery cmd
     push args answer
     noResult
