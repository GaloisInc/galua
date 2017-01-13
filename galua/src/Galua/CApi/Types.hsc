module Galua.CApi.Types where

import Foreign
import Foreign.C

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
