#include <lua.h>
#include <lauxlib.h>
#include <pthread.h>

static int my_pthread_self(lua_State *L) {
        uint64_t tid;
        pthread_threadid_np(pthread_self(), &tid);

        lua_pushinteger(L, tid);
        return 1;
}

static const luaL_Reg func[] =
  { {"self", my_pthread_self}
  , {NULL, NULL}
  };

int luaopen_my_c_module(lua_State *L) {
        luaL_newlib(L, func);
        return 1;
}
