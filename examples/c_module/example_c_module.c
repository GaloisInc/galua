#include <stdlib.h>
#include "lua.h"
#include "lauxlib.h"

#define MYNAME          "example_c_module"
#define MYVERSION       MYNAME " library for " LUA_VERSION

static int bytes(lua_State *L)
{
    size_t len;
    const char * str = luaL_checklstring(L, 1, &len);

    for (size_t i = 0; i < len; i++) {
        lua_pushinteger(L, str[i]);
    }

    return len;
}

static const luaL_Reg R[] =
{
    { "bytes", bytes },
    { NULL,  NULL }
};

LUALIB_API int luaopen_example_c_module(lua_State *L)
{
    luaL_newlib(L, R);
    return 1;
}
