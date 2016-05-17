#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "lua.h"
#include "lauxlib.h"

#define MYNAME		"example_c_module"
#define MYVERSION	MYNAME " library for " LUA_VERSION

static int Bfibstep(lua_State *L)
{
    lua_Number a=luaL_checknumber(L,1);
    lua_Number b=luaL_checknumber(L,2);
    lua_pushnumber(L,  b);      /* first result */
    lua_pushnumber(L,a+b);      /* second result */
    return 2;                   /* number of results */
}

static int Bstringtest(lua_State *L)
{
    size_t len;
    luaL_checklstring(L, -1, &len);
    lua_pushinteger(L, len);
    return 2;
}

static int Bfstringtest(lua_State *L)
{
    lua_pushfstring(L, "%s\n%f\n%f\n%f\n%U\n%d\n%p\n",
                    "42", 1e100, 42.0, 3.14, 128514, 42, (void*)L);
    return 1;
}

static int storehelper(lua_State *L) {
    lua_pushvalue(L,lua_upvalueindex(1));
    return 1;
}

static int Bstore(lua_State *L)
{
    lua_pushcclosure(L,storehelper, 1);
    return 1;
}

static const luaL_Reg R[] =
{
	{ "fibstep",	Bfibstep	},		/** fibstep(x,y) */
	{ "stringtest",	Bstringtest	},
	{ "store",	Bstore	},
	{ "fstringtest",Bfstringtest},
	{ NULL,		NULL	}
};

LUALIB_API int luaopen_shared_lib(lua_State *L)
{
    luaL_newlib(L,R);
    return 1;
}
