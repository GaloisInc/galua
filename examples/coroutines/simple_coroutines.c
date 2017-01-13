#include <stdio.h>
#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

static
int generatork(lua_State *L, int status, lua_KContext ctx) {
    if (ctx < 3) {
        lua_pushinteger(L, ctx);
        return lua_yieldk(L, 1, ctx+1, generatork);
    }
    return 0;
}

static
int generator(lua_State *L) {
    return generatork(L, LUA_OK, 0);
}

static
int testcase(lua_State *L) {
        lua_State *T = lua_newthread(L);
        lua_pushcfunction(T, generator);

        for(;;) {
          int result = lua_resume(T, L, 0);
          if (result == LUA_OK) { break; }

          lua_xmove(T, L, 1);
          long x = lua_tointeger(L, -1);
          lua_pop(L,1);
          printf("Yield %ld\n", x);
        }

        return 0;
}

int main(int argc, char *argv[]) {

        lua_State *L = luaL_newstate();

        lua_pushcfunction(L,testcase);
        int result = lua_pcall(L, 0, 0, 0);

        if (result == LUA_ERRRUN) {
                const char *msg = lua_tostring(L, -1);
                printf("error message: %s\n", msg);
        }

        lua_close(L);

        return 0;
}
