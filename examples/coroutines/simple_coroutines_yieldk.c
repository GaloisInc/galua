#include <stdio.h>
#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

static
int generator(lua_State *L) {
    lua_pushinteger(L, 10);
    return lua_yield(L,1);
}

static
int threadbody(lua_State *L, int status, lua_KContext ctx) {
        lua_pushinteger(L, ctx);
        if (ctx < 3) {
           return lua_yieldk(L, 1, ctx+1, threadbody);
        } else {
           return 1;
        }
}

static
int threadmain(lua_State *L) {
    return threadbody(L, LUA_OK, 0);
}

int main(int argc, char *argv[]) {

        lua_State *L = luaL_newstate();

        lua_State *T = lua_newthread(L);
        lua_pushcfunction(T,threadmain);

        int result;
        do {
           result = lua_resume(T, L, 0);
           printf("lua_resume returned %d\n", result);
           lua_xmove(T, L, 1);
           printf("got yielded number: %d\n", (int)lua_tointeger(L, -1));
           lua_pop(L,1);
        } while (result == LUA_YIELD);

        lua_close(L);

        return 0;
}
