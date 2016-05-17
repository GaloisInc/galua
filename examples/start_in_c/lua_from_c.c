#include <stdio.h>
#include <stdlib.h>

#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

__attribute__((noreturn))
static void failure(const char *msg);

static int my_counter(lua_State *L) {
        lua_pushvalue(L, lua_upvalueindex(1)); // [num]
        lua_pushinteger(L, 1);                 // [num, 1]
        lua_arith(L, LUA_OPADD);               // [num + 1]
        lua_replace(L, lua_upvalueindex(1));   // []

        lua_pushvalue(L, lua_upvalueindex(1)); // [num + 1]
        return 1;
}



int main(int argc, char *argv[]) {

        int res;

        lua_State *L = luaL_newstate();
        luaL_openlibs(L);

        lua_pushinteger(L, 0);
        lua_pushcclosure(L, my_counter, 1);
        lua_setglobal(L, "my_counter");

        res = luaL_dofile(L, "simple.lua");
        if (res) { failure("dofile"); }

        lua_close(L);
}

static void failure (const char *msg) {
        fprintf(stderr, "Error occurred in %s\n", msg);
        exit(EXIT_FAILURE);
}
