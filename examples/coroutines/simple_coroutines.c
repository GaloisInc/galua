#include <stdio.h>
#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>


static
int testcase(lua_State *L) {
        lua_State *T = lua_newthread(L);
        luaL_loadstring(T, "return 42");

        lua_resume(T, L, 0);

        lua_getglobal(L, "print");
        lua_xmove(T,L,1);
        lua_call(L, 1, 0);
        return 0;
}

int main(int argc, char *argv[]) {

        lua_State *L = luaL_newstate();
        luaL_openlibs(L);

        lua_pushcfunction(L,testcase);
        lua_pcall(L, 0, 0, 0);

        const char *msg = lua_tostring(L, -1);
        printf("error message: %s\n", msg);

        lua_close(L);

        return 0;
}
