#include <lua.h>
#include <lauxlib.h>

int test(lua_State *L) {
        printf("%s\n", lua_pushfstring(L, "%s, %s!", "Hello", "World"));

        lua_Integer i = 42;
        void *p = &i;
        printf("%s\n", lua_pushfstring(L, "Pointer: %%%% %% %p", p));

        printf("%s\n", lua_pushfstring(L, "Utf8: %c%U%c", (int)'<', (long)0x1f61c, (int)'>'));

        printf("%s\n", lua_pushfstring(L, "%%d: %d %%f: %f", (int)42, (double)42));

        lua_pushfstring(L, "%z");

        return 0;
}

int main() {
        lua_State *L = luaL_newstate();

        lua_pushcfunction(L, test);
        int res = lua_pcall(L, 0, 0, 0);

        if (res != LUA_OK) {
            printf("Error:\n");
            printf("%s\n", lua_tostring(L, -1));
        }

        lua_close(L);
}
