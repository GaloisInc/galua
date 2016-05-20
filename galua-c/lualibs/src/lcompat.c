#include <lua.h>
#include <lauxlib.h>

LUALIB_API lua_Unsigned luaL_checkunsigned_(lua_State *L, int arg) {
  return (lua_Unsigned)luaL_checkunsigned(L,arg);
}
#undef luaL_checkunsigned
LUALIB_API lua_Unsigned luaL_checkunsigned(lua_State *L, int arg) {
  return (lua_Unsigned)luaL_checkunsigned_(L,arg);
}
