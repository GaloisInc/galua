#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

extern char *__progname;

extern int *galua_argc_p;
extern char ***galua_argv_p;

int Bgalua_control(lua_State *L)
{
  extern void galua_control(lua_State *, const char *);

  const char * command = luaL_checkstring(L, 1);
  galua_control(L, command);
  lua_remove(L, 1);
  return lua_gettop(L);
}

int main(int argc, char**argv)
{
  int res;
  int i;

  galua_argc_p = &argc;
  galua_argv_p = &argv;

  lua_State *L = luaL_newstate();
  if (L == NULL) {
    fprintf(stderr, "%s: Failed to initialize interpreter.\n", __progname);
    exit(2);
  }

  if (argc < 2) {
    fprintf(stderr, "%s: No Lua script.\n", __progname);
    exit(EXIT_FAILURE);
  }

  luaL_openlibs(L);

  lua_register(L, "galuacontrol", Bgalua_control);

  res = luaL_loadfile(L, argv[1]);
  if (res != LUA_OK) {
    const char *msg = lua_tostring(L, -1);
    fprintf(stderr, "%s: Failed to load file\n%s", __progname, msg);
    exit(EXIT_FAILURE);
  }

  for (int i = 2; i < argc; i++) {
    lua_pushstring(L, argv[i]);
  }

  res = lua_pcall(L, argc-1, LUA_MULTRET, 0);
  if (res != LUA_OK) {
    const char *error = lua_tostring(L,-1);
    fprintf(stderr, "Exit with error: %s\n", error);
  }

  lua_close(L);
  return res;
}

