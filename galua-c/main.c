#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

extern char *__progname;

extern int galua_argc;
extern int galua_argu;
extern char **galua_argv;

int main(int argc, char**argv) {
  int res;
  int i;

  galua_argc = argc;
  galua_argv = argv;

  lua_State *L = luaL_newstate();
  if (L == NULL) {
    fprintf(stderr, "%s: Failed to initialize interpreter.\n", __progname);
    exit(2);
  }

  argv += 1+galua_argu;
  argc = 0;
  while (argv[argc] != NULL) ++argc;

  if (argc < 1) {
    fprintf(stderr, "%s: No Lua script.\n", __progname);
    exit(EXIT_FAILURE);
  }

  luaL_openlibs(L);
  res = luaL_loadfile(L, argv[0]);
  if (res != LUA_OK) {
    fprintf(stderr, "%s: Failed to load file\n", __progname);
    exit(EXIT_FAILURE);
  }

  for (int i = 1; i < argc; i++) {
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

