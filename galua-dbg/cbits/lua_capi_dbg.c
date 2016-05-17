#include <lua.h>
#include "Rts.h"
#include "Galua/Debugger/Server_stub.h"

extern int    galua_argu;
extern int    galua_argc;
extern char **galua_argv;

LUA_API
lua_State *lua_newstate_dbg (lua_Alloc f, void *ud) {
  static int port_offset = 0;
  lua_State *res;
  RtsConfig config = defaultRtsConfig;
  config.rts_opts = "--install-signal-handlers=no";

  if (galua_argc == 0) {
    hs_init_ghc(NULL,NULL,config);
    return galua_newstate_dbg(&galua_argu, port_offset++);
  }

  hs_init_ghc(&galua_argc,&galua_argv,config);
  res = galua_newstate_dbg(&galua_argu, port_offset++);
  return res;
}

