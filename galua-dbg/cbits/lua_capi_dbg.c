#include <lua.h>
#include "Rts.h"
#include "Galua/Debugger/Server_stub.h"

extern int    *galua_argc_p;
extern char ***galua_argv_p;

LUA_API
lua_State *lua_newstate_dbg (lua_Alloc f, void *ud) {
  static int port_offset = 0;
  RtsConfig config = defaultRtsConfig;
  config.rts_opts = "--install-signal-handlers=no";

  hs_init_ghc(galua_argc_p, galua_argv_p, config);
  return galua_newstate_dbg(port_offset++);
}
