#!/bin/bash
cc -shared -install_name '@rpath/Lua.framework/Versions/A/Lua' -o Lua -Linplace/lib -lgalua-dbg -liconv -lz
