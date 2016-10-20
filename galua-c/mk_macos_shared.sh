#!/bin/bash
rm -rf Lua.framework/Versions/A/Headers
mkdir -p Lua.framework/Versions/A/Headers
cp inplace/include/galua/*.h Lua.framework/Versions/A/Headers/
cc \
        -shared \
        -install_name '@rpath/Lua.framework/Versions/A/Lua' \
        -o Lua.framework/Versions/A/Lua \
        -Linplace/lib \
        -lgalua-dbg -liconv -lz
