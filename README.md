Galua
=====

Galua is a Lua debugger implemented in Haskell. It is intended to work
in place of the reference Lua implementation.

NOTE: Galua currently uses the reference compiler to turn Lua source into
bytecode. This means that the `luac` executable is a runtime dependency
of the debugger when loading Lua source files. This executable path is
configurable with the `LUAC` environment variable.

Galua can operate in stand-alone mode or embedded mode with or without
the debugger enabled

* galua          - stand-alone executable interpreter
* galua-dbg      - stand-alone executable interpreter with debugger
* libgalua.a     - embeddable library interpreter
* libgalua-dbg.a - embeddable library interpreter with debugger

The Galua debugger opens an HTTP server on port 8000 by default. The
Lua environment can be inspected by accessing this service with a
web browser:

```
http://localhost:8000
```

While all modern web browsers are likely to work, Galua is actively tested
using Chrome and Safari.

Building Galua
==============

Building galua requires at least: stack, git, make, gcc

Building galua can be started by executing the top level build script

```
$ ./build.sh
```

The resulting binaries and include files will be available at `galua-c/inplace`

Breakpoints on load
===================

To set a breakpoint before loading a file add a section to your configuration file.

Breakpoints go into the `breakpoints` section. This section should be a list. Each
element of the list starts with a lua chunk name. Lua prefixes filenames with `@`.
Line numbers can follow the chunk name. The empty chunk name matches the first chunk
loaded regardless of name. The `0` line number adds a breakpoint on the first instruction
of a chunk, regardless of line number.

As shown below, the breakpoint `["",0]` will cause the debugger to pause as soon as the
first loaded chunk starts executing.

Example:

```
breakpoints:
  * ["@example.lua", 1, 10, 20]
  * ["@sample.lua", 0]
  * ["", 0]
```

Web interface configuration
===========================

The debugger interface web server is configurable via a config.txt file in
the current directory. The *http* section of this file is specifically for
HTTP configuration options.

* The configuration file uses indentation to group sections.
* Section names and configuration keys are suffixed by colons.
* Strings are surrounded by double-quotes.
* Decimal number arguments do not need quotes.
* Options can be disabled by setting their value to the atom `no`.
* Options without arguments can be enable by setting their value to the atom `yes`.

Example:

```
http:
  address:       "::"
  port:          9000
  no-access-log: yes
```

Settings

* hostname   (string) : local hostname
* address    (string) : address webserver should bind to
* port       (number) : port webserver should bind to
* access-log (string) : filename of access log
* error-log  (string) : filename of error log
* no-access-log (none) : disable access log
* no-error-log (none) : disable error log
