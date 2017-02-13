% Galua: Implementing a Lua Debugger in Haskell
% Iavor Diatchki and Eric Mertens
% 14 February 2016

Overview of Lua
===============

The Lua Language
----------------

* Imperative: loops, updates, etc.
* Memory management via GC.
* First-class functions values.
* Data-structures via hash-tables.
* Exceptions.
* Cooperative thread model.


Example: a Lua program
----------------------

    function f(name,x,y)
      XXX
    end



Lua's C API
-----------

* Lua as a library
    - a well-defined C API

* Used to customize applications via Lua scripts
    - application sets up a Lua environment
    - users specify custom functionality via Lua scripts
    - application executes using the Lua library


Example: Using the C API
------------------------

     lua_getglobal(L, "f");     /* function to be called */
\pause

     lua_pushliteral(L, "how"); /* 1st argument */
\pause

     lua_getglobal(L, "t");     /* table to be indexed */
\pause

     lua_getfield(L, -1, "x");  /* push t.x (2nd arg) */
\pause

     lua_remove(L, -2);         /* remove 't' */
\pause

     lua_pushinteger(L, 14);    /* 3rd argument */
\pause

     lua_call(L, 3, 1);         /* call 'f' */
\pause

     lua_setglobal(L, "a");     /* set global 'a' */


Haskell Implementation
======================

Requirements
------------

* Support the full Lua language
* Use in a similar way to the standard Lua interpreter
* Provide standard debugger features:
    - Pause execution
    - Stepping through code
    - Inspecting the interpreter state
    - Evaluation of expressions


Implementation Overview
-----------------------

* Implementation in Haskell!
    - Initially a study of the semantics of Lua.
    - Uses `luac` to compile Lua to byte-code.
    - Interprets byte-code.

* Debugger provides a web-server interface.

* In-browser client UI, written in JavaScript.


Stack Frames: Local Execution
-----------------------------

![](pdf/debug-state-1.pdf)\


Calling Functions
-----------------

![](pdf/debug-state-2.pdf)\


Returning from Functions
------------------------

![](pdf/debug-state-3.pdf)\



Calling a C Function
--------------------

![](pdf/debug-state-4.pdf)\



Reentry through the C API
-------------------------

![](pdf/debug-state-5.pdf)\


C calls Lua via C API
---------------------

![](pdf/debug-state-6.pdf)\


Exception Handlers
------------------

![](pdf/debug-state-7.pdf)\


Throwing Exceptions: Unroll Stack
---------------------------------

![](pdf/debug-state-8.pdf)\


Challenge: Unroll the C Stack
-----------------------------

![](pdf/debug-state-9.pdf)\



Background: `setjmp` and `longjmp`
----------------------------------

--------  -----------------------------
`setjmp`  Capture current continuation.
          May return multiple times.
`longjmp` Jump to captured continuation.
          Does not return at all.
--------  -----------------------------


        int jumped = setjmp(saveHere);
        if (jumped) {
          // code if jump back
        } else {
          // normal code
        }

Solution: `setjmp` when Leaving
-------------------------------

![](pdf/debug-state-10.pdf)\


Solution: Execution as Normal
-----------------------------

![](pdf/debug-state-11.pdf)\


Solution: `longjmp` to Unroll C Stack
-------------------------------------

![](pdf/debug-state-12.pdf)\



Solution: `longjmp` to Unroll C Stack
-------------------------------------

![](pdf/debug-state-13.pdf)\



Solution: `longjmp` to Unroll C Stack
-------------------------------------

![](pdf/debug-state-14.pdf)\



Cooperative Concurrency
-----------------------

* More complex:
    - we have a stack of call-stacks, one per thread
    - `resume` pushes a new call stack
    - `yield` pops a stack

* Uses similar techniques:
    - Conceptually, `yield` preserves the stack, we just switch threads.
    - If a `resume` happens in C, then `yeild` needs to return.
        - unwind the C parts of the yielded stack, but not the Lua ones.
        - C functions unwound in this way require explicit continuations.


Leaving an Entering Haskell
---------------------------


Background: Haskell's FFI
-------------------------










Reflections
===========

Lua: Too Much Overloading
-------------------------

GHC/Haskell: Data Representation
--------------------------------

![](pdf/object-representations.pdf)\



Packaging and Distribution
---------------------------

  * Depend on a lot of Haskell libraries
  * We would like to provide a single library with a C API.

  * XXX: describe 
  * Improved support in latest Cabal


GHC: Other issues
-----------------

  * PIC
  * Profiling (confusing cost centers in CPS)
  * Reinitializing RTS
  * Signal handling


Conclusion
----------










