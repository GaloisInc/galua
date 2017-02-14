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

```Lua
    local files = {}
    for i = 1,10 do
      local file = io.open('/dev/null', 'w')
      file:write('hello world: '..i)
      files[i] = file
    end
```



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

```C
lua_getglobal(L, "f");     /* function to be called */
```
\pause

```C
lua_pushliteral(L, "how"); /* 1st argument */
```
\pause

```C
lua_getglobal(L, "t");     /* table to be indexed */
```
\pause

```C
lua_getfield(L, -1, "x");  /* push t.x (2nd arg) */
```
\pause

```C
lua_remove(L, -2);         /* remove 't' */
```
\pause

```C
lua_pushinteger(L, 14);    /* 3rd argument */
```
\pause

```C
lua_call(L, 3, 1);         /* call 'f' */
```
\pause

```C
lua_setglobal(L, "a");     /* set global 'a' */
```


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

```C
int jumped = setjmp(saveHere);
if (jumped) {
  // code if jump back
} else {
  // normal code
}
```

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





Interaction Between C and Haskell
---------------------------------

* We want to expose a C API to the Haskell code

* Export Haskell implementation:

```Haskell
foreign export ccall
  lua_settable_hs :: Ptr () -> CInt -> IO CInt
```

* Provide a C wrapper to deal with unwinding the stack:

```C
int lua_settable (lua_State *L, int index) {
  if (lua_settable_hs(L,index)
    longjmp(*(L->oneror),1);
  return 0;
}
```


Background: Calling Haskell from C
----------------------------------

![](pdf/foreign-call.pdf)\



Handling API Requests: Design Choices
-------------------------------------

![](pdf/handling-requests.pdf)\







Challenges
==========

Lua: Too Much Overloading
-------------------------

* Overall a very nice and simple design.

* Support for "overloading" makes simple looking code complicated.
    - Evaluating `x + y` requires a complex decision procedure,
      potentially evaluating loops!

* Some overhead could be eliminate by JIT.



GHC/Haskell: Data Representation
--------------------------------

![](pdf/object-representations.pdf)\



Packaging and Distribution
---------------------------

  * We depend on a lot of Haskell libraries.
  * We would like to provide a single static C library.
  * Solution:
    - Custom `Setup.hs`
    - Use the Cabal library to compute transitive library dependencies.
    - Link together into a single static library.
    - Hide all but the exposed C API symbols.

  * The relinking and symbol control requires OS specific code.


Issues with GHC
---------------

  * The GHC RTS cannot be re-initialized
      - We don't clean-up the RTS on `lua_close`.
  * On Linux, GHC does not compile libraries with PIC:
      - Galua cannot be integrated in a shared library.
  * GHC RTS emits a lot of signals:
      - May cause confusion with code that does not handle `EAGAIN`.
  * Profiling Galua was difficult:
      - Some code written in CPS
      - Computation attributed to unexpected functions (e.g., `>>=`).


Haskell Pros and Cons
---------------------

* Haskell's FFI worked very well.

* The performance of our Lua implementation is quite a bit worse than
  the reference C implementation:
    - Reference implementation is extremely good!
    - We've done some optimization, but more is possible.
    - Data representation issues are a challenge:
        - some control: strictness, UNPACK; not obvious when to use
        - future GHC extensions are likely to help: mutable record fields.

* Overall working in Haskell was great:
    - Adding the web-server interface was fairly trivial.
    - We could quickly add and try-out new features.
    - The code changed a lot since the beginning:
        - Haskell is great for refactoring code!


Conclusions
-----------

* We implemented a full Lua debugger in Haskell.
    - it works with Lua frameworks "in the wild" (e.g. Love)

* The debugger is just an ordinary (if big) C library.

* The techniques we used are not specific to interpreting Lua:
    - other Haskell algorithms may be implemented and distributed
      in a similar fashion.










