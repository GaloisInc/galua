-- This doesn't quite work, these shouldn't be methods

class METATABLE
  __add<A>: ((A, A) -> A)?
  __sub<A>: ((A, A) -> A)?
  __mul<A>: ((A, A) -> A)?
  __div<A>: ((A, A) -> A)?
  __mod<A>: ((A, A) -> A)?
  __pow<A>: ((A, A) -> A)?
  __unm<A>: (A -> A)?
  __idiv<A>: ((A,A) -> A)?
  __band<A>: ((A,A) -> A)?
  __bxor<A>: ((A,A) -> A)?
  __bnot<A>: (A -> A)?
  __shl<A>: ((A,A) -> A)?
  __shr<A>: ((A,A) -> A)?
  __concat<A>: ((A,A) -> A)?
  __len<A>: (A -> number)?
  __eq<A>: ((A,A) -> boolean)?
  __lt<A>: ((A, A) -> boolean)?
  __le<A>: ((A, A) -> boolean)?
  __index<A>: ((A, dynamic) -> dynamic)?
  __newindex<A>: ((A, dynamic, dynamic) -> ())?
  __call<A>: ((A, dynamic*) -> dynamic*)?

assert<A>: A -> A
collectgarbage: (string?, dynamic) -> boolean?
dofile: string? -> dynamic
error<A>: (dynamic, number?) -> A
getmetatable: dynamic -> METATABLE?

class ITERATOR_STATE
type IPAIRS_ITERATOR<K,V> = ((ITERATOR_STATE, K?) -> (K?, V?), ITERATOR_STATE, K?)

ipairs<A>: {A} -> PAIRS_ITERATOR<number,A>

load: ( string | (() -> string?) | nil, string?, string?, dynamic) -> dynamic*
loadfile: (string?, string?, dynamic) -> dynamic*

next<K,V>: ({K:V}, K?) -> (K?, V?)

pairs<K,V>: {K:V} -> PAIRS_ITERATOR<K,V>
print: dynamic* -> ()
rawequal<A>: (A,A) -> boolean
rawget<K,V>: ({K:V}, K) -> V?

rawlen<A>: {A} -> number
rawlen: string -> number
rawset<K,V>: ({K:V}, K, V) -> {K:V}
select<A>: (number, A*) -> A*
select<A>: (string, A*) -> number
setmetatable<A>: (A, METATABLE?) -> A
tonumber: (string|number, number?) -> number?
tostring<A>: A -> string
type<A>: A -> string
-- xpcall

-- IO module


type STRING_ITERATOR = ((ITERATOR_STATE, string?) -> string*, ITERATOR_STATE, string?)

class FILE
  close: () -> ()
  flush: () -> ()
  lines: string* -> STRING_ITERATOR
  read: (number | string)* -> string*

  seek: (number?, number?) -> (number?, string?)

  setvbuf: (string, number?) -> ()
  write: (number|string)* -> ()

namespace io

  -- Equivalent to file:close(). Without a file, closes the default
  -- output file.
  close: FILE? -> ()

  -- Equivalent to io.output():flush().
  flush: () -> ()

  input: string -> FILE

  lines: (string, string*) -> STRING_ITERATOR

  open: (string, string?) -> FILE

  output: string? -> FILE
  popen: (string, string?) -> FILE
  read: string* -> string*
  tmpfile: () -> FILE
  type: FILE -> string
  write: (number|string)* -> ()

namespace math
  abs: number -> number
  acos: number -> number
  asin: number -> number
  atan: number -> number
  ceil: number -> number
  cos: number -> number
  deg: number -> number
  exp: number -> number
  floor: number -> number
  fmod: number -> number
  huge: number -> number
  log: (number, number?) -> number
  max: (number, number*) -> number
  maxinteger: number
  min: (number, number*) -> number
  mininteger: number
  modf: number -> (number, number)
  pi: number
  rad: number -> number
  random: (number?, number?) -> number
  randomseed: number -> number
  sin: number -> number
  sqrt: number -> number
  tan: number -> number
  type: number -> string
  ult: (number, number) -> boolean

namespace table
  concat: ({ number|string }, string?, number?, number?) -> string
  insert<A>: ({A}, number, A) -> {A}
  insert<A>: ({A}, A) -> {A}
  move<A>: ({A},number,number,number,{A}?) -> {A}
  pack<A>: A* -> {A}
  remove<A>: ({A}, number?) -> {A}
  sort<A>: ({A}, ((A,A) -> number)?) -> {A} -- ORDERING A
  unpack<A>: ({A}, number?, number?) -> A*

class TIME
  hour:   number?
  minute: number?
  sec:    number?
  year:   number
  month:  number
  day:    number

namespace os
  clock: () -> number
  date: (string?, number?) -> string
  difftime: (TIME, TIME) -> number
  execute: string? -> (boolean?, string, number)
  exit: (boolean?, boolean?) -> ()
  getenv: string -> string?
  remove: string -> (boolean?, string?, number?)
  rename: (string, string) -> (boolean?, string?, number?)
  setlocale: (string?, string?) -> string?
  time: TIME? -> number
  tmpname: () -> string

type CODE_ITERATOR = ((ITERATOR_STATE, number?) -> number?, number?, ITERATOR_STATE, number?)

namespace utf8
  char: number* -> string
  charpattern: string
  codes: string -> NUMBER_ITERATOR
  codepoint: (string, number?, number?) -> number*
  len: (string, number?, number?) -> number
  len: (string, number?, number?) -> (boolean, number) -- false only
  offset: (string, number, number?) -> number?
