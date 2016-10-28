-- IO module

class ITERATOR_STATE

type STRING_ITERATOR = ((ITERATOR_STATE, string?) -> string*, ITERATOR_STATE, string?)

class FILE
  close: () -> ()
  flush: () -> ()
  lines: string* -> STRING_ITERATOR
  read: string* -> string* -- or numbers

  seek: (number?, number?) -> (number?, string?)

  setvbuf: (string, number?) -> ()
  write: string* -> () -- or number

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
  write: string* -> () -- or number

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
  concat: ({ string }, string?, number?, number?) -> string -- or numbers
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
