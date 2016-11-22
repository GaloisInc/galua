local function demo_stepping()

  local i = 100

  local function update()
    i = i - 1
    i = i - 1
    i = i - 1
  end

  while i > 0 do
    update()
    update()
    update()
  end
end

demo_stepping()

local function demo_c()
  local example_c_module = require 'example_c_module'

  local answer = { example_c_module.bytes('hello') }

  answer = example_c_module.bytes( {} )
end

pcall(demo_c)


local function demo_coroutines()
     local function foo (a)
       print("foo", a)
       return coroutine.yield(2*a)
     end

     local co = coroutine.create(function (a,b)
           print("co-body", a, b)
           local r = foo(a+1)
           print("co-body", r)
           local r, s = coroutine.yield(a+b, a-b)
           print("co-body", r, s)
           return b, "end"
     end)

     print("main", coroutine.resume(co, 1, 10))
     print("main", coroutine.resume(co, "r"))
     print("main", coroutine.resume(co, "x", "y"))
     print("main", coroutine.resume(co, "x", "y"))
end

demo_coroutines()
