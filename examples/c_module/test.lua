example_c_module = require 'example_c_module'

local s = example_c_module.store(10);
print(s())

local a,b=0.0,1.0

for i=1,10 do
        print(a)
        a,b=example_c_module.fibstep(a,b)
end

print('stringtest' , example_c_module.stringtest("demo"))

do return end
local function f(x)
        print('f',x)
        local y = coroutine.yield(20,30)
        print('y',y)
        return 50
end

function mythread()
print('prelua')
local r = example_c_module.call42(f)
print('postlua', r)
end

local t = coroutine.create(mythread)
print('finish1', coroutine.resume(t))
print('finish2', coroutine.resume(t))

