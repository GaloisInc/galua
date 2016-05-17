print 'starting'

local function body(arg1,arg2)
        print ('body arg',arg1,arg2)
        local y,z = coroutine.yield(10,11)
        print ('yield produced',y,z)
        return 20,21
end

local co = coroutine.create(body)

print('returned', coroutine.resume(co,42,43))
print('returned', coroutine.resume(co,38,39))
