print 'start'
local co = coroutine.create(error)
print(coroutine.resume(co,'failure'))
