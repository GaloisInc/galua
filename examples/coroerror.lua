print 'starting'
local co = coroutine.create(error)
print('returned', coroutine.resume(co,'errormessage'))
