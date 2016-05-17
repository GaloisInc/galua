local function body()
        print 'body'
end

local co = coroutine.create(body)
coroutine.resume(co)
