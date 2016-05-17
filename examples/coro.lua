print 'starting'

function partialsums(x)
        while true do
                local y = coroutine.yield(x)
                x = x + y
                if x > 50 then error "too big" end
        end
end

local function sumTo(n)
        local add = coroutine.wrap(partialsums)
        for i = 1,n do
                print(add(i))
        end
end

do -- scope for 'n'
        local n = 1
        repeat
                local success = pcall(sumTo,n)
                n = n + 1
        until not success
end
