print 'start'

local function recursive(x)
        print ('recursive',x)
    if x < 3 then
       recursive(x+1)
    end
end

recursive(0)
print 'done'
