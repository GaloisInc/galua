print 'start'

local function f()
        error 'emsg'
end

print(pcall(f))
