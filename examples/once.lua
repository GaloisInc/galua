function once(f)
        local first, result = true

        return function(...)
                if first then
                        first = false
                        result = f(...)
                end

                return result
        end
end

local function prompt(str)
        io.write(str)
        return io.read()
end

local f = once(prompt)

for i = 1,3 do
        print(f('Type something (' .. i .. ')\n'))
end
