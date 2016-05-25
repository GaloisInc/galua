-- Given a function 'f' and arguments 'args...' suspend returns a function
-- that calls f(args...) when it is called.
local function suspend(f, ...)
        local args = {...}
        return function()
                return f(table.unpack(args))
        end
end

local function compose_generators(...)

    local function worker(s, g, ...)
            if g then
                    for t in g(s) do
                            worker(t, ...)
                    end
            else
                    coroutine.yield(s)
            end
    end

    return coroutine.wrap(suspend(worker, ...))
end


local lines_gen = function (file) return file:lines()       end
local words_gen = function (line) return line:gmatch('%w+') end

for word in compose_generators(io.stdin, lines_gen, words_gen) do
        print(word)
end
