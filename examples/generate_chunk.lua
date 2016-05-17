local function f(x,y)
        return x+y
end

local outputFile = io.open("out.luac","w")
outputFile:write(string.dump(f))
outputFile:close()
