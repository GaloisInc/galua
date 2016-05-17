local prop = require 'prop'

_TCENV = { string = {}, table = {}, math = {} }

function _TCENV.string.byte(s,i,j)
        prop.stringcheck(s)
        assert(tonumber(i))
        assert(tonumber(j))

        return M.numbers()
end

function _TCENV.string.char(...)
        prop.isstrings(...)
        return prop.string()
end

function _TCENV.string.format(format)
        prop.stringcheck(format)
        return prop.string()
end

local function gmatch_helper()
        if prop.boolean() then
                return
        else
                return prop.string()
        end
end

function _TCENV.string.gmatch(s, pattern)
        stringcheck(s)
        stringcheck(pattern)
        return gmatch_helper, prop.unknown(), prop.unknown()
end

function _TCENV.string.gsub(s, pattern, repl, n)

        local repl_type = type(repl)

        if repl_type == 'function' then
                for i=1,10 do
                        repl(prop.string())
                end
        elseif repl_type == 'string' then
        elseif repl_type == 'table' then
                for i=1,10 do
                        local _ = repl[prop.string()]
                end
        else
                error('type error')
        end

        return prop.string(), prop.number()
end

function _TCENV.string.find(s, pattern, init, plain)

        assert(type(s) == 'string')
        assert(type(pattern) == 'string')
        assert(not init or tonumber(init))
        -- plain is unconstrained

        if prop.boolean() then
                return prop.number(), prop.number()
        else
                return nil
        end
end

function _TCENV.table.insert(list, arg1, arg2)

        local pos, value

        assert(type(list) == 'table')

        if arg2 then
                pos = arg1
                assert(tonumber(pos))
                value = arg2
        else
                pos = M.number()
                value = arg1
        end


        list[pos] = value
end

function _TCENV.table.concat(list, sep, i, j)

        -- list can be anything that supports __index and __len
        sep = sep or ''
        i   = i or 1
        j   = j or #list

        prop.stringcheck(sep)

        for i = i,j do
                prop.stringcheck(list[i])
        end

        return M.string()
end

function _TCENV.table.move(a1, f, e, t, a2)

        a2 = a2 or a1

        for i = f,e do
                a2[t+i] = a1[i]
        end
end

function _TCENV.table.pack(...)
        local t = {...}
        t.n = #t
        return t
end

function _TCENV.table.remove(list, pos)
        local last = #list
        pos = pos or last
        assert(tonumber(pos))

        list[pos] = nil
        for i = pos, last-1 do
                list[i] = list[i+1]
        end
end

function _TCENV.table.unpack(list, i, j)
        i = i or 1
        j = j or #list

        if i <= j then
                return list[i], _TCENV.table.unpack(list, i+1, j)
        else
                return
        end
end

function _TCENV.table.sort(list, comp)

        local kettle

        for i = 1,#list do
                kettle = list[i]
        end

        if comp then
            comp(kettle, kettle)
        end

        for i = 1,#list do
                list[i] = kettle
        end

end

local function simple_math1(n)
        assert(tonumber(n))
        return prop.number()
end

local function simple_math2(m,n)
        assert(tonumber(m))
        assert(tonumber(n))
        return prop.number()
end

local function simple_math12(m,n)
        assert(tonumber(m))
        assert(not n or tonumber(n))
        return prop.number()
end

_TCENV.math.abs  = simple_math1
_TCENV.math.acos = simple_math1
_TCENV.math.asin = simple_math1

_TCENV.math.atan = undefined -- x [, y]

_TCENV.math.ceil = simple_math1
_TCENV.math.cos = simple_math1
_TCENV.math.deg = simple_math1
_TCENV.math.exp = simple_math1
_TCENV.math.floor = simple_math1

_TCENV.math.fmod = undefined -- x, y

_TCENV.math.huge = 1.0/0
_TCENV.math.log = undefined -- x [, base]

function _TCENV.math.max(x,...)
        assert(tonumber(x))
        for _,v in ipairs{...} do
           assert(tonumber(v))
        end
        return prop.number()
end

_TCENV.math.maxinteger = prop.number()

_TCENV.math.min = _TCENV.math.max
_TCENV.math.mininteger = prop.number()
_TCENV.math.modf = simple_math1
_TCENV.math.pi = prop.number()
_TCENV.math.rad = simple_math1

_TCENV.math.random = undefined -- [m [, n]]
_TCENV.math.randomseed = simple_math1
_TCENV.math.sin = simple_math1
_TCENV.math.sqrt = simple_math1

function _TCENV.math.type(...)

        local args = {...}

        if #args == 0 then
                error('expected number')
        else if args[1] == 'number' then
                return prop.string()
        else
                return nil
        end
end

_TCENV.math.ult = undefined -- m, n
