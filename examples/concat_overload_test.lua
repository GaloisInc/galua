assert(1 ~= "1") -- sanity check

local mt = {}
function mt.__concat (x,y)
  return type(x) .. "+" .. type(y)
end

local t = {}
setmetatable(t,mt)

print(t .. "" .. 1)
assert(t .. "" .. 1 == "table+string")
assert(t .. 1 .. "" == "table+string")
assert(t .. 1 == "table+number")
assert(1 .. t == "number+table")

print("" .. 1)
assert("" .. 1 == "1")
assert(1 .. "" == "1")

local mt1 = {}
function mt1.__concat (x,y)
  return 42
end

local t1 = {}
setmetatable(t1,mt1)
assert(t1 .. {} == 42)
assert("" .. t1 .. {} == "42")
