
function f()
  print(1)
  error(0)
end

function h (e)
  print(e)
  if (e < 5) then error(e+1) end
  return "I"
end

local a = 0

while true do
  a = a + 1
end

print (xpcall(f,h))

