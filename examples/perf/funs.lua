local function f(i,sum)
  return i-1, sum + 1
end


local sum = 0
local j = 1000

while (j > 0) do

  if (j % 10 == 0) then io.write('.'); io.flush() end
  local i = 10000


  while (i > 0) do
    i,sum = f(i,sum)
  end

  j = j - 1
end

print (sum)

