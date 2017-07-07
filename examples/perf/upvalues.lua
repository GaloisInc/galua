
sum = 0
j = 1000

while (j > 0) do
  if (j % 10 == 0) then io.write('.'); io.flush() end

  i = 10000

  while (i > 0) do
    i = i - 1
    sum = sum + 1
  end

  j = j - 1
end

print (sum)

