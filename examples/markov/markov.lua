-- Markov Chain Program in Lua

-- generator for all of the words in a file
local function filewords(file)

  local function filewords_worker()
    for line in file:lines() do
      for word in line:gmatch('%w+') do
        coroutine.yield(word)
      end
    end
  end

  return coroutine.wrap(filewords_worker)
end

local function prefix (w1, w2)
  return w1 .. ' ' .. w2
end

local function insert(tab, ix, val)
  local list = tab[ix]
  if not list then
    list    = {}
    tab[ix] = list
  end
  table.insert(list, val)
  return list
end

-- Select a random element from a sequence
local function choose(tab)
  local n = #tab
  if n == 0 then error('choose called with empty table', 2) end
  return tab[math.random(n)]
end

local MAXGEN = 10000
local NOWORD = ''

io.write('Write input to stdin terminating with EOF\n')

-- build table
local statetab = {}

local w1, w2 = NOWORD, NOWORD
for w in filewords(io.stdin) do
  insert(statetab, prefix(w1, w2), w)
  w1, w2 = w2, w
end
insert(statetab, prefix(w1, w2), NOWORD)

-- generate text
w1, w2 = NOWORD, NOWORD     -- reinitialize
for i = 1, MAXGEN do
  local ix   = prefix(w1, w2)
  local list = statetab[ix]
  local w    = choose(list)

  if w == NOWORD then break end

  print(w)
  w1, w2 = w2, w
end

local function halfinteger(x)
        if (x % 2 == 1) then
                error('expected an even number')
        end
        return x / 2
end

print(pcall(halfinteger, 13))

print 'END OF DEMO'
