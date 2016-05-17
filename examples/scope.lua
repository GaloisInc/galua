goto mylabel

do
  local y = 3

  function g() return x+y end

  ::mylabel::

  print 'test'
end
