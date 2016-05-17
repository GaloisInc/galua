
function package.preload.my_module()
  local mod = {}
  function mod.my_operation()
    return 42
  end
  return mod
end

m = require 'my_module'

print (m.my_operation())
