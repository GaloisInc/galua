local example_c_module = require'example_c_module'

local a,b=0.0,1.0

for i=1,10 do
        print(a)
        a,b=example_c_module.fibstep(a,b)
end

a,b = example_c_module.fibstep() -- no numbers, should be an error!
print(a,b)
