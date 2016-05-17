print 'remove 2'
for i = 1,5 do
        print('-------',i)
        local mytable = {10,20,30,40}
        print(table.remove(mytable,i))
        for k,v in ipairs(mytable) do
                print (k,v)
        end
end

print 'remove 1'
do
        local mytable = {10,20,30,40}
        local x
        repeat
                x = table.remove(mytable)
                print(x)
        until(not x)
end

print 'sort'
do
        local mytable = {59,60,73,11,8,74,3,65,60,23}
        table.sort(mytable)
        print(table.concat(mytable,','))
end
