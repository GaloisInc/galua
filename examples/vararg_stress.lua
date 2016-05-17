for i = 1,10 do
        local t = {}
        for j = 1,i do
                table.insert(t,j)
        end
        print(table.unpack(t))
end
