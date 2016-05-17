local M = {}

function M.number()
        return 0
end

function M.boolean()
        return M.number() > 0
end

function M.string()
        if M.boolean() then
                return 'this'
        else
                return 'that'
        end
end

function M.stringcheck(x)
        assert(type(x) == 'string' or type(x) == 'number')
end

function M.numbers()
        if M.boolean() then
                return
        else
                return M.number(), M.numbers()
        end
end

return M
