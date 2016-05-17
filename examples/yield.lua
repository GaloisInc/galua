local function check(msg,bool)
        print('checking: '..msg)
        if not bool then
                error('check failed')
        end
end

check('check true',true)
check('check false2',not pcall(check, 'check false1', false))

local function yield5()
        check('isyieldable', coroutine.isyieldable())
        for i=1,5 do
                coroutine.yield(i)
        end
end

check('not isyieldable', not coroutine.isyieldable())

do
        local s = 0
        for k in coroutine.wrap(yield5) do
                s = s + k
        end
        check('sums',s == 15)
end

do
        local s = 0
        for j in coroutine.wrap(yield5) do
                for k in coroutine.wrap(yield5) do
                        s = s + j*k
                end
        end
        check('nested sums',s == 225)
end

do
        local function thread1(co1,co2)
                check('co1 status', coroutine.status(co1)=='running')
                check('co2 status', coroutine.status(co2)=='suspended')
                local current,ismain = coroutine.running()
                check('co1==current', co1==current)
                check('co2/=current', co2~=current)
                check('not ismain', not ismain)
                coroutine.resume(co2,co1,co2)
        end

        local function thread2(co1,co2)
                check('co1 status', coroutine.status(co1)=='normal')
                check('co2 status', coroutine.status(co2)=='running')
                local resumed,msg = coroutine.resume(co1)
                check('not resumed', not resumed)
                check('failure msg', msg == 'cannot resume non-suspended coroutine')
        end

        local co1 = coroutine.create(thread1)
        local co2 = coroutine.create(thread2)

        local current,ismain = coroutine.running()
        check('ismain',ismain)
        check('co1 thread', type(co1)=='thread')
        check('co1 suspended',coroutine.status(co1) == 'suspended')
        check('co2 suspended',coroutine.status(co2) == 'suspended')

        local success = coroutine.resume(co1,co1,co2)
        check('resume success', success)
        check('co1 dead',coroutine.status(co1) == 'dead')
        check('co2 dead',coroutine.status(co2) == 'dead')
end

do
        local co = coroutine.create(error)
        local success,msg = coroutine.resume(co,'aborted')
        check ('not success', not success)
        check ('msg==aborted', msg=='aborted')
end
