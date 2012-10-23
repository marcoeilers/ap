-module(mr_test).
-compile(export_all).

test_init_stop() ->
    ok = mr:start(3, mrc),
    mr:stop(mrc).

test_sum() ->
    ok = mr:start(3, mrcord),
    {ok,Sum} = mr:job(mrcord,
		      fun(X) -> X end,
		      fun(X, Acc) -> X+Acc end,
		      0,
		      lists:seq(1,10)),
    mr:stop(mrcord),
    Sum.

test_fac() ->
    ok = mr:start(3, mrc),
    {ok,Fac} = mr:job(mrc,
		      fun(X) -> X end,
		      fun(X,Acc) -> X*Acc end,
		      1,
		      lists:seq(1,10)),
    mr:stop(mrc),
    Fac.

test_fac_sum() ->
    ok = mr:start(3, mrc),
    {ok,Sum} = mr:job(mrc,
		      fun(X) -> X end,
		      fun(X, Acc) -> X+Acc end,
		      0,
		      lists:seq(1,10)),
    {ok,Fac} = mr:job(mrc,
		      fun(X) -> X end,
		      fun(X,Acc) -> X*Acc end,
		      1,
		      lists:seq(1,10)),
    mr:stop(mrc),
    {Sum,Fac}.

test_word_count() ->
    ok = mr:start(3, mrc),
    {ok,Dict} = mr:job(mrc,
		       fun(X) -> {X, 1} end,
		       fun({K, V}, Dict) ->
			       dict:update_counter(K, V, Dict)
		       end,
		       dict:new(),
		       ["Hello", "World", "Goodbye", "World"]),
    mr:stop(mrc),
    dict:to_list(Dict).
