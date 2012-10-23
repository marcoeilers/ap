-module(mr_test).
-compile(export_all).

test_init_stop() ->
    {ok, CPid} = mr:start(3),
    mr:stop(CPid).

test_sum() ->
    mr:start(3, mrcord),
    {ok,Sum} = mr:job(mrcord,
		      fun(X) -> X end,
		      fun(X, Acc) -> X+Acc end,
		      0,
		      lists:seq(1,10)),
    mr:stop(mrcord),
    Sum.

test_fac() ->
    {ok,MR} = mr:start(3),
    {ok,Fac} = mr:job(MR,
		      fun(X) -> X end,
		      fun(X,Acc) -> X*Acc end,
		      1,
		      lists:seq(1,10)),
    mr:stop(MR),
    Fac.

test_fac_sum() ->
    {ok,MR} = mr:start(3),
    {ok,Sum} = mr:job(MR,
		      fun(X) -> X end,
		      fun(X, Acc) -> X+Acc end,
		      0,
		      lists:seq(1,10)),
    {ok,Fac} = mr:job(MR,
		      fun(X) -> X end,
		      fun(X,Acc) -> X*Acc end,
		      1,
		      lists:seq(1,10)),
    mr:stop(MR),
    {Sum,Fac}.

test_word_count() ->
    {ok,MR} = mr:start(3),
    {ok,Dict} = mr:job(MR,
		       fun(X) -> {X, 1} end,
		       fun({K, V}, Dict) ->
			       dict:update_counter(K, V, Dict)
		       end,
		       dict:new(),
		       ["Hello", "World", "Goodbye", "World"]),
    mr:stop(MR),
    dict:to_list(Dict).
