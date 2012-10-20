-module(mr_test).
-compile(export_all).

test_init_stop() ->
    {ok, CPid} = mr:start(3),
    mr:stop(CPid).
