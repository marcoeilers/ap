-module(spawnfun).
-compile(export_all).

spawn10() -> 
    G = fun(X) -> timer:sleep(10),
		  io:format("~p~n", [X])
	end,
    [spawn(fun() -> G(X) end) || X <- lists:seq(1,10)].
    
spawnN(N) when N > 0 ->
    G = fun(X) -> timer:sleep(10),
		  io:format("~p~n", [X])
	end,
    [spawn(fun() -> G(X) end) || X <- lists:seq(1,N)];
spawnN(N) -> io:format("Number must be greater than zero (Got ~p).~n", [N]),
	     error.
