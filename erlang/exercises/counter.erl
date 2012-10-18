-module(counter).
-export([start/0,start/1,read/1,inc/1,dec/1]).

%% Interface
start() ->
    spawn(fun() -> counter(0) end).

start(N) ->
    spawn(fun() -> counter(N) end).

read(Pid) ->
    rpc(Pid, read).

inc(Pid) ->
    rpc(Pid, inc).

dec(Pid) ->
    rpc(Pid, dec).


%% Internal implementation
rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
	{Pid, Response} ->
	    Response
    end.

counter(N) ->
    receive
	{From, read} ->
	    From ! {self(), N},
	    counter(N);
	{From, inc} ->
	    From ! {self(), ok},
	    counter(N + 1);
	{From, dec} ->
	    From ! {self(), ok},
	    counter(N - 1);
	{From, Other} ->
	    From ! {self(), {error, unknown_request, Other}},
	    counter(N)
    end.
