%% Simple generic server library
%% 
%% Author: Ken Friis Larsen <kflarsen@diku.dk>


-module(basicserver).
-export([start/2, rpc/2]).

start(Name, Mod) ->
    register(Name, spawn(fun() -> loop(Name, Mod, Mod:init()) end)).

rpc(Name, Request) ->
    Name ! {self(), Request},
    receive
	{Name, Reply} -> Reply
    end.

loop(Name, Mod, State) ->
    receive
        {From, Request} ->
            {Reply, State1} = Mod:handle(Request, State),
            From ! {Name, Reply},
            loop(Name, Mod, State1)
    end.
