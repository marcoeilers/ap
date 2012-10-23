%% Simple generic server library with transaction functionality
%% 
%% Author: Ken Friis Larsen <kflarsen@diku.dk>
%% Date: October, 2011
-module(trans_server).
-export([start/2, rpc/2]).

start(Name, Mod) ->
    register(Name, spawn(fun() -> loop(Name, Mod, Mod:init()) end)).

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        {Pid, {abort, Why}} -> exit({rpc_abort, Why});
	{Pid, Reply} -> Reply
    end.

loop(Name, Mod, State) ->
    receive
        {From, Request} ->
            try Mod:handle(Request, State) of
                {Reply, State1} ->
                    From ! {Name, Reply},
                    loop(Name, Mod, State1)
            catch
                _ : Why ->
                    From ! {Name, {abort, Why}},
                    loop(Name, Mod, State)
            end
    end.
