%% Example code produced "during" Advanced Programming lecture.  
%% A simple move server
%% Date: Oct 11, 2011
%% Author: Ken Friis Larsen <kflarsen@diku.dk>

-module(move_server).
-export([old_move/2,start/0,move/3]).


%%% API

% ordinary function definition, just here for reference

old_move(north, {X, Y}) -> {X, Y+1};
old_move(west, {X, Y}) -> {X-1, Y}.


start() -> spawn(fun loop/0).

move(Pid, Dir, Pos) -> 
    rpc(Pid, {Dir,Pos}).


%%% Internal implementation

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
	{Pid, Response} -> Response
    end.



loop() ->
    receive
	{From, {north, {X, Y}}} -> 
	    From ! {self(), {X, Y+1}},
	    loop();
	{From, {west, {X, Y}}} -> 
	    From !  {self(), {X-1, Y}},
	    loop();
	{From, Other} ->
	    From ! {self(), {error,Other}},
	    loop()
    end.
