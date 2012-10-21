%%%-------------------------------------------------------------------
%%% @author Ken Friis Larsen <kflarsen@diku.dk>
%%% @copyright (C) 2011, Ken Friis Larsen
%%% Created : Oct 2011 by Ken Friis Larsen <kflarsen@diku.dk>
%%%-------------------------------------------------------------------
-module(mr).
-export([start/1, stop/1, job/5, status/1,test_sum/0,test_fac/0]).
-compile(debug_all).

-define(else, true). % Nice little hack.
%%%% Interface

start(N) ->
    {Reducer, Mappers} = init(N),
    {ok, spawn(fun() -> coordinator_loop(Reducer, Mappers) end)}.

status(CPid) ->
    rpc(CPid, status).

stop(CPid) ->
    rpc(CPid, stop).


%%
%% CPid: Pid of coordinator process
%% MapFun: Mapping function (:: a -> b)
%% RedFun: Reducing function (:: a -> res -> res)
%% Initial: Initial value for the coordinator
%% Data: The data that should be processed

job(CPid, MapFun, RedFun, RedInit, Data) -> 
    rpc(CPid, {job, MapFun, RedFun, RedInit, Data}).

%%%% Internal implementation


init(N) ->
    Red = spawn(fun reducer_loop/0),
    {Red, [spawn(fun() -> mapper_loop(Red, id()) end) || _ <- lists:seq(1,N) ]}.

%% Function that generates the id function.
id() ->
    fun(X) -> X end.


%% synchronous communication

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
	{Pid, Response} ->
	    Response
    end.

reply(From, Msg) ->
    From ! {self(), Msg}.

reply_ok(From) ->
    reply(From, ok).

reply_ok(From, Msg) ->
    reply(From, {ok, Msg}).


%% asynchronous communication

info(Pid, Msg) ->
    Pid ! Msg.

stop_async(Pid) ->
    info(Pid, stop).

data_async(Pid, D) ->
    info(Pid, {data, D}).

setup_async(Pid, Fun) ->
    info(Pid, {setup, Fun}).


%%% Coordinator

coordinator_loop(Reducer, Mappers) ->
    receive
	{From, stop} ->
	    io:format("~p stopping~n", [self()]),
	    lists:foreach(fun stop_async/1, Mappers),
	    stop_async(Reducer),
	    reply_ok(From);
	{From, status} ->
	    io:format("This is MR coordinator ~p managing ~B mappers.~n", [self(), length(Mappers)]),
	    reply_ok(From),
	    coordinator_loop(Reducer, Mappers);
	{From, {job, MapFun, RedFun, RedInit, Data}} ->
	    %% Uh, update mappers and reducers, split data and start processing
	    lists:foreach(fun(M) -> setup_async(M, MapFun) end, Mappers),

	    send_data(Mappers, Data),

	    %% Wait for the reducer to return something
	    {ok, Result} = rpc(Reducer, {start, {RedFun, RedInit, lists:seq(1,length(Data)-1)}}),
	    
	    reply_ok(From, Result),

	    coordinator_loop(Reducer, Mappers)
    end.

send_data(Mappers, Data) ->
    send_loop(Mappers, Mappers, Data).

send_loop(Mappers, [Mid|Queue], [D|Data]) ->
    data_async(Mid, D),
    send_loop(Mappers, Queue, Data);
send_loop(_, _, []) -> ok;
send_loop(Mappers, [], Data) ->
    send_loop(Mappers, Mappers, Data).


%%% Reducer

%% The idle reducer state
reducer_loop() ->
    receive
	stop -> 
	    io:format("Reducer ~p stopping~n", [self()]),
	    ok;
	{From, {start, {Fun, Acc, Missing}}} ->
	    io:format("Reducer received start signal~n"),
	    reply_ok(From, gather_data_from_mappers(Fun, Acc, Missing)),
	    reducer_loop()
    end.

%% Active reducer.
gather_data_from_mappers(Fun, Acc, Missing) ->
    receive
	{data, Data} ->
	    io:format("Reducer received data <~p>~n", [Data]),
	    NAcc = Fun(Data, Acc),
	    %% TODO Figure out something smarter
	    case Missing of
		[_|T] ->
		    io:format("More data to come; state is now: Acc: ~p, Missing: ~p~n", [Acc, Missing]),
		    gather_data_from_mappers(Fun, NAcc, T);
		[] ->
		    io:format("Have we reached the end here?~n"),
		    NAcc
	    end
    after 5000 ->
	    io:format("Timed out - state is now: Acc: ~p, Missing: ~p~n", [Acc, Missing]),
	    Acc
    end.




%%% Mapper

mapper_loop(Reducer, Fun) ->
    receive
	stop -> 
	    io:format("Mapper ~p stopping~n", [self()]),
	    ok;
	{data, D} ->
	    io:format("Mapper ~p received data <~p>~n", [self(), D]),
	    data_async(Reducer, Fun(D)),
	    mapper_loop(Reducer, Fun);
	{setup, NewFun} ->
	    io:format("Mapper ~p received new mapper function~n", [self()]),
	    mapper_loop(Reducer, NewFun);
	Unknown ->
	    io:format("unknown message: ~p~n",[Unknown]), 
	    mapper_loop(Reducer, Fun)
    end.


%%% Test section
test_sum() ->
    {ok,MR} = mr:start(3),
    {ok,Sum} = mr:job(MR,
		      fun(X) -> X end,
		      fun(X, Acc) -> X+Acc end,
		      0,
		      lists:seq(1,10)),
    mr:stop(MR),
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
