-module(kitchen).
-compile(export_all).

start() ->
    spawn(?MODULE, fridge, []).

start(Foodlist) ->
    spawn(?MODULE, fridge, [Foodlist]).

take(Pid, Food) ->
    rpc(Pid, {take, Food}).

store(Pid, Food) ->
    rpc(Pid, {store, Food}).

inspect(Pid) ->
    rpc(Pid, inspect).

long_request(Pid) ->
    rpc(Pid, long_request).

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
	{Pid, Response} -> Response
    after 3000 ->
	    %% Will not wait longer than 3 seconds for a response
	    %% Try 'kitchen:take(pid(0,250,0), something).'
	    timeout
    end.
    

fridge(Foodlist) ->
    receive
	{From, {store, Food}} ->
	    From ! {self(), ok},
	    fridge([Food|Foodlist]);
	{From, inspect} ->
	    From ! {self(), {ok, Foodlist}},
	    fridge(Foodlist);
	{From, long_request} ->
	    timer:sleep(3001),
	    From ! {self(), ok},
	    fridge(Foodlist);
	{From, {take, Food}} ->
	    case lists:member(Food, Foodlist) of
		true ->
		    From ! {self(), {ok, Food}},
		    fridge(lists:delete(Food, Foodlist));
		false ->
		    From ! {self(), not_found},
		    fridge(Foodlist)
	    end
    end.
			     
