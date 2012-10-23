%% Concurrent phone-book server with simple supervisor functionality
%% 
%% Author: Ken Friis Larsen <kflarsen@diku.dk>
%% Date: October, 2011
-module(keep_alive).
-export([start/0, add/2, list_all/1, update/2]).

%% Interface

start() -> keep_looping().

add(Pid, Contact) ->
    rpc(Pid, {add, Contact}).

list_all(Pid) ->
    rpc(Pid, list_all).

update(Pid, Contact) ->
    rpc(Pid, {update, Contact}).


%% Internal implementation

rpc(Pid, Request) ->
    Ref = make_ref(),
    Pid ! {self(), Ref, Request},
    receive
	{Ref, Response} -> Response
    end.

keep_looping() ->
    spawn(fun () ->
            process_flag(trap_exit, true),
            State = dict:new(),
            Super = self(),
            Pid = spawn_link(fun() -> loop(Super, State) end),
            supervisor(Pid,State,nothing)
          end).

supervisor(Pid, State, LastMsg) ->
    receive
        {'EXIT', Pid, Reason} ->
            io:format("~p exited because of ~p~n", [Pid, Reason]),
            Super = self(),	   
            Pid1 = spawn_link(fun() -> loop(Super, State) end),
	        case LastMsg of
		    {From, Ref, _Request} ->
		        From ! {Ref, {fancy_message, Reason}};
		    _ -> anything
	        end,
            supervisor(Pid1, State, nothing);
        {Pid, backup, State1} ->
             supervisor(Pid, State1, LastMsg);
        Msg -> 
	        Pid ! Msg,
	        supervisor(Pid, State, Msg)
    end.

loop(Super, Contacts) ->
    Super ! {self(), backup, Contacts},
    receive
        {From, Ref, {add, {Name,_,_} = Contact}} ->
            case dict:is_key(Name, Contacts) of 
                false ->
                    From ! {Ref, ok},
                    loop(Super, dict:store(Name, Contact, Contacts))
            end;
        {From, Ref, list_all} ->
            List = dict:to_list(Contacts),
            From ! {Ref, {ok, lists:map(fun({_, C}) -> C end, List)}},
            loop(Super, Contacts);
        {From, Ref, {update, {Name,_,_} = Contact}} ->
            From ! {Ref, ok},
            loop(Super, dict:store(Name, Contact, Contacts))
    end.
