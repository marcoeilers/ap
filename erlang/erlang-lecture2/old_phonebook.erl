%%
%% Basic concurrent phone-book
%%

-module(phonebook).
-export([start/0, add/2, list_all/1, update/2]).

%% Interface

start() -> 
    spawn(fun() -> loop(dict:new()) end).

add(Pid, Contact) -> rpc(Pid,{add, Contact}).

list_all(Pid) -> rpc(Pid, list_all).

update(Pid, Contact) -> rpc(Pid, {update, Contact}).

%% Internal implementation

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
	{Pid, Response} ->
	    Response
    end.

loop(Contacts) ->
    receive
        {From, {add, Contact}} ->
            {Name, _, _} = Contact,
            case dict:is_key(Name, Contacts) of
                true ->
                    From ! {self(), {error, Name, is_in_dict}},
                    loop(Contacts);
                false -> 
                    From ! {self(), {Name, is_added}},
                    loop(dict:store(Name, Contact, Contacts))
            end;
        {From, list_all} ->
            List = dict:to_list(Contacts),
            From ! {self(), {ok, lists:map(fun({_, C}) -> C end, List)}},
            loop(Contacts);
        {From, Other} ->
            From ! {self(), {error,Other}},
            loop(Contacts)

    end.

