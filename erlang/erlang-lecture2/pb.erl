%% Basic concurrent phone-book server callback module
%% 
%% Author: Ken Friis Larsen <kflarsen@diku.dk>
%% Date: October, 2011
-module(pb).
-compile([export_all]).
-import(basicserver, [rpc/2]).

%% Interface
% We assume that the service will be started under the name phonebook
add(Contact)    -> rpc(phonebook, {add, Contact}).
list_all()      -> rpc(phonebook, list_all).
update(Contact) -> rpc(phonebook, {update, Contact}).


%% Callback functions
init() -> dict:new().
handle({add, {Name, _, _} = Contact}, Contacts) ->
    case dict:is_key(Name, Contacts) of 
        false -> {ok, dict:store(Name, Contact, Contacts)};
	true  -> {{error, Name, is_already_there},
                  Contacts}
	% Alternative clause for true, throwing an exception
	%true  -> throw({error, Name, is_already_there})
    end;
handle(list_all, Contacts) ->
    List = dict:to_list(Contacts),
    {{ok, lists:map(fun({_, C}) -> C end, List)},
     Contacts};
handle({update, {Name, _, _} = Contact}, Contacts) ->
    {ok, dict:store(Name, Contact, Contacts)}.
