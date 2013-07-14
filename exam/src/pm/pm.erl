-module(pm).
-export([ newVanilla/0
        , newPrincess/1
        , get/1
        , put/2
        , compromised/1]).

-define(otherwise, true).


%% Create IVars

% Create new vanilla IVar
% Returns {ok, Pid} where Pid is the process ID of the IVar Process
newVanilla() ->
    {ok, spawn(fun vanilla_loop_empty/0)}.

% Create new princess IVar
% Takes a predicate which decides if values are accepted
% Returns {ok, Pid} where Pid is the process ID of the IVar Process
newPrincess(P) ->
    {ok, spawn(fun () -> princess_loop_empty(P) end)}.


%% Implementation of Vanilla IVar

% Loop that runs while a vanilla IVar is empty
% Eventually calls vanilla_loop/2
vanilla_loop_empty() ->
    receive
        {_, {put, Val}} ->
            vanilla_loop(Val, false);
        {Pid, compromised} ->
            msg(Pid, {compromised, false}),
            vanilla_loop_empty()
    end.

% Loop that runs when vanilla IVar has a value
% Takes the value stored in the IVar and its status (compromised: true or false)
% Runs indefinitely
vanilla_loop(Value, Compromised) ->
    receive
        {Pid, get} ->
            msg(Pid, Value),
            vanilla_loop(Value, Compromised);
        {Pid, compromised} ->
            msg(Pid, {compromised, Compromised}),
            vanilla_loop(Value, Compromised);
        {_, {put, _}} ->
            vanilla_loop(Value, true)
    end.

%% Implementation of Princess IVar 

% Loop that runs while a princess IVar is empty
% Takes a predicate which decides if values are accepted 
% Eventually calls princess_loop/1
princess_loop_empty(P) ->
    receive
        {_, {put, Val}} ->
            Res = try P(Val)
                  catch
                      _ : _ -> false
            end,
            case Res of
                true ->
                    princess_loop(Val);
                _ ->
                    princess_loop_empty(P)
            end;
        {Pid, compromised} ->
            msg(Pid, {compromised, false}),
            princess_loop_empty(P)
    end.

% Loop that runs when princess IVar has a value
% Takes the value stored in the IVar
% Runs indefinitely
princess_loop(Value) ->
    receive
        {Pid, get} ->
            msg(Pid, Value),
            princess_loop(Value);
        {Pid, compromised} ->
            msg(Pid, {compromised, false}),
            princess_loop(Value);
        {_, {put, _}} ->
            princess_loop(Value)
    end.

%% Manipulating IVars

% Gets the value stored in an IVar (both types)
% Takes the Pid of the IVar
% Blocks until IVar is filled (which may never happen)
get(V) ->
    msg(V, get),
    receive
        {V, Val} -> 
            Val
    end.

% Puts a value into an IVar (both types) if the IVar allows it
% Takes the Pid of the IVar and the value that is to be put in
% Returns immediately
put(V, T) ->
    msg(V, {put, T}),
    ok.

% Checks if an IVar (both types) is compromised
% Takes the Pid of the IVar
% Blocks (usually shortly) until it gets a response
compromised(V) ->
    msg(V, compromised),
    receive
        {V, {compromised, Compromised}} ->
            Compromised
    end.

%% Utility functions

% Sends message Msg to process Pid along with the current processe's ID
msg(Pid, Msg) ->
    Pid ! {self(), Msg}.

