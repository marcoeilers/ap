-include_lib("eunit/include/eunit.hrl").
-module(pm_test).
-export([ tests_all/0,
          test_all/0,
          tests_ivars/0,
          tests_pmmap/0,
          tests_tfa/0 ]).

-define(otherwise, true).

%% Returns all tests
tests_all() ->
    [tests_ivars(), tests_pmmap(), tests_tfa()].

%% Runs all tests
test_all() ->
    lists:map(fun eunit:test/1, tests_all()).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Unit tests for IVars                                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Create IVars
create_vanilla() ->
    Res = pm:newVanilla(),
    ?assertMatch({ok, _}, Res),
    {ok, Pid} = Res,
    ?assert(is_pid(Pid) and is_process_alive(Pid)).

create_princess() ->
    Res = pm:newPrincess(fun always_true/1),
    ?assertMatch({ok, _}, Res),
    {ok, Pid} = Res,
    ?assert(is_pid(Pid) and is_process_alive(Pid)).

%% Put should return no matter what
put_vanilla() ->
    {ok, Pid} = pm:newVanilla(),
    pm:put(Pid, 4),
    pm:put(Pid, ok),
    pm:put(Pid, "test").

put_princess() ->
    {ok, Pid} = pm:newPrincess(fun always_fails/1),
    pm:put(Pid, 4),
    pm:put(Pid, ok),
    pm:put(Pid, "test").

%% Get should retrieve a value
get_vanilla() ->
    {ok, Pid} = pm:newVanilla(),
    pm:put(Pid, 5),
    Res = pm:get(Pid),
    ?assert(Res =:= 5).

get_princess() ->
    {ok, Pid} = pm:newPrincess(fun always_true/1),
    pm:put(Pid, 5),
    Res = pm:get(Pid),
    ?assert(Res =:= 5).

%% Get should wait for a value if there is none
get_wait_vanilla() ->
    {ok, Pid} = pm:newVanilla(),
    spawn(fun () -> timer:sleep(1000), pm:put(Pid, ok) end),
    Res = pm:get(Pid),
    ?assert(Res =:= ok).

get_wait_princess() ->
    {ok, Pid} = pm:newPrincess(fun always_true/1),
    spawn(fun () -> timer:sleep(1000), pm:put(Pid, ok) end),
    Res = pm:get(Pid),
    ?assert(Res =:= ok).

%% Get should retrieve the right value
%% Test compromise along the way
get_put_vanilla() ->
    {ok, Pid} = pm:newVanilla(),
    ?assertNot(pm:compromised(Pid)),
    pm:put(Pid, 13),
    Res1 = pm:get(Pid),
    ?assert(Res1 =:= 13),
    ?assertNot(pm:compromised(Pid)),
    pm:put(Pid, 7),
    pm:put(Pid, ok),
    pm:put(Pid, "test"),
    Res2 = pm:get(Pid),
    ?assert(Res2 =:= 13),
    ?assert(pm:compromised(Pid)).

get_put_princess() ->
    {ok, Pid} = pm:newPrincess(fun is_five/1),
    ?assertNot(pm:compromised(Pid)),
    pm:put(Pid, 13),
    pm:put(Pid, "test"),
    pm:put(Pid, 5),
    Res1 = pm:get(Pid),
    ?assert(Res1 =:= 5),
    ?assertNot(pm:compromised(Pid)),
    pm:put(Pid, ok),
    pm:put(Pid, -3),
    Res2 = pm:get(Pid),
    ?assert(Res2 =:= 5),
    ?assertNot(pm:compromised(Pid)).

get_put_princess2() ->
    {ok, Pid} = pm:newPrincess(fun less_than_seven/1),
    ?assertNot(pm:compromised(Pid)),
    pm:put(Pid, 9),
    pm:put(Pid, 3),
    Res1 = pm:get(Pid),
    ?assert(Res1 =:= 3),
    ?assertNot(pm:compromised(Pid)),
    pm:put(Pid, 1),
    pm:put(Pid, 4),
    Res2 = pm:get(Pid),
    ?assert(Res2 =:= 3),
    ?assertNot(pm:compromised(Pid)).

%% Predicates with non-boolean return values should be false
non_bool_princess() ->
    {ok, Pid} = pm:newPrincess(fun returns_int/1),
    pm:put(Pid, ok),
    pm:put(Pid, 12),
    pm:put(Pid, true),
    pm:put(Pid, 13),
    Res1 = pm:get(Pid),
    ?assert(Res1 =:= 13),
    pm:put(Pid, 3),
    Res2 = pm:get(Pid),
    ?assert(Res2 =:= 13).
    

%% Predicates that throw errors should be false
error_princess() ->
    {ok, Pid} = pm:newPrincess(fun always_fails_except/1),
    pm:put(Pid, ok),
    pm:put(Pid, 3),
    pm:put(Pid, 13),
    Res = pm:get(Pid),
    ?assert(Res=:=13).

% All tests concerning the IVars
tests_ivars() ->
    [fun create_vanilla/0,
     fun create_princess/0,
     fun put_vanilla/0,
     fun put_princess/0,
     fun get_vanilla/0,
     fun get_princess/0,
     fun get_wait_vanilla/0,
     fun get_wait_princess/0,
     fun get_put_vanilla/0,
     fun get_put_princess/0,
     fun get_put_princess2/0,
     fun non_bool_princess/0,
     fun error_princess/0 ].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Unit tests for pmmap                                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% An empty list as input should result in an empty list
pmmap_empty() ->
    Data = [],
    Res1 = pmuse:pmmap(fun id/1, Data),
    ?assert([] =:= Res1).

% Mapping the identity function should return the input
pmmap_id() ->
    Data = lists:seq(1,30),
    Res1 = pmuse:pmmap(fun id/1, Data),
    ?assert(Data =:= Res1).

% pmmap should return the same result as lists:map/2
% Tested with function fac/1
pmmap_fac() ->
    Data = lists:seq(4,50),
    Res1 = pmuse:pmmap(fun fac/1, Data),
    Res2 = lists:map(fun fac/1, Data),
    ?assert(Res1 =:= Res2).

% Tested with other function
pmmap_other() ->
    Data = lists:seq(5,170),
    F = fun (X) -> X > 41 end,
    Res1 = pmuse:pmmap(F, Data),
    Res2 = lists:map(F, Data),
    ?assert(Res1 =:= Res2).

% All tests concerning pmmap
tests_pmmap() ->
    [fun pmmap_empty/0,
     fun pmmap_id/0,
     fun pmmap_fac/0,
     fun pmmap_other/0 ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Unit tests for treeforall                                       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% tfa applied to a leaf should always be true
tfa_leaf() ->
    Res = pmuse:treeforall(leaf, fun always_false/1),
    ?assert(Res).

% tfa should work on a single node, checked with a node
% satisfying the predicate
tfa_one_true() ->
    Tree = {node, 2, leaf, leaf},
    ?assert(pmuse:treeforall(Tree, fun (X) -> X =:= 2 end)).

% tfa should work on a single node, checked with a node
% not satisfying the predicate
tfa_one_false() ->
    Tree = {node, 4, leaf, leaf},
    ?assertNot(pmuse:treeforall(Tree, fun (X) -> X =:= 3 end)).

% Test with a tree satisfying the predicate
tfa_true() ->
    Tree = tree(30),
    ?assert(pmuse:treeforall(Tree, fun (X) -> X < 40 end)).

% Test with a tree not completely satisfying the predicate
tfa_false() ->
    Tree = tree(40),
    ?assertNot(pmuse:treeforall(Tree, fun (X) -> X < 40 end)).

% Test with a tree not at all satifying the predicate
tfa_false2() ->
    Tree = tree(40),
    ?assertNot(pmuse:treeforall(Tree, fun (X) -> X < 5 end)).

% Make sure non-boolean output of the predicate is treated like false
tfa_non_bool() ->
    Tree = tree(25),
    ?assertNot(pmuse:treeforall(Tree, fun returns_int/1)).

% Make sure errors thrown by the predicate are treated like false
tfa_error() ->
    Tree = tree(25),
    ?assertNot(pmuse:treeforall(Tree, fun always_fails/1)).

% All tests concerning treeforall
tests_tfa() ->
    [fun tfa_leaf/0,
     fun tfa_one_true/0,
     fun tfa_one_false/0,
     fun tfa_true/0,
     fun tfa_false/0,
     fun tfa_false2/0,
     fun tfa_non_bool/0,
     fun tfa_error/0].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Utility functions                                               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tree(N) ->
   tree_test:tree_of(lists:seq(1,N)).

fac(T) ->
    if T=:=0 ->
        1;
    ?otherwise ->
        T*fac(T-1)
    end.

always_true(_) ->
    true.

always_false(_) ->
    false.

always_fails(X) ->
    X/0.

always_fails_except(X) ->
    if X =:= 13 ->
        true;
    ?otherwise ->
        X/0
    end.

is_five(X) ->
    X =:= 5.

returns_int(X) -> 
    if X =:= 13 ->
        true;
    ?otherwise ->
        16
    end.

less_than_seven(X) ->
    X < 7.

id(X) -> X.

