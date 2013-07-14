-module(pmuse).
-export([ pmmap/2
        , treeforall/2 ]).

-define(otherwise, true).

%% pmmap: Input-output-behaviour like lists:map/2 for side-effect-free functions
%% Internally uses IVars for communication and executes function in parallel
%% (if the system allows it).
%% Takes a function and a list of values
%% Returns: the result from applying the function to all values in the list
pmmap(F, L) ->
    IVars = lists:map(fun(T) -> {ok, Pid} = pm:newVanilla(),
                                spawn(fun () -> pm:put(Pid, F(T)) end),
                                Pid
                      end, L),
    Res = lists:map(fun (IV) -> Res = pm:get(IV),
                          Res 
              end, IVars),
    Res.

%% treeforall: Checks if, for a given tree, a predicate holds for all nodes.
%% Returns immediately when the result is determined, though
%% a background process may continue do so dome calculations after that.
%% Takes a tree and a predicate.
%% Returns true if all nodes in the tree satisfy the predicate, otherwise false.
%% If the predicate returns anything other than true (including errors),
%% this is taken to mean false. 
treeforall(T, P) ->
    {ok, IVar} = pm:newPrincess(fun (X) -> case X of
                                               {Res, Elem} ->
                                                   Res or not(robustEval(P, Elem)) 
                                           end
                                end),
    spawn(fun () -> startTraversingTree(IVar, T, P) end),
    {Result, _} = pm:get(IVar),
    Result.

% Traverses the tree and puts values into the IVar along the way
startTraversingTree(IVar, T, P) ->
    SyncRes = traverseTree(IVar, T, P),
    pm:put(IVar, {SyncRes, nothing}).

% Recursively traverses tree
traverseTree(IVar, T, P) ->
    case T of
        leaf ->
            true;
        {node, E, L, R} ->
            pm:put(IVar, {false, E}),
            LR = traverseTree(IVar, L, P),
            RR = traverseTree(IVar, R, P),
            LR and RR and robustEval(P, E)
    end.

%% Utility functions

% Applies P to t; any other result than true will return false
robustEval(P, T) ->
    Res = try P(T)
          catch
              _ : _ -> false
          end,
    case Res of
        true ->
            true;
        _ ->
            false
    end.
