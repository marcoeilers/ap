-module(quicksort).
-export([sort/1,lsort/1,lc_quicksort/1]).


lc_quicksort([]) ->
    [];
lc_quicksort([Pivot|Rest]) -> lc_quicksort([Smaller || Smaller <- Rest, Smaller =< Pivot])
				  ++ [Pivot] ++
				  lc_quicksort([Larger || Larger <- Rest, Larger > Pivot]).


sort([]) ->
    [];
sort([Pivot|Rest]) ->
    {Smaller, Larger} = partition(Pivot, Rest, [], []),
    sort(Smaller) ++ [Pivot] ++ sort(Larger).

partition(_, [], Smaller, Larger) ->
    {Smaller, Larger};
partition(Pivot, [H|T], S, L) ->
    if H =< Pivot -> partition(Pivot, T, [H|S], L);
       true       -> partition(Pivot, T, S, [H|L])
    end.
