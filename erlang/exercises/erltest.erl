-module(erltest).
-export([move/2,qsort/1]).

move(north, {X,Y}) -> {X, Y+1};
move(west,  {X,Y}) -> {X-1,Y}.

qsort([]) -> [];
qsort([Pivot|Rest]) -> qsort([X || X <- Rest, X < Pivot]) ++ [Pivot] ++ qsort([X || X <- Rest, X >= Pivot]).


emove(north, {X,Y}) -> {X, Y+1};
emove(west,  {0,_)  -> throw(invalid_move).

%% ignore_invalid(Dir, Pos) -> try emove(Dir, Pos)
%% 			    catch 
				
