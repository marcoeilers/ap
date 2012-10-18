%% Example code produced "during" Advanced Programming lecture.  
%% Demonstrate how to declare functions
%% Date: Oct 7, 2010
%% Author: Ken Friis Larsen <kflarsen@diku.dk>

-module(erltest).
-export([move/2, qsort/1]).

move(north, {X, Y}) -> {X, Y+1};
move(west, {X, Y}) -> {X-1, Y}.

secret(_) ->
     42.

qsort([]) -> [];
qsort([Pivot|Rest]) ->
    qsort([X || X <- Rest, X < Pivot])
    ++ [Pivot] ++
    qsort([X || X <- Rest, X >= Pivot]).
