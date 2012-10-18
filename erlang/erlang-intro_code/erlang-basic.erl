%% Example code produced "during" Advanced Programming lecture.  
%% 
%% Example to be copy and pasted to the erl-shell
%%
%% Date: Oct 11, 2011
%% Author: Ken Friis Larsen <kflarsen@diku.dk>


% Fun with list comprehensions
Digits = [0,1,2,3,4,5,6,7,8,9].

Evens = [ X || X <- Digits, X rem 2 =:= 0].

Cross = [{X,Y} || X <- [1,2,3,4],
                  Y <- [11,22,33,44]].

EvenXs = [{X,Y} || {X,Y} <- Cross,
                   X rem 2 =:= 0].

% Function litteral with one clause, and a `case`-expression
Move = fun(Dir, {X,Y}) ->
               case Dir of
                   north -> {X, Y+1};
                   west  -> {X-1, Y}
               end
       end.


% Function litteral with two clauses
Move2 = fun
	    (north, {X,Y}) -> {X, Y+1};
	    (west, {X,Y})  -> {X-1, Y}
	end.
