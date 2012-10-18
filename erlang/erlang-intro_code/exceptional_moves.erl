%% Example code produced "during" Advanced Programming lecture.  
%% Throwing and catching exceptions
%% Date: Oct 7, 2010
%% Author: Ken Friis Larsen <kflarsen@diku.dk>

-module(exceptional_moves).
-export([move/2,ignore_invalid/2]).

move(north, {X, Y}) -> {X, Y+1};
move(west, {0, _}) -> throw(invalid_move);
move(west, {X, Y}) -> {X-1, Y}.

ignore_invalidW(Dir, Pos) ->
    try move(Dir, Pos) of
	{100,100} -> {0,0};
	NewPos -> NewPos
    catch
        invalid_move -> Pos
    end.
        
ignore_invalid(Dir, Pos) ->
    try move(Dir, Pos)
    catch
        invalid_move -> Pos
    end.

