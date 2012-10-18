%% Example code produced "during" Advanced Programming lecture.  
%% Working with trees
%% Date: Oct 11, 2011
%% Author: Ken Friis Larsen <kflarsen@diku.dk>

-module(trees).
-export([sampletree/0, contains/2]).

sampletree() -> 
    {node, 6, {node, 3, leaf, leaf}, 
              {node, 9, leaf, leaf}}.

contains(_, leaf) -> false;
contains(Key, {node, K, Left, Right}) ->
    if
        Key =:= K -> true;
        Key < K   -> contains(Key, Left);
        Key > K   -> contains(Key, Right)
    end.
