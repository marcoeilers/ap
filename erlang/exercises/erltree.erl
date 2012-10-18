
-module(erltree).
-export([sampletree/0,contains/2]).

sampletree() -> { node, 6, {node, 3, leaf, leaf},
		  {node, 9, leaf, leaf}}.

contains(_, leaf) -> false;
contains(Key, {node, N, subtree1, subtree2}) ->
    if
	Key =:= N -> true;
	Key < N -> contains(Key, subtree1);
	Key > N -> contains(Key, subtree2)
    end.
		  
	 
