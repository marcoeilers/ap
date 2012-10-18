-module(what_the_if).
-export([heh_fine/0,oh_god/1,help_me/1]).

heh_fine() ->
    if 1 =:= 1 ->
	    works
    end,
    if 1 =:= 2; 1 =:= 1 ->
	    works
    end,
    if 1 =:= 2, 1 =:= 1 ->
	    fails
    end.
       
oh_god(N) ->
    if N =:= 2 -> might_succeed;
       true -> always_does
    end.

%% Demonstrating how if's are basically just guards (and must always
%% return something). The 'true' bit can be considered an 'else' part.
help_me(Animal) ->
    Talk = if Animal == cat -> "meow";
	      Animal == dog -> "bark";
	      Animal == beef -> "moo";
	      Animal == tree -> "bark";
	      true -> "fnafignafi" end,
    {Animal, "says " ++ Talk ++ "!" }.
