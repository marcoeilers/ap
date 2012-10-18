% Demonstrating how two differently written functions are the same
-module(beach).
-export([beach_c/1, beach_f/1]).

beach_c(Temperature) ->
    case Temperature of
	{celsius, N} when N >= 20, N =< 45 ->
	    'favorable';
	{kelvin, N} when N >= 293, N =< 318 ->
	    'scientifically favorable';
	{fahrenheit, N} when N >= 68, N =< 113 ->
	    'favorable in the US';
	_ -> 'avoid beach'
    end.

beach_f({celsius, N}) when N >= 20, N =< 45 ->
    'favorable';
beach_f({kelvin, N}) when N >= 293, N =< 318 ->
    'scientifically favorable';
beach_f({fahrenheit, N}) when N >= 68, N =< 113 ->
    'favorable in the US';
beach(_) ->
    'avoid beach'.
