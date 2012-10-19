-module(multiproc).
-compile(export_all).


important() ->
    receive
	{Prio, Msg} when Prio > 10 ->
	    [{Prio, Msg} | important()]
    after 0 ->
	    normal()
    end.

normal() ->
    receive
	{Prio, Msg} ->
	    [{Prio, Msg} | normal()]
    after 0 ->
	    []
    end.
