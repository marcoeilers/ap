-module(dolphins).
-compile(export_all).

%% Spawning this dolphin and passing it a message can succeed once,
%% then no answers are given.
dolphin1() ->
    receive
	do_a_flip ->
	    io:format("How about no?~n");
	fish ->
	    io:format("So long and thanks for all the fish~n");
	_ ->
	    io:format("Heh, we're smarter than you humans~n")
    end.

%% Now we can respond to the caller
dolphin2() ->
    receive
	{From, do_a_flip} ->
	    From ! "How about no?~n";
	{From, fish} ->
	    From ! "So long and thanks for all the fish~n";
	{From, _} ->
	    From ! "Heh, we're smarter than you humans~n"
    end.


dolphin3() ->
    receive
	{From, do_a_flip} ->
	    From ! "How about no?",
	    dolphin3();
	{From, fish} ->
	    From ! "So long and thank for all the fish!";
	{From, _} ->
	    From ! "Heh, we're smarter than you humans",
	    dolphin3()
    end.
	  
