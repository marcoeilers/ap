-module(functions).
-compile(export_all).

%% List functions (hd
head([H|_]) -> H.
tail([_|T]) -> T.


snd({_,Y}) -> Y.
fst({X,_}) -> X.


same(X,X) ->
    true;
same(_,_) ->
    false.

%% Usage:
%% > functions:valid_date({{2012, 10, 11}, {17, 50, 00}}).
valid_date({ Date = {Y,M,D}, Time = {H, Min, S}}) ->
    io:format("The Date tuple (~p) says today is: ~p/~p/~p,~n",[Date,Y,M,D]),
    io:format("The time tuple (~p) indicates: ~p:~p:~p.~n", [Time,H,Min,S]);
valid_date(_) ->
    io:format("Stop feeding me wrong data!~n").

old_enough(X) when X >= 16 andalso X =< 104 -> true;
old_enough(_) -> false.
