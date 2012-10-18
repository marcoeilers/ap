-module(hhfuns).
-export([]).
-compile(export_all).

one() -> 1.
two() -> 2.

add(X,Y) ->
    X() + Y().

add() ->
    add(fun one/0, fun two/0).


map(_, []) ->
    [];
map(F, [H|T]) ->
    [F(H)|map(F,T)].

zip([],_) ->
    [];
zip(_,[]) ->
    [];
zip([X|Y],[Z|V]) ->
    [{X,Z}|zip(Y,V)].


zipWith(_, [], _) ->
    [];
zipWith(_, _, []) ->
    [];
zipWith(F, [H1|T1], [H2|T2]) ->
    [F(H1, H2)|zipWith(F,T1,T2)].
