-module(hhfuns).
-export([]).
-compile(export_all).

one() -> 1.
two() -> 2.

add(X,Y) ->
    X() + Y().
