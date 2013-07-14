-module(tree_test).
-compile(export_all).

tree_of(Xs) -> build([{leaf,X} || X <- Xs ]).
 
build([]) -> leaf;
build([{leaf,X}]) -> {node, X, leaf, leaf};
build([{{node,Y,T1,T2}, X}]) -> {node, Y, T1, build([{T2, X}])};
build(List) -> build(sweep(List)).

sweep([]) -> [];
sweep([Ts]) -> [Ts];
sweep([{T1,X1},{T2,X2}|Ts]) -> [{{node, X1, T1, T2},X2}|sweep(Ts)].

