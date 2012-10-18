%% Notes section:
%% 
%% Compile in Emacs with C-c C-k (or M-x erlang-compile)
%%
%% Or, alternatively:
%%
%% > c("/path/to/useless", [{outdir, "/path/to/output/dir"}, export_all]).
%%
%% where the second argument are compile flags. Common compile flags are:
%%
%% * debug_info, for use with debuggers
%% * {outdir, Dir}
%% * export_all, export all functions (ignore the -export directive)
%% * {d, Macro} or {d, Macro, Value}, defines a macro to be used in
%%   the module, where Macro is an atom. 

%% Adding:
%%
%% -compile([debug_info, export_all]).
%%
%% to the file achieves the same as c() or compile:file(). Does not
%% work in Emacs.

%% Erlang macros:
%% '-define(MACRO, some_value).' and used as '?MACRO'


-module(useless).
-export([add/2,greet_and_add_two/1]).

-define(NL, "~n").
-compile([export_all]).
-author("Thomas Jespersen").
-vsn("0.1").

add(A, B) ->
    A + B.

hello() ->
    io:format("Hello, World!" ++ ?NL).

greet_and_add_two(X) ->
    hello(),
    add(X, 2).

%% Printing:
%%
%% Using io:format(format, [list_of_args]).
%%
%% A lot like printf, but uses ~ instead of %, so %s is really ~s, \n
%% is ~n. ~~ prints a single ~. ~f is for floating point numbers.
%%
%% An interesting one is ~p, which prints any type, try:
%%
%% > io:format("~p~n", [ [1,2,3] ]).
%%
%% Compare:
%%
%% > io:format("~s~n",[<<"Hello">>]).
%% > io:format("~p~n",[<<"Hello">>]).

greet(male, Name) ->
    io:format("Hello, Mr. ~s" ++ ?NL, [Name]);
greet(female, Name) ->
    io:format("Hello, Mrs. ~s" ++ ?NL, [Name]);
greet(_, Name) ->
    io:format("Hello, ~s" ++ ?NL, [Name]).


