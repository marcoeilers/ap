/* -*- mode: prolog -*- */

% It seems to me that everything works now.

less(z,s(X)):- num(X).
less(s(X), s(Y)) :- less(X,Y).


num(z).
num(s(X)) :- num(X).

checkset([]).
checkset([X]):- num(X).
checkset([A,B|C]) :- 
	less(A,B), 
	checkset([B|C]).


ismember(X,Y,no):-
	checkset([X|Y]),
	checkset(Y).
ismember(X,Y,no):-
	checkset([Y|X]),
	checkset(Y).
ismember(X,[H|T],no):-
	checkset([H,X|T]),
	ismember(X,T,no),
	checkset([H|T]).

ismember(X,[X|Z],yes) :- 
	checkset([X|Z]).
ismember(X,[Y|Z],yes) :- 
	checkset([Y|Z]), 
	ismember(X,Z,yes).

union([],[],[]).
union([H|R],Y,[H|T]) :- 
	union(R,Y,T),
	checkset(Y), 
	checkset([H|T]),
	checkset([H|R]).
union(X,[H|R],[H|T]) :- 
	union(X,R,T),
	checkset(X), 
	checkset([H|T]),
	checkset([H|R]).
union([H|B],[H|R],[H|T]) :- 
	union(B,R,T),
	checkset([H|B]), 
	checkset([H|T]),
	checkset([H|R]).

intersection([H|B], [H|D], [H|F]):-
	intersection(B,D,F),
	checkset([H|B]),
	checkset([H|D]),
	checkset([H|F]).
intersection([A|B], [C|D], I):-
	less(A,C),
	intersection(B,[C|D],I),
	checkset([A|B]),
	checkset([C|D]),
	checkset(I).
intersection([A|B], [C|D], I):-
	less(C,A),
	intersection([A|B],D,I),
	checkset([A|B]),
	checkset([C|D]),
	checkset(I).
intersection([_|_],[],[]).
intersection([],[_|_],[]).
intersection([],[],[]).
	


