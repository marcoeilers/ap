/* -*- mode: prolog -*- */

% Some problems with duplicates for union and intersection left.

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

% union([s(z)],Y,[z,s(z)]).
% has duplicate of Y=[z,s(z)]
union(X,[],X):- checkset(X).
union([],X,X):- checkset(X).
union([H|R],[A|B],[H|T]) :- 
	union(R,[A|B],T),
	less(H,A),
	checkset([A|B]), 
	checkset([H|T]),
	checkset([H|R]).
union([A|B],[H|R],[H|T]) :- 
	union([A|B],R,T),
	less(H,A),
	checkset([A|B]), 
	checkset([H|T]),
	checkset([H|R]).
union([H|B],[H|R],[H|T]) :- 
	union(B,R,T),
	checkset([H|B]), 
	checkset([H|T]),
	checkset([H|R]).

%some duplicates
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
intersection(_,[],[]).
intersection([],_,[]).
	


