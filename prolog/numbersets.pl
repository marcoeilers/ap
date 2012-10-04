/* -*- mode: prolog -*- */

% SEEMS to work for most cases that I've tried. 
% However, returns a big amount of duplicates
% and runs infinitely instead of failing if there
% are no new cases for some predicates (in some modes).

less(z,s(_)).
less(s(X), s(Y)) :- less(X,Y).

num(z).
num(s(X)) :- num(X).

equal(z,z).
equal(s(X),s(Y)) :- equal(X,Y).

equalList([],[]).
equalList([H|T],[P|Q]) :- 
	equal(H,P), 
	equalList(T,Q).

different(X,Y) :- less(X,Y).
different(X,Y) :- less(Y,X).

geq(X,Y):-equal(X,Y).
geq(X,Y):-less(Y,X).

checkset([]).
checkset([_]).
checkset([H,X]) :- less(H,X).
checkset([A,B|C]) :- 
	less(A,B), 
	checkset([B|C]).

ismember(_,[],no).
ismember(X,[Y|Z],no) :- 
	checkset([Y|Z]), 
	different(X,Y), 
	num(X), 
	ismember(X,Z,no).
ismember(X,[Y|Z],yes) :- 
	checkset([Y|Z]), 
	equal(X,Y), 
	num(X).
ismember(X,[Y|Z],yes) :- 
	checkset([Y|Z]), 
	num(X), 
	ismember(X,Z,yes).

allmembers([],_).
allmembers([H|T],Y):-
	ismember(H,Y,yes), 
	allmembers(T,Y),
	checkset([H|T]).

insert(X,[],[X]).
insert(X,[H|T],S) :- 
	less(X,H), 
	equalList([X,H|T],S),
	checkset([H|T]).
insert(X,[X|T],[X|T]) :- 
	checkset([X|T]).
insert(X,[H|T],[H|R]) :- 
	less(H,X), 
	insert(X,T,R), 
	checkset([H|T]).


union(X,[],X):- checkset(X).
union([],X,X):- checkset(X).
union([H],Y,S) :- 
	insert(H,Y,S).
union([H],Y,S) :- 
	insert(H,Y,S).
union([H|R],Y,S) :- 
	insert(H,Y,T), 
	union(R,T,S),
	checkset(Y), 
	checkset([H|R]),
	checkset(S).

intersection(_,[],[]).
intersection([],_,[]).
intersection([A|B],Y,[A|T]):-
	ismember(A,Y,yes),
	intersection(B,Y,T),
	checkset([A|B]),
	checkset(Y),
	checkset([A|T]).
intersection([A|B],Y,T):-
	ismember(A,Y,no),
	intersection(B,Y,T),
	checkset([A|B]),
	checkset(Y).


