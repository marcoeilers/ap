/* -*- mode: prolog -*- */

/*
           bob    juliet
            |      |        
            +------+------+
            |      |      |
 peter    lisa   paul   mary   
   |        |             |     
   +--------+             +-------+
            |             |       |
          harry          dick  sandra
*/

female(mary).
female(sandra).
female(juliet).
female(lisa).
male(peter).
male(paul).
male(dick).
male(bob).
male(harry).
parent(bob, lisa).
parent(bob, paul).
parent(bob, mary).
parent(juliet, lisa).
parent(juliet, paul).
parent(juliet, mary).
parent(peter, harry).
parent(lisa, harry).
parent(mary, dick).
parent(mary, sandra).

father(X, Y) :- male(X), parent(X, Y).
mother(X, Y) :- female(X), parent(X, Y).

brother(X, Y) :-
	parent(Z, X),
	parent(Z, Y),
	male(X),
	X \= Y.

sister(X, Y) :-
	parent(Z, X),
	parent(Z, Y),
	female(X),
	X \= Y.

/* is X grandmother of Y? */
grandmother(X, Y) :-
	parent(X, Z), % X is the parent of some Z, AND
	parent(Z, Y), % Z is the parent of Y
	female(X).    % plus X is female.

haveChild(X,Y) :-
	father(X,Z),
	mother(Y,Z).

/* Cousins if parents are brother/sister */
cousin(X, Y) :-
	parent(Z, X),
	parent(W, Y),
	( brother(Z, W); sister(Z, W) ).
