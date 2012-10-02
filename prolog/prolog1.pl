bigger(elephant, horse).
bigger(horse, donkey).
bigger(donkey, dog).
bigger(donkey, monkey).

/* Is this a comment? Yes! */
is_bigger(X, Y) :- bigger(X, Y).
is_bigger(X, Y) :- bigger(X, Z), is_bigger(Z, Y).

/* Try the query: 
 * ?- is_bigger(donkey, X), is_bigger(X, monkey).
 */

hello :- write('Hello, World!'), nl.


/* Matching examples */
/*

X = Y. #=> true, because they are both free variables

_ = _. #=> true

p(X, 2, 2) = p(1, Y, X). #=> false, because X cannot be instantiated to 1 and 2 at the same time

f(a, g(X, Y)) = f(X, Z), Z = g(W, h(x)). #=>
X = a,
Y = h(a),
Z = g(a, h(a)),
W = a.

f(a, g(X, Y)) = f(X, Y), Z = g(W, h(x)). #=> this does not behave nicely in gprolog.
                     ^
*/