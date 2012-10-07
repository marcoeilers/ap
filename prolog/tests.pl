:- begin_tests(tests).
:- ensure_loaded('numbersets.pl').

test(numZero) :-
        num(z).

test(numNonZero) :-
        num(s(s(s(s(z))))).

test(lessZero) :-
	less(z, s(s(z))).

test(lessNonZero) :-
	less(s(z), s(s(z))).

test(lessFail, [fail]) :-
	less(s(s(z)), s(z)).

test(checksetEmpty) :-
	checkset([]).

test(checksetSingleNum):-
	checkset([s(s(z))]).

test(checksetNonNum, [fail]):-
	checkset([x]).

test(checksetSeveralNums) :-
	checkset([z, s(s(z)), s(s(s(z)))]).

test(checksetFail, [fail]) :-
	checkset([z, s(s(z)), s(z)]).

test(memberNoEmpty) :-
	ismember(s(z), [], no).

test(memberNoNotIncluded) :-
	ismember(s(z), [z,s(s(z)), s(s(s(z)))], no).

test(memberNoNotIncluded, [fail]) :-
	ismember(s(z), [z,s(s(z)), s(s(s(z)))], yes).

test(memberYesSingle) :-
	ismember(z, [z], yes).

test(memberNoSingleFail, [fail]) :-
	ismember(z, [z], no).

test(memberYesIncluded) :-
	ismember(s(z), [z, s(z), s(s(s(z)))], yes).

test(memberNoIncludedFail, [fail]) :-
	ismember(s(z), [z, s(z), s(s(s(z)))], no).

test(memberX, [true(X==yes)]) :-
	ismember(z, [z,s(z)],X).

test(memberYesX, [all(X==[z,s(z),s(s(s(z)))])]) :-
	ismember(X, [z,s(z),s(s(s(z)))],yes).

test(unionEmpty) :-
	union([],[],[]).

test(unionOneEmptyX, [true(X==[z, s(s(z))]) ]) :-
	union([], [z, s(s(z))], X).

test(unionNonEmptyX, [true(X==[z, s(z), s(s(z))])]) :-
	union([z,s(s(z))], [s(z), s(s(z))], X).

test(unionFirstX, [all(X==[[z], [z, s(z)]])]) :-
	union(X, [s(z)], [z, s(z)]).

test(unionFail, [fail]) :-
	union(_, [s(z)], [z, s(s(z))]).

test(unionXY, [all([X,Y]==[[ [z], []], [[], [z]], [[z], [z]]])]) :-
	union(X, Y, [z]).

test(intersectionEmpty) :-
	intersection([], [], []).

test(intersectionOneEmptyX, [true(X==[])]) :-
	intersection([], [z, s(z)], X).

test(intersectionNonEmptyX, [true(X==[s(s(z))])]) :-
	intersection([z, s(s(z))], [s(z), s(s(z))], X).

test(intersectionFail, [fail]) :-
	intersection([z], [s(z)], [s(z)]).



:- end_tests(tests).
