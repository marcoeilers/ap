:- begin_tests(tests).
:- ensure_loaded('numbersets.pl').

% z must be recognized as a number...
test(numZero) :-
        num(z).

% ... just like numbers using the s() constructor
test(numNonZero) :-
        num(s(s(s(s(z))))).

% invalid numbers should be rejected.
test(numFail, [fail]) :-
	num(s(x)).

% make sure 0 < 2
test(lessZero) :-
	less(z, s(s(z))).

% make sure 1 < 2
test(lessNonZero) :-
	less(s(z), s(s(z))).

% 2 < 1 should fail
test(lessFail, [fail]) :-
	less(s(s(z)), s(z)).

% empty lists are always valid sets...
test(checksetEmpty) :-
	checkset([]).

% ... as are sets consisting of a single number
test(checksetSingleNum):-
	checkset([s(s(z))]).

% a list with a non-number in it is invalid
test(checksetNonNum, [fail]):-
	checkset([x]).

% check a valid set with several numbers (should be accepted)
test(checksetSeveralNums) :-
	checkset([z, s(s(z)), s(s(s(z)))]).

% make sure the invalid one gets rejected (elements are in wrong order)
test(checksetFail, [fail]) :-
	checkset([z, s(s(z)), s(z)]).

% make sure the invalid one gets rejected (duplicates are not allowed)
test(checksetDuplicateFail, [fail]) :-
	checkset([z, s(z), s(z)]).

% no number is a member of an empty set
test(memberNoEmpty) :-
	ismember(s(z), [], no).

% make sure ismember returns no if the number is not included in the set
test(memberNoNotIncluded) :-
	ismember(s(z), [z,s(s(z)), s(s(s(z)))], no).

% ismember with yes should fail if the number is not included
test(memberNoNotIncluded, [fail]) :-
	ismember(s(z), [z,s(s(z)), s(s(s(z)))], yes).

% make sure a number is recognized as part of a one-number set...
test(memberYesSingle) :-
	ismember(z, [z], yes).

% ... and that ismember with no fails in this case
test(memberNoSingleFail, [fail]) :-
	ismember(z, [z], no).

% same for a set with several numbers
test(memberYesIncluded) :-
	ismember(s(z), [z, s(z), s(s(s(z)))], yes).

% again, no should fail if the number is included
test(memberNoIncludedFail, [fail]) :-
	ismember(s(z), [z, s(z), s(s(s(z)))], no).

% this should also work if we leave the last parameter unbound
test(memberX, [true(X==yes)]) :-
	ismember(z, [z,s(z)],X).

% with the first parameter unbound and yes as the last
% we should get all numbers in the set as values for X
test(memberYesX, [all(X==[z,s(z),s(s(s(z)))])]) :-
	ismember(X, [z,s(z),s(s(s(z)))],yes).

% We cannot test the same configuration with no, because
% X would have unfinitely many valid values and plunit cannot 
% handle this. Therefore we tested this by hand.
% The same is true for any mode where the second paramenter is 
% unbound.

% the union of two empty sets should be an empty set.
test(unionEmpty) :-
	union([],[],[]).

% the union of a set and an empty set has to be the set itself.
test(unionOneEmptyX, [true(X==[z, s(s(z))]) ]) :-
	union([], [z, s(s(z))], X).

% make sure union works on nonempty sets with overlapping range
% and elements present in both sets
test(unionNonEmptyX, [true(X==[z, s(z), s(s(z))])]) :-
	union([z,s(s(z))], [s(z), s(s(z))], X).

% make sure union enumerates all possibilities when the first
% param is unbound
test(unionFirstX, [all(X==[[z], [z, s(z)]])]) :-
	union(X, [s(z)], [z, s(z)]).

% there t_3 cannot be a union of t_1 and t_2 if t_2 contains an
% element not in t_3, so this should fail
test(unionFail, [fail]) :-
	union(_, [s(z)], [z, s(s(z))]).

% make sure all possibilities are enumerated if both first params
% are unbond
test(unionXY, [all([X,Y]==[[ [z], []], [[], [z]], [[z], [z]]])]) :-
	union(X, Y, [z]).

% the intersection of two empty sets has to be an empty set
test(intersectionEmpty) :-
	intersection([], [], []).

% the intersection of a non-empty set and an empty set has to
% be empty as well
test(intersectionOneEmptyX, [true(X==[])]) :-
	intersection([], [z, s(z)], X).

% make sure intersection works on two nonempty sets containing 
% both common and distinct elements
test(intersectionNonEmptyX, [true(X==[s(s(z))])]) :-
	intersection([z, s(s(z))], [s(z), s(s(z))], X).

% t_1 must include all elements in t_3, so this should fail.
test(intersectionFail, [fail]) :-
	intersection([z], s(z), [s(z)]).

% Again, we cannot test the cases with infinitely many valid 
% outputs here.

:- end_tests(tests).
