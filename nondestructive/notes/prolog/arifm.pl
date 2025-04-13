#!/usr/bin/swipl -q

% Arifm is done with 2-place functor-ops * / + -
% rational numbers can be written 2 rdiv 3 (rdiv(2, 3))

% For prolog these are just regular compound terms, but the is/2
% spec predicate that calculates the expression (if there is a free var,
% it errs), and compares/binds it to lhs

listlen([], 0).
listlen([_|T], L) :- listlen(T, TL), L is TL + 1.

% Apart from is, < > >= and =< predicates also force calculation. 
% NOTE: >= and =< are comparisons, <= is a different thing.
% and =:= and =\= (eq/not eq in arithmetic sense).
%
% This is because, obviously, the prolog solver can't solve equations.
% Also these predicates are not completely 'logical' -- they don't allow
% for inversion, as everything has to be bound.
%
% Let's make some more stuff

prefix([], _, 0).
prefix([H|T2], [H|T], N) :- N2 is N - 1, prefix(T2, T, N2).

% NOTE -- does not work with free N

% Now lets make a predicate that decides that smth is 0 <= x <= 10 and lists
% all for a free var without listing the things out

% This is tricky cause for comparisons we first have to bind vars

% This thing
% a) goes into inf rec when out of bounds
% b) after first 'true' also goes into inf rec
ten_broken(1).
ten_broken(X) :- ten_broken(Y), X is Y + 1, X =< 10.

% All of these will only be or for 1 (base) and 2 (one rec Y + 1).
ten_broken2(1).
ten_broken2(X) :- ten_broken2(Y), !, X is Y + 1, !, X =< 10, !.

% The idea is to cut only once all correct solutions have been found
ten(1).
ten(X) :- ten(Y), ((Y >= 10, !, fail) ; X is Y + 1).

% Can also be done by making a between pred
arebetween(A, B, _) :- A > B, !, fail.
arebetween(A, _, A).
arebetween(A, B, X) :- A1 is A + 1, arebetween(A1, B, X).

ten2(N) :- arebetween(1, 10, N).

% Basically we recurse and backtrack to deeper recursion each time, but
% once we meet the end condition -- we cut and fail.
