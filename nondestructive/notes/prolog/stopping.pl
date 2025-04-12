#!/usr/bin/swipl -q

% First, let us look at the 'not' predicate. It is hella weird
%
% Surprisingly, not breaks logical semantics.

% not can never make new values for variables:
% it the inner thing failed, they will not get bound, but not will succeed.
% if the inner thing succeeded, then not will make it fail and nothing will be
% bound
some_pred(X, Y) :-
    X = abrak,
    not(X = Y), % This will fail, always if Y is unbound
    Y = adabra.

some_other_pred(X, Y) :-
    X = abrak,
    Y = adabra,
    not(X = Y). % And this will always succeed

% this is because predicates work like 'can you get values so the thing is
% true'? With 'not' it can't do so, because the inner expression can't
% be made to behave as though it succeeded when in reality it failed
% (fail means stop and backtrack, not just logical fail).

% Kinda weird, not really useful as a purely logical 'not'
%
% Also can be written as
some_pred2(X, Y) :-
    X \= Y, % not( = )
    \+ X = Y. % \+ is not

% Now let's try to make del_all with not, which will delete not some inclusions
% of X, but all of them

del_all_broken(_, [], []).
del_all_broken(Elem, [Elem|Tail], Res) :- del_all_broken(Elem, Tail, Res).
del_all_broken(Elem, [X|Tail], [X|NewTail]) :-
    X \= Elem,
    del_all_broken(Elem, Tail, NewTail).

% the diff from del_any is X \= Elem -- this will short circuit and not allow
% leaving Elem in, turning it to del_all.
%
% However, this does not always work)
% for example del_all(X, [1, 2], [1]). will just fail
%
% This is because if Elem is not bound, the third clause will always
% fail, because the = in \= always binds and thus \= fails.
% This could be hacked around with var/nonvar predicates which special
% case the unbound case, but that is quite ugly.

% Sincle logical semantics are now dead and we are in procedural prolog land,
% we can solve this problem better, with explicitly cutting off waypoints.
% ! cuts all waypoints up to this point.

del_all(_, [], []) :- !.
del_all(Elem, [Elem|Tail], Res) :- !, del_all(Elem, Tail, Res).
del_all(Elem, [X|Tail], [X|NewTail]) :- del_all(Elem, Tail, NewTail).

% Here on final and when we successfully remove an elem, we cut waypoints,
% making it so the solver never tracks back to check what if we didn't remove
% Elem (and it is important the the remove+cut clause is first).
%
% However, this version still fails on the above case -- it just binds X
% to the first thing (1) and then dies. Fixing this case would be moving the
% cut to the third clause, but that would break the regular 'del all' case

del_all_choice(_, [], []) :- !.
del_all_choice(Elem, [Elem|Tail], Res) :- del_all_choice(Elem, Tail, Res).
del_all_choice(Elem, [X|Tail], [X|NewTail]) :-
    !, del_all_choice(Elem, Tail, NewTail).

% Now, back to listrev
% We just cut at the empty list -- this prevents listrev(X, []) inf recursion.
listrev(L, R) :- listrev(L, R, []).
listrev([], A, A) :- !.
listrev([H|T], R, A) :- listrev(T, R, [H|A]).

% Fun fact -- in del_all we cut off definitely wrong solutions.
% In listrev, we don't cut off solutions -- we cut of dead pathes
