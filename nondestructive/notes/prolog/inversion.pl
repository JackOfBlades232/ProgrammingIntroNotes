#!/usr/bin/swipl -q

% This script breaks into the interpreter, defining the db and rules to play
% with.

% let's use predicate inversion for not standard logical operations.
% For eaxmple, lets make a predicate, the inversion of which yields a list
% with one or more elements of the value X removed.

% since predicates don't return values, we'll use the third arg to bind it in

del_any(_, [], []). 
del_any(Elem, [Elem|Tail], Res) :- del_any(Elem, Tail, Res).
del_any(Elem, [X|Tail], [X|NewTail]) :- del_any(Elem, Tail, NewTail).

% first -- recursion base
% second -- removes first elem and proceeds
% third -- appends list head to ret and applies recursively to tail

% if Res is free, when this effectively constructs the result there.
% However, the solver will backtrack to every point where second is taken,
% so inversion will yield __all__ lists where some of Elem are returned.

% If we change the order of second and third, the order of solutions changes.
del_any2(_, [], []). 
del_any2(Elem, [X|Tail], [X|NewTail]) :- del_any2(Elem, Tail, NewTail).
del_any2(Elem, [Elem|Tail], Res) :- del_any2(Elem, Tail, Res).

% This can be used in all sorts of ways -- we can specify all args, then
% we just check if it's true.
%
% We can specify Res, but not elem -- then we get the elem that has to
% be removed to get res from input.
%
% Only specify the list -- and we get all variations of Elem x What is Left
%
% Don't specify the list -- and we get an infinite seq of solutions,
% which is fine cause we don't exhaust it if not needed. However, this only
% works w/ del_any2 -- del_any will die in infinite recursion. Also, since
% the solver defines a total order even on an infinite solution set, some
% solutions are uncreachable.

% now let's write join -- a predicate, that checks whether arg1 concatenated
% with arg2 is arg3 (and automatically allows us to concatenate lists).

join([], L, L).
join([H|T], L, [H|M]) :- join(T, L, M).

% This can also be inverted in all sorts of ways -- cat 2 lists, get all
% splits of a list, etc.

% now lets reverse a list -- i. e. make a predicate that is true when
% args are the same list reversed.

% hepler three-arg pred is different cause of arity
listrev(L, R) :- listrev(L, R, []).

% in second we just 'construct' the inverse of L into the accum until
% L is empty, then decide if accum is same as R.
listrev([], A, A).
listrev([H|T], R, A) :- listrev(T, R, [H|A]).

% This too has a problem -- when inverted as listrev(L, [...]), and we don't
% halt from the interpreter, this thing will go into infinite recursion and
% crash the solver -- because we manipulate the first arg, it will just
% try everything under the sun until the list is too large.
%
% This beh seems common -- some inversion scenarios go into infinite recursion.
% This problem has a solution, but it breaks the logical model, cause it
% controls the way the solver traverses solution space.
