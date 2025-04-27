#!/usr/bin/swipl -q

% Now, seeing as logical sematic is not enough, there are explicit predicates
% for specifically ensuring correct inversion.

% var and nonvar check for unbound variables. Note, that if X is bound
% to a free var Y, then var(X) succeeds. The intuation is thus --
% X = term always succeeds at this point.

% let's remember prefix and fix it (var N case)

prefix([], _, 0).
prefix([H|T2], [H|T], N) :-
    nonvar(N), N2 is N - 1, prefix(T2, T, N2).
prefix([H|T2], [H|T], N) :-
    var(N), prefix(T2, T, N2), N is N2 + 1.

% Other useful stuff -- =.. is predicate 'lhs is a comp term, rhs is list with
% same elements'
% i.e. p(1, 2, 3) =.. [p, 1, 2, 3]
% This can be inverted both sides, so allows creating terms and lists.
%
% Also name -- checks/converts atom and charcode list for name
%
% For strings convertions string_codes and atom_string

% For nondeterministic goals (calc of predicate which yields multiple returns)
% prolog allows getting all solutions. findall/3, findall(Template, Goal, List),
% Goal is the predicate with some args, list is the thing to unify the result w/
% Template is the thing to be unfied with every found solution, kinda

% so, for

color(red).
color(green).
color(blue).

% findall(X, color(X), Result) gives just [red, green, blue]
% and findall([X], color(X), Result) gives just [[red], [green], [blue]]

% The real use for this shows when we put multiple free vars into the
% goal and thus bind them all into the template

parent("Mary", "Ann").
parent("Mary", "John").
parent("George", "John").
parent("Hamish", "George").
parent("Hannah", "Mary").

% findall([A, D], parent(A, D), Result) will give all pairs of [parent, child]

% bagof is the same as findall, but if we don't specify some vars in the
% template, we will get a combination for every var that is in the goal but
% not in the template. setof is same, but orders and deduplicates solution.

% Prolog also allows for so called 'data base'.
% This is a collection of facts and predicates that can be altered at runtime.

% we can make predicates dynamic with the following goal in a rule

:- dynamic parent/2, color/1.

% For adding asserta and assert/assertz are used. a to beginning, z to end,
% as order can be important for the solver.
%
% For removal, retract and retractall are used. retractall takes a comp term
% and removes all it's results.
%
% retract is a bit more complicated -- it removes facts one by one, and
% each time unifies free vars with ones from the retracted fact.

% One can also add predicates with assert/retract if they have not been
% declared at all -- dynamic is not required.

% Using dynamic procedure predicates is possible, but not recommended.

% database can be used to declare global vars
:- dynamic global_vars/2.

assign_var(Var, Value) :-
    retractall(global_vars(Var, _)),
    asserta(global_vars(Var, Value)).
