#!/usr/bin/swipl -q

% a prolog program is a series of clauses:
% head :- tail.
% can also be: head.
% or: :- tail.
%
% head (if exists) is one compound term. Main functor and arity give
% attribute this clause to a predicate/procedure. Many clauses can
% apply to the same procedure. Order is important, cause it's like program text.
%
% the tail is a sequence of terms separated by , and ; which are respectively
% 'and' and 'or'. , and ; are actually just main functors, and the whole thing
% is a compound term.
% Each term in the tail code a procedure 'call' and are called
% 'goals'. Each goal either fails or succeeds. The outcome of the proc
% is the logical sucess with respect to , and ;
%
% For , we just go while we succeed, then we stop (so at first glance it
% looks like a sequence). If 
% 1) The goal succeeds, but the solver knows there can be more solutions
% 2) At the disjunction ; point
% The solver creates a waypoint -- when it has solved it's current route
% it backtracks to the last waypoint and 
% 1) goes again with the next solution
% 2) if failed, goes to the other branch
%
% A goal, as has been said, codes a procedure call. If it's an intrinsic,
% it just makes the solver do something specific (like a specform in lisp).
%
% If it is a regular procedure, the solver tries to do the following:
% It tries to unify (=) the text of the goal with the head of every clause
% of the given procedure.
% If the unification succeeds, it calculates the tail (if there is no tail,
% it's just success). Each time it starts the calc, a waypoint is created --
% if calc fails, it backtracks and tries other clauses. If the calling calc
% also fails -- it will try other clauses too (it may try other solutions,
% as unification with free vars can yield different solutions).
%
% Only if unification with all heads of clauses fails, then it's a fail.

% tail only
:- initialization(main). % main is just a predicate
:- op(1000, xfx, has). % 1000 is op prio, xfx -- expr description

% compound terms at 'top level' mean 'fact declarations'
% head only
parent("Mary", "Ann").
parent("Mary", "John").
parent("George", "John").
parent("Hamish", "George").
parent("Hannah", "Mary").

female("Ann").
female("Mary").

% This means that the prolog solver now knows that the parent relation is
% true for these people. Now parent/2 is a predicate/prolog-procedure

% facts are fixed statements of a relation. We can also make rules

% head and tail
%
% when calculating the tail, unification makes it so the free vars in
% the head get bound to the stuff in lhs (if lhs has free vars too, rhs
% ones are bound as an alias). During tail calculation, vars that are still
% free may __get_values__.
loves(X, Y) :- parent(X, Z), parent(Y, Z).
loves("Don Juan", X) :- female(X).

% here Z is 'anything', and the prolog solver will go over variants.
% rules can also look like facts, but just have free vars (meaning 'anything')

loves("Crow", _). % _ is a spec var name for anon vars, can be used many times
                  % see, C++?

% can be recursive
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).

% here main is a predicate that succeeds if all the goals separated by ,
% succeed, and some of them have side effects. the initialization(main)
% top-level goal makes the interpreter calc main as the program (a quirk of
% swipl)
main :-
    on_signal(int, _, default),
    prompt(_, ''),
    % current_prolog_flag(argv, Argv),

    % data in prolog is also heterogeneous dynamic stuff, akin to lisp s-exprs
    % any correct prolog expr is called a 'term'
    %
    % Data can be numeric (ints, floats) and so-called atoms, which are both
    % like symbols and strings (hello world in hello.pl is a atom)
    
    write(hello),
    write('hello, man\n'), % same, and argv is an atom too

    % atoms can be associated with predicates (procedures), and do other symbol
    % like stuff.

    % Another atomic data obj is the empty list []

    % Prolog also has strings separately from atoms

    % more complex data structures (compound terms) have an atom as 'name'
    % (called 'main functor') and a list of arguments, shich are like records
    write(date(2029, may, 31)),
    write(person('John Doe', m, 1978)),
    write(place(n, 54.8, e, 37.2)), nl,

    % number of args in a compound term is it's arity

    % this stuff can be nested. Here there are 2 functors -- point and circle
    % but only circle is considered main.
    write(circle(point(0, 0), 1)), nl, % there can be no space before (

    % a 'type' is considered atom + arity

    % this is also valid
    write('This is a term'(1, 2, 0)),
    nl,

    % two-arg terms can be written in a spec form if a built in target op has
    % been applied (up the text)

    write(a has b), nl,
    write(has(a, b)), nl, % same

    % arifm are terms defined as operators
    write(X is 2 * 2), nl,
    write(X is *(2, 2)), nl, % same

    % actually, everything is a term followed by a .
    % and , :- ; etc are just functors that work like operators (, has a spec
    % role though, namely functor arg separator, or list literal arg separator)

    % Prolog lists are also made with dotted pairs
    write([1|2]), % also used to be .(1, 2) or 1 . 2
    write('[|]'(1, 2)), % same lol
    write([1, 2, 3|4]),
    write([1, 2, 3|[]]), % is [1, 2, 3]
    nl,

    % strings that are not symbols are written in "
    % Before a certain version they were just char code lists.
    % This is an atomic type, not a symbol
    write("Hello, world\n"),

    % now char code list is this
    write(`Hello, world\n`), nl,

    % this can be changed w/ set_prolog_flag though

    % So data is atoms % compound terms with main functor an args --
    % and dotted pairs => lists are a case of compound terms.

    % variables in prolog are symbols with associated values. Each var is
    % either free or bound at a given time -- there is no assignment.
    %
    % var names can start with either a capital letter or a _
    % binding can happen via 'term unification'

    % term unification is the operator-functor =, which is like a logical
    % statement, but does some special stuff
    =(1, 1),
    1 = 1, % same
    % 1 = 2, fails

    % when any side of unification is a free var, it is bound to the other side
    %
    X = 2, 
    X = Y,
    write(X), nl,
    write(Y), nl,

    % if both are free, the second is bound to the first, the first being
    % it's new val.
    A = B,
    A = 1,
    write(B), nl,

    % for compound terms unification fails if their main functor or arity
    % differs, unification fails. Otherwise, it is applied recursively
    point(P, R) = point(3, 1),
    % a(P, R) = b(P, R) fails
    % a(P, R) = a(P) fails
    write(P), nl,
    write(R), nl,

    % Later I'll see how a query can give more than one answer -- for such
    % queries unification still one solution. Idk how yet.

    % variables can also be bound to compound terms
    XX = f(YY),
    write(XX), nl,

    % It can even get a self referential binding -- this is an infinite f(f(...
    XXX = f(XXX),
    write(XXX), nl,

    % a cyclic list can be done this way
    XXXX = [1, 2|XXXX],
    write(XXXX), nl,

    % with facts we can now check
    parent("Mary", "Ann"),
    % parent("Ann", "Mary"), fails
    Person = "John",
    parent("George", Person),

    loves("Crow", Person),
    loves("Mary", "George"),
    % loves("Mary", "John"), fails

    % this means 'all ancestors, backtrack while there are some left'
    % this is called 'predicate inversion', which means to turn a predicate
    % into a sequence of concrete values.
    ancestor(FreeVar, "John"),

    halt.
