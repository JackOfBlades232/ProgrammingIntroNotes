#!/usr/bin/swipl -q

:- initialization(main).
:- op(1000, xfx, has). % 1000 is op prio, xfx -- expr description

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
    write(place(n, 54.8, e, 37.2)),
    nl,

    % number of args in a compound term is it's arity

    % this stuff can be nested. Here there are 2 functors -- point and circle
    % but only circle is considered main.
    write(circle(point(0, 0), 1)), % there can be no space before (
    nl,

    % a 'type' is considered atom + arity

    % this is also valid
    write('This is a term'(1, 2, 0)),
    nl,

    % two-arg terms can be written in a spec form if a built in target op has
    % been applied (up the text)

    write(a has b),
    nl,
    write(has(a, b)), % same
    nl,

    % arifm are terms defined as operators
    write(X is 2 * 2),
    nl,
    write(X is *(2, 2)), % same
    nl,

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
    write(`Hello, world\n`),
    nl,

    % this can be changed w/ set_prolog_flag though

    % So data is atoms % compound terms with main functor an args --
    % and dotted pairs => lists are a case of compound terms. But what is
    % a prolog program, and what is that 'is' thing?

    halt.
