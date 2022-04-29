#!/usr/bin/swipl -q

:- initialization(main).

handle(_, -1, _) :- !.
handle(Stream, 10, N) :- !, write(N), nl, reading(Stream, 0).
handle(Stream, _, N) :- N1 is N+1, reading(Stream, N1).

reading(Stream, N) :- get0(Stream, C), !, handle(Stream, C, N).

err(error(E, C)) :- !,
    E =.. [ErrorReason|_], C =.. [_, _, Message],
    write(user_error, ErrorReason),
    write(user_error, ' ('),
    write(user_error, Message),
    write(user_error, ')\n'),
    halt.
err(Ex) :-
    write(user_error, 'Exception unknown: '),
    write(user_error, Ex),
    nl(user_error),
    halt.

process_file_on(Fname) :-
    open(Fname, read, Stream, [encoding(octet)]),
    reading(Stream, 0).

process_file(Fname) :- catch(process_file_on(Fname), Ex, err(Ex)).

main :-
    on_signal(int, _, default),
    prompt(_, ''),
    current_prolog_flag(argv, Argv),
    (Argv = [Fname|_], process_file(Fname) ; reading(user_input, 0)),
    halt.
