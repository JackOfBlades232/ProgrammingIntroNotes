#!/usr/bin/swipl -q
:- initialization(main).

% file is opened with 'open' with 3/4 args
% first -- name, can be atom, string or charcode list. Also can be spec
%   like pipe("ls") -- launch ls and pipe output to stream.
% second -- atoms read/write/append/update -- all regular, update is like
%   write but does not truncate first.
% third -- dest. Either var for unification, or an atom to be bound like
%   current_*** is bound (override is allowed).
% fourth -- list of options, can be omitted
%   (one good idea -- encoding(octet) to read real bytes)
%
% closing happens w/ close, but std streams can't be closed
% (output closing flushes output).
% If an alias was set for user_***, then close will restore initial value.

% Error handling is done with exceptions. Not really prologue-ish,
% but regular fail would not be good -- there isn't a straight way
% to get info about the error cause binding does not happen on fail.

% catch gets the error and dispatches to a provided predicate.
% The error is a compound term of the form
% error(existence_error(source_sink, 'smth.txt),
%   context(system:open/3, 'No such file or directory'))

% Error types are not thoroughly documented

% Lets do the example from scheme and lisp io
% Reading will be done with recursive predicates handle and reading

handle(_, -1, _) :- !.
handle(Stream, 10, N) :- !, write(N), nl, reading(Stream, 0).
handle(Stream, _, N) :- N1 is N + 1, reading(Stream, N1).

reading(Stream, N) :- get0(Stream, C), !, handle(Stream, C, N).

% Now, error handler

err(error(E, C)) :- !,
    E =.. [ErrorReason|_], C =.. [_, _, Message], % list unification to unpack
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

% Now make file processor and exception-handling wrapper

process_file_on(Fname) :-
    open(Fname, read, Stream, [encoding(octet)]),
    reading(Stream, 0).
process_file(Fname) :- catch(process_file_on(Fname), Ex, err(Ex)).

% and, do the thing

main :-
    on_signal(int, _, default),
    prompt(_, ''),
    current_prolog_flag(argv, Argv),
    (Argv = [Fname|_],
        process_file(Fname);
        reading(user_input, 0)
    ),
    halt.
