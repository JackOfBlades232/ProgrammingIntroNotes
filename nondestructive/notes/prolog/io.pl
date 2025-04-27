#!/usr/bin/swipl -q

:- initialization(main).

% There are ways to read/write prolog terms/expressions. Only useful -- write
% This writes to stdout as a side effect, and succeeds. nl -- new line.
% There is also by-symbol input.

% get reads unicode codes irrespective of locale. For that get_byte can be
% used to get raw bytes. For that, io has to be made bytewise. But then
% get0 will also read bytes, so no need for get_byte
:- set_stream(current_input, encoding(octet)).
:- set_stream(current_output, encoding(octet)).

% user_*** stuff can be redirected with
:- set_stream(current_output, alias(user_output)).

main :-
    on_signal(int, _, default),
    prompt(_, ''),

    write(funct(arg1, arg2)),
    nl,

    get(X), % char code from stdin, skipping ws (why?) -- unifies with free var
    get0(Y), % same, but normal -- no ws skip
    put(X), % writes char to stdout
    put(Y),

    % eof is -1

    get_byte(Z),
    put_byte(Z),

    get_char(W), % instead of char code this makes an atom of this char
    put_char(W), % and prints one-char items.

    % on eof get_char returns 'end_of_file' atom

    nl,

    % for file IO there are two styles -- first is 'Edinborough one'. In it,
    % there is the notion of current file. To open and set this file (or
    % get back to an already opened file) see predicate is used

    see('hello.pl'), % weird -- takes an _atom_

    % now let's read using repeat -- always gives success, allowing cycles until
    % cut inside prolog procedural semantic
    repeat, get0(A), (A = -1; put(A), false), !,

    seen, % closes current file chosen by see

    % For output it's tell or append, and told at end.
    % To query (ie check if it is current or bind free var) cur i and o stream
    % seeing/telling is used. In this example output is saved and then
    % restored.
    
    telling(Save),
    append('out'), write('Hello, world\n'), told,
    tell(Save),

    % This has two problems -- single 'state' and only identifying by names
    % That, and error handling is problematic for Edinborough-style predicates
    
    % The second mode is 'ISO mode', more like standard posix streams
    % In this mode streams are idenified by stream objects.
    %
    % current_input and current_output are those, along with user_input,
    % user_output and user_error.
    %
    % user_ are just std streams. current_ are initially same, but are changed
    % by see/tell/append and are used by no-arg predicates

    % This stuff does not change atoms, their 'values' (atoms don't have values
    % btw, only vars which are separate), but a special association table
    % between these atoms and stream objects

    % all the io predicates can take stream as arg 1
    write(user_output, 'Hello, world\n'), % always to stdout
    write(user_error, 'Troube\n'),

    % continued in fileio.pl

    halt.
