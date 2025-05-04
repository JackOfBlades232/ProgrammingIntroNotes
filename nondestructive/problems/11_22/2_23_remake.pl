#!/usr/bin/swipl -q

:- initialization(main).

isspace(E) :- E = ' '; E = '\t'; E = '\n'; E = '\r'.

count_words(S, N) :- string_chars(S, C), count_words([' '|C], 0, N).
count_words([], A, N) :- N = A, !.
count_words([_], A, N) :- N = A, !.
count_words([C1, C2|Tail], A, N) :-
    isspace(C1), not(isspace(C2)), !,
    A2 is A + 1, count_words([C2|Tail], A2, N).
count_words([_|Tail], A, N) :- count_words(Tail, A, N).

main :-
    on_signal(int, _, default),
    prompt(_, ''),
    current_prolog_flag(argv, Argv),

    (Argv = [Arg], !,
        count_words(Arg, N), write(N), nl;
        write('Invalid usage: prog [arg].\n')
    ),
    
    halt.
