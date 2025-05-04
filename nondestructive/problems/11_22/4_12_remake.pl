#!/usr/bin/swipl -q

:- initialization(main).

isprefix([], [_|_]).
isprefix([H|PT], [H|T]) :- isprefix(PT, T).

substr_count([], _, N, Out) :- Out = N, !.
substr_count([H|T], Needle, N, Out) :-
    isprefix(Needle, [H|T]), !, N2 is N + 1, substr_count(T, Needle, N2, Out).
substr_count([_|T], Needle, N, Out) :- substr_count(T, Needle, N, Out).

print_args(_, []).
print_args(Needle, [H|T]) :-
    string_chars(H, C), string_chars(Needle, CN),
    substr_count(C, CN, 0, N), N > 0, !,
    write('"'), write(H), write('" '), write(N), nl, 
    print_args(Needle, T).
print_args(Needle, [_|T]) :- print_args(Needle, T).

main :-
    on_signal(int, _, default),
    prompt(_, ''),
    current_prolog_flag(argv, Argv),

    ([Needle|Haystacks] = Argv, !,
        print_args(Needle, Haystacks);
        write('Invalid usage: prog [haystack] [needles...]\n')
    ),
    
    halt.
