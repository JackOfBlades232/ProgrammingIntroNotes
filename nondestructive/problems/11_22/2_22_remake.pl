#!/usr/bin/swipl -q

:- initialization(main).

none(_, []).
none(E, [X|Tail]) :- E \= X, none(E, Tail).

samelen([], []).
samelen([_|LTail], [_|RTail]) :- samelen(LTail, RTail).

uniq(L, R) :- uniq(L, R, []).
uniq([], R, R) :- !.
uniq([H|T], R, A) :- none(H, A), !, uniq(T, R, [H|A]).
uniq([_|T], R, A) :- uniq(T, R, A).

isuniq(S) :- uniq(S, U), samelen(S, U).

one(E, [E|Tail]) :- !, none(E, Tail).
one(E, [_|Tail]) :- one(E, Tail).
has(E, [E|_]) :- !.
has(E, [_|Tail]) :- has(E, Tail).

digit(C) :- has(C, ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0']).
isdigits([E|[]]) :- !, digit(E).
isdigits([E|Tail]) :- digit(E), isdigits(Tail).

same([]).
same([_]).
same([X|[Y|Tail]]) :- X = Y, same([Y|Tail]).

latin(C) :- char_code(C, Cd), (Cd =< 90, Cd >= 65; Cd =< 132, Cd >= 97).
haslatin([E|_]) :- latin(E), !.
haslatin([_|Tail]) :- haslatin(Tail).

hascommon([E|_], L) :- has(E, L), !.
hascommon([_|Tail], L) :- hascommon(Tail, L).

print_longest(L) :- print_longest(L, "", 0).
print_longest([], Cur, _) :- write(Cur), nl.
print_longest([X|Tail], _, CurLen) :-
    string_length(X, Len), Len > CurLen, !, print_longest(Tail, X, Len).
print_longest([_|Tail], Cur, CurLen) :- print_longest(Tail, Cur, CurLen).

print_unique([]).
print_unique([S|Tail]) :-
    string_chars(S, C), isuniq(C), !, write(S), nl, print_unique(Tail).
print_unique([_|Tail]) :- print_unique(Tail).

print_at_and_dot([]).
print_at_and_dot([S|Tail]) :-
    string_chars(S, C),
    one('@', C), has('.', C), !,
    write(S), nl,
    print_at_and_dot(Tail).
print_at_and_dot([_|Tail]) :- print_at_and_dot(Tail).

print_digits([]).
print_digits([S|Tail]) :-
    string_chars(S, C), isdigits(C), !, write(S), nl, print_digits(Tail).
print_digits([_|Tail]) :- print_digits(Tail).

print_samechar([]).
print_samechar([S|Tail]) :-
    string_chars(S, C), same(C), !, write(S), nl, print_samechar(Tail).
print_samechar([_|Tail]) :- print_samechar(Tail).

print_haslatin([]).
print_haslatin([S|Tail]) :-
    string_chars(S, C), haslatin(C), !, write(S), nl, print_haslatin(Tail).
print_haslatin([_|Tail]) :- print_haslatin(Tail).

print_hascommon([], _).
print_hascommon([S|Tail], With) :-
    string_chars(S, C), string_chars(With, CWith),
    hascommon(C, CWith), !,
    write(S), nl,
    print_haslatin(Tail).
print_hascommon([_|Tail], With) :- print_hascommon(Tail, With).

print_hascommon_with_first([]).
print_hascommon_with_first([_]).
print_hascommon_with_first([H|T]) :- print_hascommon(T, H).

main :-
    on_signal(int, _, default),
    prompt(_, ''),
    current_prolog_flag(argv, Argv),

    write(Argv), nl,

    write("A: "), nl,
    print_longest(Argv),

    write("B: "), nl,
    print_unique(Argv),

    write("C: "), nl,
    print_at_and_dot(Argv),

    write("D: "), nl,
    print_digits(Argv),

    write("E: "), nl,
    print_samechar(Argv),

    write("F: "), nl,
    print_haslatin(Argv),

    write("G: "), nl,
    print_hascommon_with_first(Argv),
    
    halt.
