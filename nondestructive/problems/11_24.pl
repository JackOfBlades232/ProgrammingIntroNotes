#!/usr/bin/swipl -q

:- initialization(main).

inrange(1, _).
inrange(I, R) :- inrange(II, R), ((II >= R, !, fail); I is II + 1).

absval(X, Y) :- X < 0, Y = -X.
absval(X, Y) :- X >= 0, Y = X.
minval(X, Y, M) :- X < Y, M = X.
minval(X, Y, M) :- X >= Y, M = Y.

has(E, [E|_]).
has(E, [_|Tail]) :- has(E, Tail).
none(_, []).
none(E, [X|Tail]) :- E \= X, none(E, Tail).
uniq(L, R) :- uniq(L, R, []).
uniq([], R, R) :- !.
uniq([H|T], R, A) :- none(H, A), !, uniq(T, R, [H|A]).
uniq([_|T], R, A) :- uniq(T, R, A).
empty([]).
first([A|_], A) :- !.
last([A], A) :- !.
last([_|T], A) :- last(T, A).

choice(L, E) :- random_permutation(L, T), !, has(E, T).
shuffle_sort(L, R) :- random_permutation(L, T), !, sort(T, R).

is_knight_dxdy(DX, DY) :-
    absval(DX, ADX), absval(DY, ADY),
    (ADX =:= 2, ADY =:= 1; ADX =:= 1, ADY =:= 2).
is_knight_turn(From, To) :-
    pos(FX, FY) = From, pos(TX, TY) = To,
    DX is TX - FX, DY is TY - FY, is_knight_dxdy(DX, DY).

base_available_turns(X, Y, N, M, Dest) :-
    inrange(TX, N), inrange(TY, M),
    is_knight_turn(pos(X, Y), pos(TX, TY)), Dest = pos(TX, TY).

to_visit(N, M, Dest) :-
    inrange(X, N), inrange(Y, M),
    findall(O, base_available_turns(X, Y, N, M, O), TurnsList),
    uniq(TurnsList, TurnsListUnique), length(TurnsListUnique, Turns),
    Dest = square(Turns, pos(X, Y)).

delsq(_, [], Out) :- !, Out = [].
delsq(E, [E|Tail], Out) :- !, delsq(E, Tail, Out).
delsq(E, [H|Tail], Out) :-
    square(_, pos(EX, EY)) = E, square(HTurns, pos(HX, HY)) = H,
    DX is EX - HX, DY is EY - HY, is_knight_dxdy(DX, DY), !,
    delsq(E, Tail, PrunedTail), NTurns is HTurns - 1,
    Out = [square(NTurns, pos(HX, HY))|PrunedTail].
delsq(E, [H|Tail], Out) :-
    delsq(E, Tail, PrunedTail), Out = [H|PrunedTail].

turn_is_ok(Pos, Dest, ToVisit, Remaining) :-
    has(Dest, ToVisit),
    square(_, DPos) = Dest, is_knight_turn(Pos, DPos),
    delsq(Dest, ToVisit, RemainingUnsorted),
    shuffle_sort(RemainingUnsorted, Remaining).

open_solve(_, [], Sol) :- !, Sol = [].
open_solve(Pos, ToVisit, Sol) :-
    turn_is_ok(Pos, Dest, ToVisit, Remaining),
    square(_, DPos) = Dest,
    open_solve(DPos, Remaining, RemainingSol), !,
    Sol = [DPos|RemainingSol].

closed_solve(Pos, [], Init, Sol) :- !, is_knight_turn(Pos, Init), Sol = [].
closed_solve(Pos, ToVisit, Init, Sol) :-
    turn_is_ok(Pos, Dest, ToVisit, Remaining),
    square(_, DPos) = Dest,
    closed_solve(DPos, Remaining, Init, RemainingSol), !,
    Sol = [DPos|RemainingSol].

solution_is_valid([_]) :- !.
solution_is_valid([A, B|T]) :- is_knight_turn(A, B), solution_is_valid([B|T]).
solution_is_valid(Sol, N, M) :-
    uniq(Sol, US), length(Sol, LS), length(US, LU), LB is N * M,
    LS =:= LU, LS =:= LB, solution_is_valid(Sol).

solution_is_valid_and_closed(Sol, N, M) :-
    solution_is_valid(Sol, N, M),
    first(Sol, H), last(Sol, T), is_knight_turn(T, H).

print_sol([]) :- nl.
print_sol([pos(X, Y)|Tail]) :-
    write(X), write(','), write(Y), write(' '), print_sol(Tail).

find_open_path(N, M) :-
    findall(Dest, to_visit(N, M, Dest), ToVisitInit),
    choice(ToVisitInit, Init), delsq(Init, ToVisitInit, ToVisitUnsorted),
    shuffle_sort(ToVisitUnsorted, ToVisit), square(_, InitPos) = Init,
    open_solve(InitPos, ToVisit, Sol), !,
    CSol = [InitPos|Sol], solution_is_valid(CSol, N, M),
    print_sol(CSol).

find_closed_path(N, M) :-
    findall(Dest, to_visit(N, M, Dest), ToVisitInit),
    choice(ToVisitInit, Init), delsq(Init, ToVisitInit, ToVisitUnsorted),
    shuffle_sort(ToVisitUnsorted, ToVisit), square(_, InitPos) = Init,
    closed_solve(InitPos, ToVisit, InitPos, Sol), !,
    CSol = [InitPos|Sol], solution_is_valid_and_closed(CSol, N, M),
    print_sol(CSol).

main :-
    on_signal(int, _, default),
    prompt(_, ''),
    current_prolog_flag(argv, Argv),

    ([ArgN, ArgM] = Argv, atom_number(ArgN, N), atom_number(ArgM, M), !,
        (N =< M,
            ((N =< 2; N =:= 3, (M =:= 3; M =:= 5; M =:= 6); N =:= 4, M =:= 4),
                write('No solutions\n');
                ((N mod 2 =:= 1, M mod 2 =:= 1; N =:= 4;
                    N =:= 3, (M =:= 4; M =:= 8)), !,
                    
                    write('Open path:\n'), find_open_path(N, M);
                    write('Closed path:\n'), find_closed_path(N, M)
                )    
            );
            write('Invalid usage: N <= M required\n')
        );
        write('Invalid usage: prog [N] [M]\n')
    ),
    
    halt.

