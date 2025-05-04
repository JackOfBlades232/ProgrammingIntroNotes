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

is_knight_dxdy(DX, DY) :-
    absval(DX, ADX), absval(DY, ADY),
    (ADX =:= 2, ADY =:= 1; ADX =:= 1, ADY =:= 2).

base_available_turns(X, Y, N, M, Dest) :-
    inrange(TX, N), inrange(TY, M), (TX =\= 1; TY =\= 1),
    DX is TX - X, DY is TY - Y, is_knight_dxdy(DX, DY),
    Dest = pos(TX, TY).

to_visit(N, M, Dest) :-
    inrange(X, N), inrange(Y, M), (X =\= 1; Y =\= 1),
    findall(O, base_available_turns(X, Y, N, M, O), TurnsList),
    uniq(TurnsList, TurnsListUnique), length(TurnsListUnique, Turns),
    Dest = square(Turns, pos(X, Y)).

delsq(_, [], Out) :- !, Out = [].
delsq(E, [E|Tail], Out) :- !, delsq(E, Tail, Out).
delsq(E, [H|Tail], Out) :-
    square(_, pos(EX, EY)) = E, square(HTurns, pos(HX, HY)) = H,
    DX is EX - HX, DY is EY - HY, is_knight_dxdy(DX, DY),
    delsq(E, Tail, PrunedTail), NTurns is HTurns - 1,
    Out = [square(NTurns, pos(HX, HY))|PrunedTail].
delsq(E, [H|Tail], Out) :-
    delsq(E, Tail, PrunedTail), Out = [H|PrunedTail].

turn_is_ok(pos(X, Y), Dest, ToVisit, Remaining) :-
    has(Dest, ToVisit),
    square(_, pos(DestX, DestY)) = Dest,
    DX is DestX - X, DY is DestY - Y, is_knight_dxdy(DX, DY),
    delsq(Dest, ToVisit, RemainingUnsorted),
    sort(RemainingUnsorted, Remaining).

solution_is_valid([]) :- !.
solution_is_valid([_]) :- !.
solution_is_valid([A, B|T]) :- 
    pos(AX, AY) = A, pos(BX, BY) = B, DX is AX - BX, DY is AY - BY,
    is_knight_dxdy(DX, DY), solution_is_valid([B|T]).
solution_is_valid(Sol, N, M) :-
    uniq(Sol, US), length(Sol, LS), length(US, LU), LB is N * M,
    LS =:= LU, LS =:= LB, solution_is_valid(Sol).

open_solve(_, [], Sol) :- !, Sol = [].
open_solve(Pos, ToVisit, Sol) :-
    turn_is_ok(Pos, Dest, ToVisit, Remaining),
    square(_, DPos) = Dest,
    open_solve(DPos, Remaining, RemainingSol), !,
    Sol = [DPos|RemainingSol].

print_sol([]) :- nl.
print_sol([pos(X, Y)|Tail]) :-
    write(X), write(','), write(Y), write(' '), print_sol(Tail).

find_open_path(N, M) :-
    findall(Dest, to_visit(N, M, Dest), ToVisitUnsorted),
    sort(ToVisitUnsorted, ToVisit),
    open_solve(pos(1, 1), ToVisit, Sol),
    CSol = [pos(1, 1)|Sol], solution_is_valid(CSol, N, M),
    print_sol([pos(1, 1)|Sol]).

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
                    write('Closed path: TODO\n'), find_open_path(N, M)
                )    
            );
            write('Invalid usage: N <= M required\n')
        );
        write('Invalid usage: prog [N] [M]\n')
    ),
    
    halt.

