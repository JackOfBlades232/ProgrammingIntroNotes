#!/usr/bin/swipl -q

% @TODO: finish

:- initialization(main).

has(E, [E|_]).
has(E, [_|Tail]) :- has(E, Tail).
contains(E, [E|_]) :- !.
contains(E, [_|Tail]) :- contains(E, Tail).
none(_, []).
none(E, [X|Tail]) :- E \= X, none(E, Tail).
uniq(L, R) :- uniq(L, R, []).
uniq([], R, R) :- !.
uniq([H|T], R, A) :- none(H, A), !, uniq(T, R, [H|A]).
uniq([_|T], R, A) :- uniq(T, R, A).
listrev(L, R) :- listrev(L, R, []).
listrev([], A, A) :- !.
listrev([H|T], R, A) :- listrev(T, R, [H|A]).
uniq_inorder(L, R) :- uniq(L, U), listrev(R, U).
shuffle_sort(L, R) :- random_permutation(L, T), !, sort(T, R).

same_elems_unique(L, R) :-
    uniq_inorder(L, UL), uniq_inorder(R, UR), L = UL, R = UR,
    sort(L, SL), sort(R, SR), SL = SR.

prefix_matches([H|L], [H|R], I) :- !, prefix_matches(L, R, II), I is II + 1.
prefix_matches(_, _, I) :- I = 0.

% @TODO: get evenness of transposition to decide exact goal

fifteen_solved(Ord) :-
    Ord =
    [
        '1', '2', '3', '4', '5', '6', '7', '8', '9',
        '10', '11', '12', '13', '14', '15', '@'
    ];
    Ord =
    [
        '1', '2', '3', '4', '5', '6', '7', '8', '9',
        '10', '11', '12', '13', '15', '14', '@'
    ].

[1, 2, 5, 9, 3, 6, 4, 13, 10, 7, 8, 14, 11, 12, 15, 16]

apply_turn(Board, FromInd, ToInd, Out) :-
    FromInd < ToInd,
    nth1(ToInd, Board, To, R1),
    nth1(FromInd, R1, From, R2),
    nth1(FromInd, R3, To, R2),
    nth1(ToInd, Out, From, R3).
apply_turn(Board, FromInd, ToInd, Out) :-
    ToInd < FromInd,
    nth1(FromInd, Board, From, R1),
    nth1(ToInd, R1, To, R2),
    nth1(ToInd, R3, From, R2),
    nth1(FromInd, Out, To, R3).

turns(FI, TI) :- FI0 is FI - 1, FI0 mod 4 < 3, TI is FI + 1.
turns(FI, TI) :- FI0 is FI - 1, FI0 div 4 < 3, TI is FI + 4.
turns(FI, TI) :- FI0 is FI - 1, FI0 mod 4 > 0, TI is FI - 1.
turns(FI, TI) :- FI0 is FI - 1, FI0 div 4 > 0, TI is FI - 4.

dist(From, To, D) :-
    FC is (From - 1) mod 4, FR is (From - 1) div 4,
    TC is (To - 1) mod 4, TR is (To - 1) div 4,
    D is abs(FC - TC) + abs(FR - TR).

dist_to_dest_fifteen(Elem, Ind, Out) :-
    (Elem = '14'; Elem = '15'), !,
    dist(Ind, 14, ZD), dist(Ind, 15, OD), Out is min(ZD, OD).
dist_to_dest_fifteen(Elem, Ind, Out) :-
    Elem = '@', !, dist(Ind, 16, Out).
dist_to_dest_fifteen(Elem, Ind, Out) :-
    atom_number(Elem, N), dist(Ind, N, Out).

score_fifteen(Board, Score) :- score_fifteen(Board, 1, Score),
    write('Score='), write(Score), nl, print_board(Board), nl.
score_fifteen([H|T], I, Acc) :- 
    dist_to_dest_fifteen(H, I, D), S is (17 - I) * (6 - D),
    II is I + 1, score_fifteen(T, II, IAcc), Acc is IAcc + S.
score_fifteen([], _, Acc) :- Acc is 0.

fifteen_turns(FromInd, Board, ToInd, NewBoard, Score, Visited) :-
    turns(FromInd, ToInd),
    ToInd > PrefLen,
    apply_turn(Board, FromInd, ToInd, NewBoard),
    not(contains(NewBoard, Visited)),
    score_fifteen(NewBoard, Score).

init_pos(L, I) :- init_pos(L, 1, I).
init_pos([H|_], N, I) :- H = '@', !, I = N.
init_pos([_|T], N, I) :- NN is N + 1, init_pos(T, NN, I).

print_board([]) :- !.
print_board([X, Y, Z, W|T]) :-
    write(X), write(' '), write(Y), write(' '),
    write(Z), write(' '), write(W), nl, print_board(T).

solve_fifteen(Pos, Board, Visited, Sol) :-
    findall(
        turn(Score, To, NewBoard),
        fifteen_turns(Pos, Board, To, NewBoard, Score, Visited),
        TurnsUnsorted),
    shuffle_sort(TurnsUnsorted, Turns),
    has(Turn, Turns), turn(_, To, NB) = Turn,
    (
        Pos = 16, fifteen_solved(NB), !, Sol = [To];
        solve_fifteen(To, NB, [NB|Visited], RT), !, Sol = [To|RT]
    ).

print_sol([_]) :- !.
print_sol([From, To|T]) :-
    write(From), write(' <-> '), write(To), nl, print_sol([To|T]).

do_fifteen(Init) :-
    init_pos(Init, Pos),
    solve_fifteen(Pos, Init, [Init], Sol),
    print_sol(Sol).

do_thirteen(_) :- write('Thirteen: TODO\n'), fail.

main :-
    on_signal(int, _, default),
    prompt(_, ''),
    current_prolog_flag(argv, Argv),

    (
        same_elems_unique(Argv,
            [
                '1', '2', '3', '4', '5', '6', '7', '8', '9',
                '10', '11', '12', '13', '14', '15', '@'
            ]), !,
            do_fifteen(Argv);

        same_elems_unique(Argv,
            [
                '0', '00', '1', '2', '3', '4', '5', '6', '7',
                '8', '9', '10', '11', '12', '13', '@'
            ]), !,
            do_thirteen(Argv);

        write('Wrong input: use a permutation of [1-15] and @ for "fifteen", '),
        write('and [0-13], 00 and @ for "thirteen"\n')
    ),


    halt.

