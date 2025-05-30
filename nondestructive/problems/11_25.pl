#!/usr/bin/swipl -q

% @TODO(opt): make it run in adequate time for more boards
%             with analytical pre-solution up to 3x3 board
% It is quite tedious to do in prolog, so I won't consider it required

use_module(library(heaps)).

:- initialization(main).

inversions_with(_, [], Out) :- Out is 0.
inversions_with(E, [H|T], Out) :- inversions_with(E, T, OutWithout),
    (E > H, !, Out is OutWithout + 1; Out is OutWithout).

inversion_count([], Out) :- Out is 0.
inversion_count([H|T], Out) :- inversion_count(T, OutWithout),
    inversions_with(H, T, OutWith), Out is OutWith + OutWithout.

even_permutation(L, Add) :-
    inversion_count(L, Count), (Count + Add) mod 2 =:= 0.

code_list_to_board(
    [E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14, E15, E16],
    board(row(E1, E2, E3, E4), row(E5, E6, E7, E8), row(E9, E10, E11, E12),
        row(E13, E14, E15, E16))).

board_row(board(R, _, _, _), 1, Row) :- Row = R.
board_row(board(_, R, _, _), 2, Row) :- Row = R.
board_row(board(_, _, R, _), 3, Row) :- Row = R.
board_row(board(_, _, _, R), 4, Row) :- Row = R.

row_at(row(E, _, _, _), 1, Elem) :- Elem = E.
row_at(row(_, E, _, _), 2, Elem) :- Elem = E.
row_at(row(_, _, E, _), 3, Elem) :- Elem = E.
row_at(row(_, _, _, E), 4, Elem) :- Elem = E.

board_row_set(board(_, A, B, C), board(Row, A, B, C), 1, Row).
board_row_set(board(A, _, B, C), board(A, Row, B, C), 2, Row).
board_row_set(board(A, B, _, C), board(A, B, Row, C), 3, Row).
board_row_set(board(A, B, C, _), board(A, B, C, Row), 4, Row).
row_set(row(_, A, B, C), row(Elem, A, B, C), 1, Elem).
row_set(row(A, _, B, C), row(A, Elem, B, C), 2, Elem).
row_set(row(A, B, _, C), row(A, B, Elem, C), 3, Elem).
row_set(row(A, B, C, _), row(A, B, C, Elem), 4, Elem).

board_at(B, RI, EI, Elem) :- board_row(B, RI, Row), row_at(Row, EI, Elem).
board_set(B, OB, RI, EI, Elem) :- 
    board_row(B, RI, R), row_set(R, NR, EI, Elem), board_row_set(B, OB, RI, NR).

apply_turn_to_board(Board, OutBoard, ERI, EEI, RI, EI) :-
    board_at(Board, RI, EI, Subs),
    board_set(Board, OTB, ERI, EEI, Subs),
    board_set(OTB, OutBoard, RI, EI, 16).

turns_from(RI, EI, RMin, _, _, _, ORI, OEI) :-
    RI > RMin, ORI is RI - 1, OEI is EI.
turns_from(RI, EI, _, EMin, _, _, ORI, OEI) :-
    EI > EMin, ORI is RI, OEI is EI - 1.
turns_from(RI, EI, _, _, RMax, _, ORI, OEI) :-
    RI < RMax, ORI is RI + 1, OEI is EI.
turns_from(RI, EI, _, _, _, EMax, ORI, OEI) :-
    EI < EMax, ORI is RI, OEI is EI + 1.

turns_from(Board, OutBoard, ORI, OEI) :-
    board_at(Board, RI, EI, 16),
    turns_from(RI, EI, 1, 1, 4, 4, ORI, OEI),
    apply_turn_to_board(Board, OutBoard, RI, EI, ORI, OEI).

diff_board_score_at(From, To, RI, EI, S) :-
    board_at(From, RI, EI, F), board_at(To, RI, EI, T),
    (F =\= T, T =\= 16, !, S = 1; S = 0).

diff_board_score(From, To, 4, 4, S) :- !,
    diff_board_score_at(From, To, 4, 4, S).
diff_board_score(From, To, RI, EI, S) :-
    diff_board_score_at(From, To, RI, EI, Addition),
    (EI =:= 4, !, NRI is RI + 1, NEI is 1; NRI is RI, NEI is EI + 1),
    diff_board_score(From, To, NRI, NEI, Rest), S is Rest + Addition.
diff_board_score(From, To, S) :- diff_board_score(From, To, 1, 1, S).

heuristic(From, To, S) :- diff_board_score(From, To, S).
score(From, To, Depth, S) :-
    heuristic(From, To, H), S is Depth + H.

expand(node(Board, Depth, Trail), Closed, Final, Children) :-
    findall(
        node(Child, CDepth, CScore, [CTurn|Trail]),
        (
            CDepth is Depth + 1,
            turns_from(Board, Child, ORI, OEI),
            not(member(Child, Closed)),
            score(Child, Final, CDepth, CScore),
            CTurn = turn(ORI, OEI)
        ),
        Children).

insert_all_into_open([], Open, Out) :- Out = Open.
insert_all_into_open([NH|NT], Open, Out) :-
    NH = node(B, D, Score, T),
    add_to_heap(Open, Score, node(B, D, T), SOut),
    insert_all_into_open(NT, SOut, Out).

search([node(Final, _, _, Soln)|_], _, Final, Soln) :- !.
search(Open, Closed, Final, Soln) :-
    get_from_heap(Open, _, H, T),
    (
        H = node(Final, _, Soln), !;

        expand(H, Closed, Final, Children),
        insert_all_into_open(Children, T, N),
        search(N, [H|Closed], Final, Soln)
    ).

solve(Init, Final, Soln) :-
    score(Init, Final, 0, InitScore),
    singleton_heap(InitOpen, InitScore, node(Init, 0, [])),
    search(InitOpen, [], Final, Soln).

print_soln([]) :- nl.
print_soln([turn(X, Y)|T]) :-
    write(X), write(','), write(Y), nl,
    print_soln(T).

list_is_fifteen(L) :-
    Elems =
    [
        '1', '2', '3', '4', '5', '6', '7', '8', '9',
        '10', '11', '12', '13', '14', '15', '@'
    ],
    sort(Elems, ElemsSorted), sort(L, LSorted),
    ElemsSorted = LSorted.

atom_to_code_fifteen('@', 16) :- !.
atom_to_code_fifteen(Atom, Code) :- atom_number(Atom, Code).

list_to_board_fifteen(
    [E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14, E15, E16],
    board(row(C1, C2, C3, C4), row(C5, C6, C7, C8), row(C9, C10, C11, C12),
        row(C13, C14, C15, C16))
) :-
    atom_to_code_fifteen(E1, C1),
    atom_to_code_fifteen(E2, C2),
    atom_to_code_fifteen(E3, C3),
    atom_to_code_fifteen(E4, C4),
    atom_to_code_fifteen(E5, C5),
    atom_to_code_fifteen(E6, C6),
    atom_to_code_fifteen(E7, C7),
    atom_to_code_fifteen(E8, C8),
    atom_to_code_fifteen(E9, C9),
    atom_to_code_fifteen(E10, C10),
    atom_to_code_fifteen(E11, C11),
    atom_to_code_fifteen(E12, C12),
    atom_to_code_fifteen(E13, C13),
    atom_to_code_fifteen(E14, C14),
    atom_to_code_fifteen(E15, C15),
    atom_to_code_fifteen(E16, C16).

final_board_fifteen(InitBoard, FinalBoard) :-
    board_at(InitBoard, RI, EI, 16), Add is (4 - RI) + (4 - EI),
    code_list_to_board(InitCodes, InitBoard),
    (even_permutation(InitCodes, Add), !,
        FinalBoard = board(
            row(1, 2, 3, 4),
            row(5, 6, 7, 8),
            row(9, 10, 11, 12),
            row(13, 14, 15, 16));
        FinalBoard = board(
            row(1, 2, 3, 4),
            row(5, 6, 7, 8),
            row(9, 10, 11, 12),
            row(13, 15, 14, 16))
    ).

list_is_thirteen(L) :-
    Elems =
    [
        '1', '2', '3', '4', '5', '6', '7', '8', '9',
        '10', '11', '12', '13', '0', '00', '@'
    ],
    sort(Elems, ElemsSorted), sort(L, LSorted),
    ElemsSorted = LSorted.

atom_to_code_thirteen('@', 16) :- !.
atom_to_code_thirteen('00', 1) :- !.
atom_to_code_thirteen(Atom, Code) :-
    atom_number(Atom, BCode), Code is BCode + 2.

list_to_board_thirteen(
    [E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14, E15, E16],
    board(row(C1, C2, C3, C4), row(C5, C6, C7, C8), row(C9, C10, C11, C12),
        row(C13, C14, C15, C16))
) :-
    atom_to_code_thirteen(E1, C1),
    atom_to_code_thirteen(E2, C2),
    atom_to_code_thirteen(E3, C3),
    atom_to_code_thirteen(E4, C4),
    atom_to_code_thirteen(E5, C5),
    atom_to_code_thirteen(E6, C6),
    atom_to_code_thirteen(E7, C7),
    atom_to_code_thirteen(E8, C8),
    atom_to_code_thirteen(E9, C9),
    atom_to_code_thirteen(E10, C10),
    atom_to_code_thirteen(E11, C11),
    atom_to_code_thirteen(E12, C12),
    atom_to_code_thirteen(E13, C13),
    atom_to_code_thirteen(E14, C14),
    atom_to_code_thirteen(E15, C15),
    atom_to_code_thirteen(E16, C16).

final_board_thirteen(InitBoard, FinalBoard) :-
    board_at(InitBoard, RI, EI, 16), Add is (4 - RI) + (4 - EI),
    code_list_to_board(InitCodes, InitBoard),
    (even_permutation(InitCodes, Add), !,
        FinalBoard = board(
            row(1, 2, 3, 4),
            row(5, 6, 7, 8),
            row(9, 10, 11, 12),
            row(13, 14, 15, 16));
        FinalBoard = board(
            row(2, 1, 3, 4),
            row(5, 6, 7, 8),
            row(9, 10, 11, 12),
            row(13, 14, 15, 16))
    ).

main :-
    on_signal(int, _, default),
    prompt(_, ''),
    current_prolog_flag(argv, Argv),

    (
        list_is_fifteen(Argv), !,
            list_to_board_fifteen(Argv, Board),
            final_board_fifteen(Board, Final);
        list_is_thirteen(Argv), !,
            list_to_board_thirteen(Argv, Board),
            final_board_thirteen(Board, Final);
            
        write('Wrong input: use a permutation of [1-15] and @ for "fifteen", '),
        write('and [0-13], 00 and @ for "thirteen"\n'),
        halt
    ),

    solve(Board, Final, Soln),
    print_soln(Soln),

    halt.
