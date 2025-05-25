#!/usr/bin/swipl -q

% @TODO: finish

:- initialization(main).

inversions_with(_, [], Out) :- Out is 0.
inversions_with(E, [H|T], Out) :- inversions_with(E, T, OutWithout),
    (E > H, !, Out is OutWithout + 1; Out is OutWithout).

inversion_count([], Out) :- Out is 0.
inversion_count([H|T], Out) :- inversion_count(T, OutWithout),
    inversions_with(H, T, OutWith), Out is OutWith + OutWithout.

even_permutation(L) :- inversion_count(L, Count), Count mod 2 =:= 0.

list_is_fifteen(L) :-
    Elems =
    [
        '1', '2', '3', '4', '5', '6', '7', '8', '9',
        '10', '11', '12', '13', '14', '15', '@'
    ],
    sort(Elems, ElemsSorted), sort(L, LSorted),
    ElemsSorted = LSorted.

code_list_to_board(
    [E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14, E15, E16],
    board(row(E1, E2, E3, E4), row(E5, E6, E7, E8), row(E9, E10, E11, E12),
        row(E13, E14, E15, E16))).

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
    code_list_to_board(InitCodes, InitBoard),
    (even_permutation(InitCodes), !,
        FinalBoard = board(
            row(1, 2, 3, 4), row(5, 6, 7, 8),
            row(9, 10, 11, 12), row(13, 14, 15, 16));
        FinalBoard = board(
            row(1, 2, 3, 4), row(5, 6, 7, 8),
            row(9, 10, 11, 12), row(13, 15, 14, 16))
    ).

main :-
    on_signal(int, _, default),
    prompt(_, ''),
    current_prolog_flag(argv, Argv),

    (
        list_is_fifteen(Argv), !,
            list_to_board_fifteen(Argv, Board),
            write(Board), nl,
            final_board_fifteen(Board, Final),
            write('Dest:'), nl,
            write(Final), nl
        ;
            
        % @TODO: thirteen

        write('Wrong input: use a permutation of [1-15] and @ for "fifteen", '),
        write('and [0-13], 00 and @ for "thirteen"\n')
    ),


    halt.

