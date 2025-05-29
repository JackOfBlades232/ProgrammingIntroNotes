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

rowcol_to_linear(RI, EI, LI) :- LI is (RI - 1) * 4 + EI.
linear_to_rowcol(LI, RI, EI) :-
    RI is (LI - 1) div 4 + 1, EI is (LI - 1) mod 4 + 1.

board_at(B, LI, Elem) :-
    linear_to_rowcol(LI, RI, EI), board_at(B, RI, EI, Elem).
board_search(B, LI, Elem) :-
    board_at(B, RI, EI, Elem), rowcol_to_linear(RI, EI, LI).

% @TODO: set by linear index?

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

% @TODO: extend to partial boards
turns_from(Board, OutBoard, ORI, OEI) :-
    board_at(Board, RI, EI, 16),
    turns_from(RI, EI, 1, 1, 4, 4, ORI, OEI),
    apply_turn_to_board(Board, OutBoard, RI, EI, ORI, OEI).

diff_board_score_at(From, To, RI, EI, S) :-
    board_at(From, RI, EI, F), board_at(To, RI, EI, T),
    (F =\= T, T =\= 16, S = 1; S = 0).

diff_board_score(From, To, 4, 4, S) :- !,
    diff_board_score_at(From, To, 4, 4, S).
diff_board_score(From, To, RI, EI, S) :-
    diff_board_score_at(From, To, RI, EI, Addition),
    (EI =:= 4, !, NRI is RI + 1, NEI is 1; NRI is RI, NEI is EI + 1),
    diff_board_score(From, To, NRI, NEI, Rest), S is Rest + Addition.
diff_board_score(From, To, S) :- diff_board_score(From, To, 1, 1, S).

choose_next_node([], Min, _) :- Min = 2^64 - 1.
choose_next_node([HT|OT], Min, MinRec) :-
    HT = record(_, G, H), Score is G + H,
    choose_next_node(OT, OMin, OMinRec),
    (Score < Min,
        Min = Score, MinRec = HT;
        Min = OMin, MinRec = OMinRec
    ).

solve_board(Final, OpenList, ClosedList) :-
    

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

% @TEST, to be removed
turns_from_test(B, O) :- turns_from(B, OB, ORI, OEI), O = tfout(OB, ORI, OEI).
print_tfouts([]).
print_tfouts([tfout(OB, ORI, OEI)|T]) :- 
    write(ORI), write(' '), write(OEI), nl, write(OB), nl,
    print_tfouts(T).

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
            write(Final), nl,

            board_row(Board, 2, R), write(R), nl,
            row_at(R, 4, E), write(E), nl,

            board_at(Board, 3, 2, EE), write(EE), nl,
            board_at(Board, RI, EI, 12), write(RI), write(' '), write(EI), nl,

            board_at(Board, 11, EEE), write(EEE), nl,
            board_search(Board, LI, 7), write(LI), nl,

            findall(
                tfout(OB, ORI, OEI),
                turns_from_test(Board, tfout(OB, ORI, OEI)),
                Turns),
            print_tfouts(Turns),

            diff_board_score(Board, Final, Score),
            write('Score: '), write(Score), nl;
            
        % @TODO: thirteen

        write('Wrong input: use a permutation of [1-15] and @ for "fifteen", '),
        write('and [0-13], 00 and @ for "thirteen"\n')
    ),

    halt.
