#!/usr/bin/swipl -q

:- initialization(main).

inrange(1, _).
inrange(I, R) :- inrange(II, R), ((II >= R, !, fail); I is II + 1).

queen_is_ok(queen(_, Y), [], M) :- inrange(Y, M).
queen_is_ok(queen(X, Y), [queen(X1, Y1)|Others], M) :-
    inrange(Y, M), Y =\= Y1,
    DX is X - X1, DY is Y - Y1, MDY is Y1 - Y, DX =\= DY, DX =\= MDY,
    queen_is_ok(queen(X, Y), Others, M).

solve(N, M, N, Sol) :- !, inrange(Y, M), Sol = [queen(N, Y)].
solve(N, M, Base, Sol) :-
    Base2 is Base + 1, solve(N, M, Base2, OthersSol),
    queen_is_ok(queen(Base, Y), OthersSol, M),
    Sol = [queen(Base, Y)|OthersSol].

print_solution([]).
print_solution([queen(_, Y)|Tail]) :-
    write(Y), write(' '), print_solution(Tail).

print_solutions([]).
print_solutions([H|T]) :- print_solution(H), nl, print_solutions(T).

main :-
    on_signal(int, _, default),
    prompt(_, ''),
    current_prolog_flag(argv, Argv),

    ([Arg] = Argv, atom_number(Arg, N), !,
        findall(Sol, solve(N, N, 1, Sol), Sols), print_solutions(Sols);
        write('Invalid upsage: prog [N]\n')
    ),
    
    halt.

