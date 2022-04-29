:- set_prolog_flag(double_quotes, string).

:- dynamic parent/2.

parent("Lucas", "Mary").
parent("Lucas", "Jason").
parent("Lucas", "Peter").
parent("Mary", "Fred").
parent("Mary", "Jane").
parent("Jason", "Sean").
parent("Jason", "Jessica").
parent("Jason", "Hannah").
parent("Jessica", "Joseph").
parent("Jessica", "John").
parent("Jessica", "Laura").

ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).

sibling(A, B) :- parent(X, A), parent(X, B), A \= B.


plays("John", football).
plays("George", football).
plays("Sam", football).
plays("George", chess).
plays("Mary", chess).
plays("Sam", baseball).

learns("John", chemistry).
learns("John", biology).
learns("Sam", math).
learns("Sam", biology).
learns("George", history).
learns("Ann", law).

interesting(Pers) :- learns(Pers, _), plays(Pers, _).
student_plays(Pers, Game) :-
    learns(Pers, _), plays(Pers, Game).



del_any(_, [], []).
del_any(Elem, [Elem|Tail], Res) :- del_any(Elem, Tail, Res).
del_any(Elem, [X|Tail], [X|NewT]) :- del_any(Elem, Tail, NewT).

del_any_inv(_, [], []).
del_any_inv(Elem, [X|Tail], [X|NewT]) :- del_any_inv(Elem, Tail, NewT).
del_any_inv(Elem, [Elem|Tail], Res) :- del_any_inv(Elem, Tail, Res).

del_all(_, [], []).
del_all(Elem, [Elem|Tail], Res) :-
    del_all(Elem, Tail, Res).
del_all(Elem, [X|Tail], [X|NewT]) :-
    not(X = Elem),
    del_all(Elem, Tail, NewT).

del_all_cut(_, [], []) :- !.
del_all_cut(Elem, [Elem|Tail], Res) :- !, del_all_cut(Elem, Tail, Res).
del_all_cut(Elem, [X|Tail], [X|NewT]) :- del_all_cut(Elem, Tail, NewT).

del_all_ifthen(_, [], []).
del_all_ifthen(Elem, [X|Tail], Res) :-
    Elem = X ->
        del_all_ifthen(Elem, Tail, Res) ;
        del_all_ifthen(Elem, Tail, NewT) , Res = [X|NewT].

listlen([], 0).
listlen([_|T], L) :- listlen(T, TL), L is TL + 1.

prefix([], _, 0).
prefix([H|T2], [H|T], N) :- N2 is N - 1, prefix(T2, T, N2).

prefix2([], _, 0).
prefix2([H|T2], [H|T], N) :- nonvar(N), N2 is N - 1, prefix2(T2, T, N2).
prefix2([H|T2], [H|T], N) :- var(N), prefix2(T2, T, N2), N is N2 + 1.


join([], L, L).
join([H|T], L, [H|M]) :- join(T, L, M).


listrev(L, R) :- listrev(L, R, []).

listrev([], A, A).
listrev([H|T], R, A) :- listrev(T, R, [H|A]).

