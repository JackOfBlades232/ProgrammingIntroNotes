#!/usr/bin/swipl -q

% 11_14

% A
even_len_list([]).
even_len_list([_]) :- fail.
even_len_list([_, _|Tail]) :- even_len_list(Tail).

% B
odd_len_list(X) :- even_len_list([[]|X]).

% C
list_helper([]).
list_helper([_|Tail]) :- list_helper(Tail).
two_repeated_list([X, X|Tail]) :- list_helper(Tail), !.
two_repeated_list([_|Tail]) :- two_repeated_list(Tail).

% D
empty_lists_at_evens_list([_, []|Tail]) :- empty_lists_at_evens_list(Tail), !.
empty_lists_at_evens_list([_]).
empty_lists_at_evens_list([]).

% 11_15

none(_, []).
none(E, [X|Tail]) :- E \= X, none(E, Tail).

one(E, [E|Tail]) :- !, none(E, Tail).
one(E, [_|Tail]) :- one(E, Tail).

atleast2(E, [E|Tail]) :- one(E, Tail).
atleast2(E, [_|Tail]) :- atleast2(E, Tail).

% 11_16

lst([]).
lst([_|Tail]) :- lst(Tail).

similar([], []).
similar([F|FTail], [[]|ETail]) :- lst(F), similar(FTail, ETail).
similar([F|FTail], [F|ETail]) :- similar(FTail, ETail).

% 11_17

eq([], []).
eq([X|FTail], [X|STail]) :- eq(FTail, STail).

ins_one(List, Elem, [Elem|RTail]) :- eq(List, RTail).
ins_one([X|LTail], Elem, [X|RTail]) :- ins_one(LTail, Elem, RTail).

% 11_18

% A
shrink2any_helper([_|LTail], R, 0) :- shrink2any_helper(LTail, R, 1).
shrink2any_helper([_|LTail], R, 1) :- eq(LTail, R).
shrink2any_helper([X|LTail], [X|RTail], N) :-
    shrink2any_helper(LTail, RTail, N).

shrink2any(L, R) :- shrink2any_helper(L, R, 0).

% B
shrink2atmost_helper([_|LTail], R, 0) :-
    eq(LTail, R);
    shrink2any_helper(LTail, R, 1).
shrink2atmost_helper([_|LTail], R, 1) :- eq(LTail, R).
shrink2atmost_helper([X|LTail], [X|RTail], N) :-
    shrink2atmost_helper(LTail, RTail, N).

shrink2atmost(L, R) :- eq(L, R); shrink2atmost_helper(L, R, 0).

% C
shrink2consecutive([_, _|LTail], R) :- eq(LTail, R).
shrink2consecutive([X|LTail], [X|RTail]) :- shrink2consecutive(LTail, RTail).

% 11_19

strike(L, [[]|L]).
strike([_|LTail], [[]|RTail]) :- strike(LTail, [[]|RTail]).
strike([X|LTail], [X|RTail]) :- strike(LTail, RTail).

% 11_20

permute([], []).
permute(L, [X|Tail]) :- ins_one(Rem, X, L), permute(Rem, Tail).

% 11_21

listrev(L, R) :- listrev(L, R, []).
listrev([], A, A) :- !.
listrev([H|T], R, A) :- listrev(T, R, [H|A]).

uniq(L, R) :- uniq(L, RR, []), listrev(R, RR).
uniq([], R, R) :- !.
uniq([H|T], R, A) :- none(H, A), !, uniq(T, R, [H|A]).
uniq([_|T], R, A) :- uniq(T, R, A).
