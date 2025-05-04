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
