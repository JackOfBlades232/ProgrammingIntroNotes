#!/usr/bin/swipl -q

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
