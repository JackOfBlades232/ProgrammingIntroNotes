#!/usr/bin/swipl -q

% @TODO: still wrong, fix

count_element(E, [E|_], N) :- !.
count_element(E, [_|Tail]) :- has_element(E, Tail).

has_two_of(E, [E|Tail]) :- has_element(E, Tail).
has_two_of(E, [_|Tail]) :- has_two_of(E, Tail).
