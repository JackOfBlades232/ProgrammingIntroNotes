#!/usr/bin/swipl -q

:- initialization(main).            % sets the entry point, only for interpreter
main :-                             % can also take Argv, but not when compiling
    on_signal(int, _, default),     % fixes ^C
    prompt(_, ''),                  % removes interpreter prompts
    current_prolog_flag(argv, Argv),% sets argv
    write('Hello, world!\n'),
    write(Argv),
    nl,
    halt.

% compilation is quite cursed
