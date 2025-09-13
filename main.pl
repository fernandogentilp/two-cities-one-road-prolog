% ==================================================
% Módulo: main.pl
% Propósito: TODO
% ==================================================

:- use_module(types).
:- use_module(mapUtils).
:- use_module(map).
:- use_module(interfaces).
:- use_module(game).

% Menu principal
main :-
    nl, nl,
    homeScreen(Screen),
    write(Screen), nl,

    read_single_key(Key),
    process_key(Key, UpperKey),
    processMenuOption(UpperKey).

processMenuOption('A') :-
    start.

processMenuOption('Q') :-
    write('Programa encerrado.'), nl.

processMenuOption(_) :-
    main.

:- initialization(main, main).
