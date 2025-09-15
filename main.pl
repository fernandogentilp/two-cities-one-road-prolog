% ==================================================
% Módulo: main.pl
% Propósito: ponto de entrada principal do jogo
% ==================================================

:- module(main, [main/0]).

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

processMenuOption('G') :-
    start,
    main.

processMenuOption('Q') :-
    halt.

processMenuOption(_) :-
    main.

:- initialization(main, main).
