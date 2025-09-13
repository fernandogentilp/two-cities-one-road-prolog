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
    write('Escolha uma opcao:'), nl,
    write('1. Iniciar novo jogo'), nl,
    write('2. Sair'), nl,
    read(Option),
    processMenuOption(Option).

processMenuOption(1) :-
    start.

processMenuOption(2) :-
    write('Ate logo!'), nl.

processMenuOption(_) :-
    write('Opcao invalida!'), nl,
    main.

:- initialization(main, main).
