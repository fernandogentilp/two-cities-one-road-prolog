% ==================================================
% Módulo: game.pl
% Propósito: lógica principal do jogo
% ==================================================

:- module(game, [start/0, gameLoop/3]).

% modulos e biblioteca importados
:- use_module(types).
:- use_module(persistence).
:- use_module(mapUtils).
:- use_module(library(readutil)).
:- use_module(map).
:- use_module(interfaces).
:- use_module(map, [randomize/0]).

% exibicao do mapa
printMap([]).
printMap([Row|Rows]) :-
    printRow(Row), nl,
    printMap(Rows).

printRow([]).
printRow([tile(Terrain,_,_,_,Built)|Rest]) :-
    format('~w(~w) ', [Terrain, Built]),
    printRow(Rest).

% predicados de offset para definir a nova coordenada apos jogador indicar a direcao que quer ir
move_offset('W', (-1, 0)).
move_offset('S', (1, 0)).
move_offset('A', (0, -1)).
move_offset('D', (0, 1)).
move_offset('Q', stop).

% inicia o jogo com valores iniciais, salva estado inicial e entra no loop
start :-
    verify_and_create_files,  % garante arquivos criados se não existirem
    randomize,
    buildMap(5, 5, Map),
    InitialPos = (0,0),
    InitialBudget = 15,
    update_start_tile(Map, InitialPos, Map1),

    % grava estado inicial nos arquivos de persistência
    file_matrix(MatrixFile), write_string(MatrixFile, Map1),
    file_cash(CashFile), write_int(CashFile, InitialBudget),
    file_coord(CoordFile), write_string(CoordFile, "0 0"),

    % inicia loop do jogo
    gameLoop(Map1, InitialPos, InitialBudget).

% marca o tile inicial como construido
update_start_tile(Map, Pos, NewMap) :-
    getElement(Pos, Map, tile(T, Pos, PC, BC, _)),
    New = tile(T, Pos, PC, BC, true),
    update_tile(Map, Pos, New, NewMap).

% atualizacao do tile apos construir
update_tile(Map, (Lat, Long), NewTile, NewMap) :-
    nth0(Lat, Map, OldRow, RestRows),
    nth0(Long, OldRow, _OldTile, RestCols),
    nth0(Long, NewRow, NewTile, RestCols),
    nth0(Lat, NewMap, NewRow, RestRows).

% verifica se há algum tile vizinho que pode ser construido com o orcamento restante
hasAffordableTile(Map, Pos, Budget) :-
    findNeighbors(Pos, Neighbors),
    include(validCoord(Map), Neighbors, ValidNeighbors),
    member(Coord, ValidNeighbors),
    getElement(Coord, Map, tile(_, _, _, BuildCost, Built)),
    Built = false,
    BuildCost =< Budget, !.

% fala por si so, valida coordenadas
validCoord(Map, (Lat, Long)) :-
    verifyCoord((Lat, Long), Map).

% loop do jogo
% derrota ao zerar orcamento
gameLoop(Map, _, Budget) :-
    Budget =< 0,
    endScreen("Fim de jogo! Seu orçamento acabou!",
              ["Você perdeu!", "Mapa final"],
              Screen),
    write(Screen), nl,
    printMap(Map).

% derrota ao nao ter vizinhos dentro do orcamento
gameLoop(Map, Pos, Budget) :-
    \+ hasAffordableTile(Map, Pos, Budget),
    endScreen("Sem orcamento suficiente!", 
             ["Voce nao tem orcamento para construir em paineis adjacentes!", 
              "Voce perdeu!", "Mapa final:"], 
              Screen),
    write(Screen), nl,
    printMap(Map).

% continua
gameLoop(Map, Pos, Budget) :-
    gameScreen(Map, Budget, Screen),
    write(Screen), nl,
    
    read_single_key(Input),
    processInput(Input, Map, Pos, Budget).

% processamento da entrada do jogador
processInput('q', MapIn, _Pos, _Budget) :-
    nl, write('Jogo interrompido! Mapa final:'), nl,
    printMap(MapIn).

processInput(Input, MapIn, Pos, Budget) :-
    process_key(Input, UpperInput),
    member(UpperInput, ['W','A','S','D','Q']),
    move_offset(UpperInput, Offset),
    (   Offset = stop ->
        nl, write('Jogo interrompido! Mapa final:'), nl,
        printMap(MapIn)
    ;   process_movement(Offset, MapIn, Pos, Budget)
    ).

processInput(_, MapIn, Pos, Budget) :-
    write('Comando invalido! Use W/A/S/D ou "stop", sempre com um "."'), nl,
    gameLoop(MapIn, Pos, Budget).

process_movement((DL, DC), MapIn, (Lat, Long), Budget) :-
    NewLat is Lat + DL,
    NewLong is Long + DC,
    NewPos = (NewLat, NewLong),
    (   verifyCoord(NewPos, MapIn) ->
        getElement(NewPos, MapIn, tile(_, NewPos, _, BuildCost, Built)),
        (   Built = true ->
            write('Esse painel já está construido! Tente outro.'), nl,
            gameLoop(MapIn, (Lat, Long), Budget)
        ;   BuildCost =< Budget ->
            getElement(NewPos, MapIn, tile(T, NewPos, PC, BC, _)),
            NewTile = tile(T, NewPos, PC, BC, true),
            update_tile(MapIn, NewPos, NewTile, MapOut),
            NewBudget is Budget - BuildCost,
            gameLoop(MapOut, NewPos, NewBudget)
        ;   write('Orçamento insuficiente para construir nesse painel!'), nl,
            gameLoop(MapIn, (Lat, Long), Budget)
        )
    ;   write('Movimento invalido! Tente novamente.'), nl,
        gameLoop(MapIn, (Lat, Long), Budget)
    ).
