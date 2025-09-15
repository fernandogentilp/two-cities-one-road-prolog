% ==================================================
% Módulo: game.pl
% Propósito: lógica principal do jogo
% ==================================================

:- module(game, [start/0, gameLoop/4]).

% modulos e biblioteca importados
:- use_module(types).
:- use_module(mapUtils).
:- use_module(library(readutil)).
:- use_module(map).
:- use_module(interfaces, [mapToMatrix/2, gameScreen/3, read_single_key/1, process_key/2, endScreen/3]).
:- use_module(map, [randomize/0]).
:- use_module(graph).

% exibicao do mapa
printMap(Map) :-
    mapToMatrix(Map, Matrix),
    printMatrix(Matrix).

printMatrix([]).
printMatrix([Row|Rows]) :-
    printMatrixRow(Row), nl,
    printMatrix(Rows).

printMatrixRow([]).
printMatrixRow([Char|Rest]) :-
    write(Char),
    printMatrixRow(Rest).

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

% inicia o jogo com valores iniciais e um mapa aleatorio, assim como o orcamento do jogador
start :-
    randomize,
    buildMap(5, 5, Map),
    InitialPos = (0,0),
    InitialBudget = 15,
    update_start_tile(Map, InitialPos, Map1),
    gameLoop(Map1, InitialPos, InitialBudget, [InitialPos]).

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
gameLoop(Map, _, Budget, _) :-
    Budget =< 0,
    endScreen("Fim de jogo! Seu orçamento acabou!",
              ["Você perdeu!", "Mapa final"],
              Screen),
    write(Screen), nl,
    printMap(Map).

% derrota ao nao ter vizinhos dentro do orcamento
gameLoop(Map, Pos, Budget, _) :-
    \+ hasAffordableTile(Map, Pos, Budget),
    endScreen("Sem orcamento suficiente!", 
             ["Voce nao tem orcamento para construir em paineis adjacentes!", 
              "Voce perdeu!", "Mapa final:"], 
              Screen),
    write(Screen), nl,
    printMap(Map).

% continua
gameLoop(Map, Pos, Budget, Path) :-
    gameScreen(Map, Budget, Screen),
    write(Screen), nl,
    
    read_single_key(Input),
    processInput(Input, Map, Pos, Budget, Path).

% Lógica de vitória
gameLoop(Map, Pos, Budget, Path) :-
    isRightmostCity(Pos, Map),
    victoryScreen(Map, Budget, Path, Screen),
    write(Screen), nl,
    !.

% processamento da entrada do jogador
processInput('q', MapIn, _Pos, _Budget, _Path) :-
    nl, write('Jogo interrompido! Mapa final:'), nl,
    printMap(MapIn).

processInput(Input, MapIn, Pos, Budget, Path) :-
    process_key(Input, UpperInput),
    member(UpperInput, ['W','A','S','D','Q']),
    move_offset(UpperInput, Offset),
    (   Offset = stop ->
        nl, write('Jogo interrompido! Mapa final:'), nl,
        printMap(MapIn)
    ;   process_movement(Offset, MapIn, Pos, Budget, Path)
    ).

processInput(_, MapIn, Pos, Budget, Path) :-
    write('Comando invalido! Use W/A/S/D/Q'), nl,
    gameLoop(MapIn, Pos, Budget, Path).

process_movement((DL, DC), MapIn, (Lat, Long), Budget, Path) :-
    NewLat is Lat + DL,
    NewLong is Long + DC,
    NewPos = (NewLat, NewLong),
    (   verifyCoord(NewPos, MapIn) ->
        getElement(NewPos, MapIn, tile(_, NewPos, _, BuildCost, Built)),
        (   Built = true ->
            write('Esse painel já está construido! Tente outro.'), nl,
            gameLoop(MapIn, (Lat, Long), Budget, Path)
        ;   BuildCost =< Budget ->
            getElement(NewPos, MapIn, tile(T, NewPos, PC, BC, _)),
            NewTile = tile(T, NewPos, PC, BC, true),
            update_tile(MapIn, NewPos, NewTile, MapOut),
            NewBudget is Budget - BuildCost,
            append(Path, [NewPos], NewPath), 
            gameLoop(MapOut, NewPos, NewBudget, NewPath)
        ;   write('Orçamento insuficiente para construir nesse painel!'), nl,
            gameLoop(MapIn, (Lat, Long), Budget, Path)
        )
    ;   write('Movimento invalido! Tente novamente.'), nl,
        gameLoop(MapIn, (Lat, Long), Budget, Path)
    ).

% Processamento de movimento com histórico
process_movement((DL, DC), MapIn, (Lat, Long), Budget, Path) :-
    NewLat is Lat + DL,
    NewLong is Long + DC,
    NewPos = (NewLat, NewLong),
    (   verifyCoord(NewPos, MapIn) ->
        getElement(NewPos, MapIn, tile(_, NewPos, _, BuildCost, Built)),
        (   Built = true ->
            write('Esse painel já está construido! Tente outro.'), nl,
            gameLoop(MapIn, (Lat, Long), Budget, Path)
        ;   BuildCost =< Budget ->
            getElement(NewPos, MapIn, tile(T, NewPos, PC, BC, _)),
            NewTile = tile(T, NewPos, PC, BC, true),
            update_tile(MapIn, NewPos, NewTile, MapOut),
            NewBudget is Budget - BuildCost,
            append(Path, [NewPos], NewPath), 
            gameLoop(MapOut, NewPos, NewBudget, NewPath)
        ;   write('Orçamento insuficiente para construir nesse painel!'), nl,
            gameLoop(MapIn, (Lat, Long), Budget, Path)
        )
    ;   write('Movimento invalido! Tente novamente.'), nl,
        gameLoop(MapIn, (Lat, Long), Budget, Path)
    ).

% Predicados de vitória
isRightmostCity((Row, Col), Map) :-
    mapSize(Map, _Rows, Cols),
    Col =:= Cols - 1,                
    getElement((Row, Col), Map, tile(city, (Row, Col), _, _, _)).

mapSize(Map, Rows, Cols) :-
    length(Map, Rows),
    Map = [FirstRow|_],
    length(FirstRow, Cols).

% Tela de vitória personalizada
victoryScreen(Map, Budget, Path, Screen) :-
    pathCost(Map, Path, PlayerCost),
    
    % Calcular caminho ótimo
    flatten(Map, FlatTiles),
    listNodes(FlatTiles, Nodes),
    buildGraph(FlatTiles, Graph),
    Path = [Start|_],
    End = Path,
    
    bellmanFord(Nodes, Graph, Start, Result),
    
    (   Result = right(_Dists, Predecessors) ->
        buildPath(Start, End, Predecessors, OptPath),
        pathCost(Map, OptPath, OptCost),
        (   PlayerCost =:= OptCost ->
            Message = 'Incrivel! Voce encontrou o caminho otimo!'
        ;   Message = 'Tente novamente para encontrar o caminho otimo.'
        )
    ;   Message = 'Erro ao calcular caminho otimo.'
    ),
    
    Lines = [
        'Parabens! Voce conectou as duas cidades!',
        format('Orcamento restante: ~w', [Budget]),
        format('Custo do seu caminho: ~w', [PlayerCost]),
        Message,
        'Mapa final:'
    ],
    
    endScreen("VITORIA!", Lines, Screen).
