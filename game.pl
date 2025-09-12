% modulos e biblioteca importados
:- use_module(types).
:- use_module(mapUtils).
:- use_module(library(readutil)).
:- use_module(map).

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
move_offset(w, (-1, 0)).
move_offset(s, (1, 0)).
move_offset(a, (0, -1)).
move_offset(d, (0, 1)).

% inicia o jogo com valores iniciais e um mapa aleatorio, assim como o orcamento do jogador
start :-
    buildMap(5, 5, Map),
    InitialPos = (0,0),
    InitialBudget = 15,
    update_start_tile(Map, InitialPos, Map1),
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
    nl, write('Fim de jogo! Seu orcamento acabou!'), nl,
    write('Voce perdeu!'), nl,
    nl, write('Mapa final:'), nl,
    printMap(Map).

% derrota ao nao ter vizinhos dentro do orcamento
gameLoop(Map, Pos, Budget) :-
    \+ hasAffordableTile(Map, Pos, Budget),
    nl, write('Voce nao tem orcamento suficiente para construir em nenhum painel adjacente!'), nl,
    write('Voce perdeu!'), nl,
    nl, write('Mapa final:'), nl,
    printMap(Map).

% continua
gameLoop(Map, Pos, Budget) :-
    nl, write('--- MAPA ATUAL ---'), nl,
    printMap(Map),
    format('Orcamento atual: ~w~n', [Budget]),
    nl, write('Use W/A/S/D para mover e construir ou "stop." para sair:'), nl,
    read(Input),
    processInput(Input, Map, Pos, Budget).

% processamento da entrada do jogador
processInput(stop, MapIn, _Pos, Budget) :-
    nl, write('Jogo interrompido! Mapa final:'), nl,
    printMap(MapIn),
    format('Orcamento final: ~w~n', [Budget]).

processInput(Input, MapIn, (Lat, Long), Budget) :-
    move_offset(Input, (DL, DC)),
    NewLat is Lat + DL,
    NewLong is Long + DC,
    NewPos = (NewLat, NewLong),
    (   verifyCoord(NewPos, MapIn) ->
        getElement(NewPos, MapIn, tile(_, NewPos, _, BuildCost, Built)),

        (   Built = true ->
            write('Esse painel ja esta construido! Tente outro.'), nl,
            gameLoop(MapIn, (Lat, Long), Budget)

        ;   BuildCost =< Budget ->
            getElement(NewPos, MapIn, tile(T, NewPos, PC, BC, _)),
            NewTile = tile(T, NewPos, PC, BC, true),
            update_tile(MapIn, NewPos, NewTile, MapOut),
            NewBudget is Budget - BuildCost,
            gameLoop(MapOut, NewPos, NewBudget)

        ;
            write('Orcamento insuficiente para construir nesse painel!'), nl,
            gameLoop(MapIn, (Lat, Long), Budget)
        )
    ;
        write('Movimento invalido! Tente novamente.'), nl,
        gameLoop(MapIn, (Lat, Long), Budget)
    ).

processInput(_, MapIn, (Lat, Long), Budget) :-
    write('Comando invalido! Use W/A/S/D ou "stop", sempre com um "."'), nl,
    gameLoop(MapIn, (Lat, Long), Budget).
