% modulos e biblioteca importados
:- use_module(types).
:- use_module(mapUtils).
:- use_module(library(readutil)).
:- use_module(map).
:- use_module(graph).

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

% evaluatePath(+PlayerPath, +Map, +Start, +End)
% PlayerPath: Lista de coordenadas do caminho percorrido pelo jogador.
% Map: O mapa atual do jogo.
% Start: A coordenada inicial do jogador.
% End: A coordenada de destino (usada para buscar no resultado do Bellman-Ford).
evaluatePath(PlayerPath, Map, Start, End) :-
    nl, write('--- Análise do Caminho ---'), nl,

    % Calcula o custo do caminho do jogador
    calculatePlayerPathCost(PlayerPath, Map, PlayerCost),

    % Obtém os nós e constrói o grafo
    listNodes(Map, Nodes),
    buildGraph(Map, Graph),

    % Executa Bellman-Ford para encontrar o caminho ideal
    bellmanFord(Nodes, Graph, Start, BellmanFordResult),

    % Processa o resultado do Bellman-Ford
    processBellmanFordResult(BellmanFordResult, PlayerCost, End),

    nl, write('--------------------------'), nl.

% --- Predicados Auxiliares para evaluatePath ---

% calculatePlayerPathCost(+Path, +Map, -TotalCost)
% Calcula o custo total de passar pelos tiles no caminho do jogador.
calculatePlayerPathCost([], _, 0).
calculatePlayerPathCost([Coord|Rest], Map, TotalCost) :-
    getElement(Coord, Map, Tile),
    passingCost(Tile, Cost),
    calculatePlayerPathCost(Rest, Map, RestCost),
    TotalCost is Cost + RestCost.



% Modificação em evaluatePath para passar Start e PlayerPath
evaluatePathModified(PlayerPath, Map, Start, End) :-
    nl, write('--- Análise do Caminho ---'), nl,

    calculatePlayerPathCost(PlayerPath, Map, PlayerCost),

    listNodes(Map, Nodes),
    buildGraph(Map, Graph),

    bellmanFord(Nodes, Graph, Start, BellmanFordResult),

    processBellmanFordResultModified(Start, PlayerPath, BellmanFordResult, PlayerCost, End), % Passando Start e PlayerPath

    nl, write('--------------------------'), nl.

% Modificação em processBellmanFordResult para receber Start e PlayerPath
processBellmanFordResultModified(_Start, _PlayerPath, left(ErrorMsg), _PlayerCost, _End) :-
    format('Erro ao calcular caminho ideal: ~w~n', [ErrorMsg]).
processBellmanFordResultModified(Start, PlayerPath, right(Distances, Preds), PlayerCost, End) :-
    (   member(End-OptimalCost, Distances) ->
        buildPath(Start, End, Preds, BestPath),
        format('Seu caminho passou por: ~w~n', [PlayerPath]),
        format('Seus custos: ~w~n', [PlayerCost]),
        format('Caminho ideal (Bellman-Ford): ~w~n', [BestPath]),
        format('Custo ideal (Bellman-Ford): ~w~n', [OptimalCost]),

        Diff is PlayerCost - OptimalCost,
        (   Diff == 0 ->
            writeln('Trabalho perfeito! Você fez a ferrovia mais rápida possível!')
        ;   Diff > 0 ->
            format('Viajar custou ~w a mais que o melhor trajeto possível.~n', [Diff])
        ;
            format('Seu custo foi ~w a menos que o ideal (verifique a lógica).~n', [Diff])
        )
    ;
        writeln('Destino inalcançável pelo Bellman-Ford.'),
        format('Seus custos: ~w~n', [PlayerCost])
    ).

    % Predicado de teste para evaluatePathModified
test_evaluation :-
    % Pega o mapa de exemplo do modulo graph
    graph:example_map(Map),

    % Define o caminho do jogador. Por exemplo, de (0,0) para (2,1)
    PlayerPath = [(0,0), (1,0), (2,0), (2,1)],

    % Define o ponto de partida e chegada
    Start = (0,0),
    End = (2,1),

    % Chama a função para avaliar o caminho

    evaluatePathModified(PlayerPath, Map, Start, End).
