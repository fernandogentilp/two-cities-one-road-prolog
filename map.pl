% ==================================================
% Módulo: map.pl
% Propósito: geração de mapas
% ==================================================

:- module(map, [buildMap/4, randomize/0]).

:- use_module(types).
:- use_module(library(random)).

% Definir o predicado dinâmico dentro do módulo
:- dynamic random_initialized/0.

randomize :-
    (   random_initialized -> true
    ;   % Correção: usar a abordagem correta para SWI-Prolog
        % Gera uma semente aleatória baseada no tempo atual
        get_time(Time),
        TimeInt is round(Time),
        set_random(seed(TimeInt)),  % Método correto para SWI-Prolog
        asserta(random_initialized)
    ).

% predicados que unificam um tipo de terreno ao seu custo de travessia
passing(plains, 1).
passing(mountains, 3).
passing(lake, 3).
passing(forest, 2).
passing(city, 1).

% predicados que unificam um tipo de terreano ao seu custo de construcao
building(plains, 1).
building(mountains, 3).
building(lake, 2).
building(forest, 3).
building(city, 1).

% lista de possiveis terrenos
terrains([plains, mountains, lake, forest, city]).

% gera um terreno aleatorio
random_terrain(Terrain) :-
    terrains(Ts),
    random_member(Terrain, Ts).

% gera um painel aleatorio
random_tile(Lat, Long, tile(Terrain, (Lat, Long), PassingCost, BuildingCost, false)) :-
    random_terrain(Terrain),
    passing(Terrain, PassingCost),
    building(Terrain, BuildingCost).

% gera uma linha de paineis aleatorios
random_row(_, _, 0, []) :- !.
random_row(Lat, StartLong, N, [Tile | Rest]) :-
    N > 0,
    Long is StartLong,
    random_tile(Lat, Long, Tile),
    N1 is N - 1,
    NextLong is StartLong + 1,
    random_row(Lat, NextLong, N1, Rest).

% gera um mapa aleatorio
random_map(_, 0, _, []) :- !.
random_map(StartLat, Rows, Cols, [Row | Rest]) :-
    Rows > 0,
    Lat is StartLat,
    random_row(Lat, 0, Cols, Row),
    R1 is Rows - 1,
    NextLat is StartLat + 1,
    random_map(NextLat, R1, Cols, Rest).

% altera um painel (usado para adicionar as cidades)
updateTileInRow([_ | Rest], 0, NewTile, [NewTile | Rest]) :- !.
updateTileInRow([H | T], Index, NewTile, [H | NewRest]) :-
    Index > 0,
    I1 is Index - 1,
    updateTileInRow(T, I1, NewTile, NewRest).

% altera uma linha (adicionar as cidades)
updateRowInMap([_ | Rest], 0, NewRow, [NewRow | Rest]) :- !.
updateRowInMap([H | T], Index, NewRow, [H | NewRest]) :-
    Index > 0,
    I1 is Index - 1,
    updateRowInMap(T, I1, NewRow, NewRest).

% adicionar cidades
forceCityTile(Lat, Long, tile(city, (Lat, Long), 1, 1, false)).

% constroi um mapa com base nas dimensoes recebidas, e verifica se e valido
buildMap(Rows, Cols, FinalMap, StartPos) :-
    randomMap(0, Rows, Cols, Map),

    MaxRowIndex is Rows - 1,
    random_between(0, MaxRowIndex, Row1),
    repeat,
        random_between(0, MaxRowIndex, Row2),
        Row2 \= Row1, !,

    forceCityTile(Row1, 0, City1),
    nth0(Row1, Map, OriginalRow1),
    updateTileInRow(OriginalRow1, 0, City1, UpdatedRow1),
    updateRowInMap(Map, Row1, UpdatedRow1, TempMap),

    RightColIndex is Cols - 1,
    forceCityTile(Row2, RightColIndex, City2),
    nth0(Row2, TempMap, OriginalRow2),
    updateTileInRow(OriginalRow2, RightColIndex, City2, UpdatedRow2),
    updateRowInMap(TempMap, Row2, UpdatedRow2, FinalMap),

    map(FinalMap),
    StartPos = (Row1, 0).


