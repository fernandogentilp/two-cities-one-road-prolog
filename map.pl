% ==================================================
% Módulo: map.pl
% Propósito: TODO
% ==================================================

:- module(map, [buildMap/3]).
:- use_module(types).
:- use_module(library(random)).

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

% constroi um mapa com base nas dimensoes recebidas, e verifica se e valido
buildMap(Rows, Cols, Map) :-
    random_map(0, Rows, Cols, Map),
    map(Map).
