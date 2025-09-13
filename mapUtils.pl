% ==================================================
% Módulo: mapUtils.pl
% Propósito: TODO
% ==================================================

:- module(mapUtils, [getElement/3, verifyCoord/2, findNeighbors/2]).
:- use_module(types).

%nth0(Index, List, Elem) unifica a linha ou o painel do index passado como parametro junto da lista, começando de 0
getElement((Lat, Long), Map, Tile) :- nth0(Lat, Map, Row), nth0(Long, Row, Tile).

%verifica se indices lat e long sao validos, dentro dos limites da length do mapa e das linhas.
%length unifica o tamanho de uma lista com a propria lista passada como parametro.
verifyCoord((Lat, Long), Map) :- 
    Map \= [], 
    Lat >= 0, Long >= 0,
    length(Map, Rows), Lat < Rows,
    nth0(0, Map, FirstRow), length(FirstRow, Cols), Long < Cols.

%unifica uma lista com os 4 vizinhos (cima baixo esquerda e direita) de uma coordenada passada como parametro.
%is é usado porque prolog não processa operações aritméticas automaticamente, retornaria "Lat - 1", por ex.
findNeighbors((Lat, Long), Neighbors) :- 
    North is Lat - 1,
    South is Lat + 1,
    West is Long - 1,
    East is Long + 1,
    Neighbors = [(North, Long), (South, Long), (Lat, West), (Lat, East)].
