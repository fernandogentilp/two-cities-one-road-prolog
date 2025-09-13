% ==================================================
% Módulo: types.pl
% Propósito: definição de tipos
% ==================================================

%preparando o modulo para ser importado, os /1 significam um predicado com um argumento
:- module(types, [terrain/1, coord/1, tile/1, row/1, map/1]).

%predicados terrain para definicao de tipo
terrain(plains).
terrain(mountains).
terrain(lake).
terrain(forest).
terrain(city).

%predicado coords para definicao das coordenadas
coord((Lat, Long)) :- integer(Lat), integer(Long).

%definindo possiveis valores para built (prolog nao tem tipo bool)
built(true).
built(false).

%predicado tile para definir um painel
tile(tile(Terrain, Location, PassingCost, BuildingCost, Built)) :- 
    terrain(Terrain), 
    coord(Location), 
    integer(PassingCost), 
    integer(BuildingCost), 
    built(Built).

%definindo uma linha de tiles para a matriz
row([]).
row([T|Ts]) :- tile(T), row(Ts).

%definindo uma matriz de tile usada como map
map([]).
map([Row|Rows]) :- row(Row), map(Rows).
