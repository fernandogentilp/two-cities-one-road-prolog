:- use_module(types).
:- use_module(mapUtils).
:- use_module(library(readutil)).

%mapa (exemplo, apenas esse e manualmente)
buildMap(Map) :-
    Map = [
      [tile(plains,   (0,0), 1, 5, false), tile(forest,   (0,1), 2, 4, false), tile(plains,   (0,2), 1, 5, false), tile(city,      (0,3), 1,10,false)],
      [tile(lake,     (1,0), 3, 6, false), tile(plains,   (1,1), 1, 5, false), tile(forest,   (1,2), 2, 4, false), tile(plains,    (1,3), 1, 5, false)],
      [tile(plains,   (2,0), 1, 5, false), tile(mountains,(2,1), 4, 8, false), tile(lake,     (2,2), 3, 6, false), tile(forest,    (2,3), 2, 4, false)],
      [tile(city,     (3,0), 1,10, false), tile(plains,   (3,1), 1, 5, false), tile(mountains,(3,2), 4, 8, false), tile(plains,    (3,3), 1, 5, false)]
    ],
    map(Map).

%atualizacao do tile apos construir
update_tile(MapIn, (Lat, Long), NewTile, MapOut) :-
    nth0(Lat, MapIn, OldRow, RestRows),
    nth0(Long, OldRow, _OldTile, RestCols),
    nth0(Long, NewRow, NewTile, RestCols),
    nth0(Lat, MapOut, NewRow, RestRows).

%exibicao do mapa
printMap([]).
printMap([Row|Rows]) :-
    printRow(Row), nl,
    printMap(Rows).

printRow([]).
printRow([tile(Terrain,_,_,_,Built)|Rest]) :-
    format('~w(~w) ', [Terrain, Built]),
    printRow(Rest).

%predicados de offset para definir a nova coordenada apos jogador indicar a direcao que quer ir
move_offset(w, (-1, 0)).
move_offset(s, (1, 0)).
move_offset(a, (0, -1)).
move_offset(d, (0, 1)).

startGame :-
    buildMap(Map),
    %comeca no primeiro tile construido inicialmente
    InitialPos = (0,0),
    update_start_tile(Map, InitialPos, Map1),
    gameLoop(Map1, InitialPos).

%marca o tile inicial como construido
update_start_tile(MapIn, Pos, MapOut) :-
    getElement(Pos, MapIn, tile(T, Pos, PC, BC, _)),
    New = tile(T, Pos, PC, BC, true),
    update_tile(MapIn, Pos, New, MapOut).

%loop do jogo
gameLoop(Map, Pos) :-
    nl, write('--- MAPA ATUAL ---'), nl,
    printMap(Map),
    nl, write('Use W/A/S/D para mover e construir ou "stop." para sair:'), nl,
    read(Input),
    processInput(Input, Map, Pos).

%processamento da entrada do jogador
processInput(stop, MapIn, _Pos) :-
    nl, write('Jogo interrompido! Mapa final:'), nl,
    printMap(MapIn), nl.

processInput(Input, MapIn, (Lat, Long)) :-
    move_offset(Input, (DL, DC)),
    NewLat is Lat + DL,
    NewLong is Long + DC,
    NewPos = (NewLat, NewLong),
    (   verifyCoord(NewPos, MapIn) ->
        getElement(NewPos, MapIn, tile(_, NewPos, _, _, Built)),
        (   Built = true ->
            write('Esse tile já está construído! Tente outro.'), nl,
            gameLoop(MapIn, (Lat, Long))
        ;   % Built = false
            getElement(NewPos, MapIn, tile(T, NewPos, PC, BC, _)),
            NewTile = tile(T, NewPos, PC, BC, true),
            update_tile(MapIn, NewPos, NewTile, MapOut),
            gameLoop(MapOut, NewPos)
        )
    ;  
        write('Movimento invalido! Tente novamente.'), nl,
        gameLoop(MapIn, (Lat, Long))
    ).

processInput(_, MapIn, (Lat, Long)) :-
    write('Comando invalido! Use W/A/S/D ou "stop."'), nl,
    gameLoop(MapIn, (Lat, Long)).
