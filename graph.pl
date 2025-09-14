:- module(graph, [bellmanFord/4, buildGraph/2, listNodes/2, buildPath/4]).

:- use_module(mapUtils).
:- use_module(types).

% transforma o mapa em lista de paineis
flattenMap(MapNested, TilesFlat) :-
    append(MapNested, TilesFlat).

% custo de travessia de um painel
edgeCost(tile(_Terrain, _Location, PassingCost, _BuildingCost, _Built), PassingCost).

% vizinhos no grafo
graphNeighbors((X,Y), TilesFlat, Neighbors) :-
    DX = [0,1,0,-1],
    DY = [1,0,-1,0],
    findall((NX,NY),
      ( nth0(I, DX, DXI),
        nth0(I, DY, DYI),
        NX is X + DXI, NY is Y + DYI,
        member(tile(_, (NX,NY), _, _, _), TilesFlat)
      ),
    Neighbors).

% construcao do grafo
buildGraph(TilesFlat, Graph) :-
    findall((Coord1, Coord2, Cost),
        ( member(tile(_, Coord1, _, _, _), TilesFlat),
          graphNeighbors(Coord1, TilesFlat, Neighs),
          member(Coord2, Neighs),
          % get tile for Coord2
          member(tile(T2, Coord2, PC2, BC2, Built2), TilesFlat),
          edgeCost(tile(T2, Coord2, PC2, BC2, Built2), Cost)
        ),
    Graph).

% listagem dos vertices
listNodes(TilesFlat, Nodes) :-
    findall(Coord, member(tile(_, Coord, _PC, _BC, _Built), TilesFlat), Nodes).

% inicialzia distancias em infinito
initialize([], _, [], []).
initialize([N|Ns], Src, [(N, Dist)|Ds], Preds) :-
    ( N = Src -> Dist = 0 ; Dist = 1000000000 ),
    initialize(Ns, Src, Ds, Preds).

% obter distancia entre nos
getDist(Node, [(Node, D)|_], D) :- !.
getDist(Node, [_|Rest], D) :- getDist(Node, Rest, D).

% atualiza as distancias
updateDistAndPred(V, NewDist, PredV, DistIn, PredIn, DistOut, PredOut) :-
    select((V,OldD), DistIn, RestDist),
    ( NewDist < OldD ->
        DistOut = [(V,NewDist)|RestDist],
        % update predecessor
        (   delete_pairs(V, PredIn, PredTmp) -> true ; PredTmp = PredIn ),
        PredOut = [(V,PredV)|PredTmp]
    ;
        % no change
        DistOut = [(V,OldD)|RestDist],
        PredOut = PredIn
    ).

% remove pares
delete_pairs(_, [], []).
delete_pairs(V, [(V,_)|Xs], Ys) :- delete_pairs(V, Xs, Ys).
delete_pairs(V, [P|Xs], [P|Ys]) :- P = (U,_), U \= V, delete_pairs(V, Xs, Ys).

% relaxamento do grafo
relax([], State, State).
relax([(U,V,W)|Es], (Dist, Pred), (NewDistOut, NewPredOut)) :-
    getDist(U, Dist, DU),
    getDist(V, Dist, DV),
    Sum is DU + W,
    ( Sum < DV ->
        updateDistAndPred(V, Sum, U, Dist, Pred, NewDistTmp, NewPredTmp),
        relax(Es, (NewDistTmp, NewPredTmp), (NewDistOut, NewPredOut))
    ;
        relax(Es, (Dist, Pred), (NewDistOut, NewPredOut))
    ).

% relaxamento repetidas vezes
relaxNTimes(0, _, State, State).
relaxNTimes(N, Graph, State, FinalState) :-
    N > 0,
    relax(Graph, State, State1),
    N1 is N - 1,
    relaxNTimes(N1, Graph, State1, FinalState).

% checa se existe ciclo negativo
hasNegativeCycle([], _) :- false.
hasNegativeCycle([(U,V,W)|Es], Dist) :-
    getDist(U, Dist, DU),
    getDist(V, Dist, DV),
    Sum is DU + W,
    ( Sum < DV -> true ; hasNegativeCycle(Es, Dist) ).

% algoritmo de bellman-ford
bellmanFord(Nodes, Graph, Src, Result) :-
    initialize(Nodes, Src, Dist0, Pred0),
    length(Nodes, N),
    N1 is N - 1,
    relaxNTimes(N1, Graph, (Dist0, Pred0), (DistFinal, PredFinal)),
    ( hasNegativeCycle(Graph, DistFinal) ->
        Result = left('Negative cycle detected')
    ;
        Result = right(DistFinal, PredFinal)
    ).

% constroi o caminho
buildPath(Start, End, Preds, Path) :-
    buildPathAux(Start, End, Preds, [], Rev),
    reverse(Rev, Path).

buildPathAux(Start, Start, _Preds, Acc, [Start|Acc]).
buildPathAux(Start, Current, Preds, Acc, Path) :-
    Current \= Start,
    ( member((Current,Prev), Preds) ->
        buildPathAux(Start, Prev, Preds, [Current|Acc], Path)
    ;
        % No predecessor found — path doesn't exist
        Path = []
    ).
