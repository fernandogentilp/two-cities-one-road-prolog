:- module(graph, [bellmanFord/4]).

%-------------------------------------
% Funções utilitárias
%-------------------------------------

getNode(Coord, Map, Tile) :- member(Tile, Map), Tile = tile(Coord,_).

edgeCost(tile(_, Type), Cost) :-
    ( Type = grass -> Cost = 1
    ; Type = water -> Cost = 2
    ; Type = mountain -> Cost = 5
    ; Cost = 1
    ).

graphLoc((X,Y), Map) :-
    member(tile((X,Y),_), Map).

graphNeighbors((X,Y), Map, Neighbors) :-
    DX = [0,1,0,-1], DY = [1,0,-1,0],
    findall((NX,NY),
        (nth0(I, DX, DXI), nth0(I, DY, DYI),
         NX is X + DXI, NY is Y + DYI,
         graphLoc((NX,NY), Map)),
    Neighbors).

% --------------------------------------
% Construção do grafo
% --------------------------------------

% buildGraph(+Map, -Graph)
buildGraph(Map, Graph) :-
    findall((Coord1,Coord2,Cost),
        (member(Tile, Map),
         Tile = tile(Coord1,_),
         graphNeighbors(Coord1, Map, Neighbors),
         member(Coord2, Neighbors),
         getNode(Coord2, Map, Tile2),
         edgeCost(Tile2, Cost)
        ),
    Graph).

% listNodes(+Map, -Nodes)
listNodes(Map, Nodes) :-
    findall(Coord, member(tile(Coord,_), Map), Nodes).

% --------------------------------------
% Bellman-Ford
% --------------------------------------

infinity(1000000000).

initialize([], _, [], []).
initialize([N|Ns], Src, [(N,Dist)|Ds], Preds) :-
    (N = Src -> Dist = 0 ; infinity(Dist)),
    initialize(Ns, Src, Ds, Preds).

getDist(_, [], Dist) :- infinity(Dist).
getDist(V, [(U,D)|_], D) :- V == U, !.
getDist(V, [_|Xs], D) :- getDist(V, Xs, D).

updateDistAndPred(V, NewDist, PredV, DistIn, PredIn, DistOut, PredOut) :-
    ( select((V,OldD), DistIn, RestDist) ->
        ( NewDist < OldD ->
            % atualiza distância e predecessor
            DistOut = [(V,NewDist)|RestDist],
            delete_pairs(V, PredIn, PredTmp),
            PredOut = [(V,PredV)|PredTmp]
        ;
            % mantém valor antigo
            DistOut = [(V,OldD)|RestDist],
            PredOut = PredIn
        )
    ;
        % se não existe o nó na lista (caso inesperado), adiciona-o
        DistOut = [(V,NewDist)|DistIn],
        delete_pairs(V, PredIn, PredTmp),
        PredOut = [(V,PredV)|PredTmp]
    ).

delete_pairs(_, [], []).
delete_pairs(V, [(V,_)|Xs], Ys) :- delete_pairs(V, Xs, Ys).
delete_pairs(V, [P|Xs], [P|Ys]) :- delete_pairs(V, Xs, Ys).

relax([], State, State).
relax([(U,V,W)|Es], (Dist,Pred), (FinalDist,FinalPred)) :-
    getDist(U, Dist, DU),
    getDist(V, Dist, DV),
    Sum is DU + W,
    ( Sum < DV ->
        updateDistAndPred(V, Sum, U, Dist, Pred, NewDist, NewPred),
        relax(Es, (NewDist,NewPred), (FinalDist,FinalPred))
    ;
        relax(Es, (Dist,Pred), (FinalDist,FinalPred))
    ).

relaxNTimes(0, _, State, State).
relaxNTimes(N, Graph, State, Final) :-
    N > 0,
    relax(Graph, State, NewState),
    N1 is N-1,
    relaxNTimes(N1, Graph, NewState, Final).

hasNegativeCycle([], _) :- false.
hasNegativeCycle([(U,V,W)|Es], Dist) :-
    getDist(U, Dist, DU),
    getDist(V, Dist, DV),
    Sum is DU + W,
    ( Sum < DV -> true ; hasNegativeCycle(Es, Dist) ).

bellmanFord(Nodes, Graph, Src, Result) :-
    initialize(Nodes, Src, DistInit, PredInit),
    length(Nodes, L),
    NRelax is L - 1,
    relaxNTimes(NRelax, Graph, (DistInit,PredInit), (DistFinal,PredFinal)),
    (hasNegativeCycle(Graph, DistFinal) ->
        Result = left('Ciclo negativo detectado!')
    ;
        Result = right(DistFinal, PredFinal)
    ).

buildPath(Start, End, Preds, Path) :-
    buildPathAux(Start, End, Preds, [], RevPath),
    reverse(RevPath, Path).

buildPathAux(Start, Start, _, Acc, [Start|Acc]).
buildPathAux(Start, Current, Preds, Acc, Path) :-
    Current \= Start,
    (member((Current,Prev), Preds) ->
        buildPathAux(Start, Prev, Preds, [Current|Acc], Path)
    ;
        Path = []
    ).

% -----------------------------
% Exemplo de mapa
% -----------------------------
example_map([
    tile((0,0), grass),
    tile((0,1), water),
    tile((1,0), grass),
    tile((1,1), mountain),
    tile((2,0), grass),
    tile((2,1), grass)
]).

% -----------------------------
% Predicado main para teste
% -----------------------------
main :-
    % Pega o mapa de exemplo
    example_map(Map),
    
    % Constroi o grafo
    buildGraph(Map, Graph),
    
    % Lista os nós
    listNodes(Map, Nodes),
    
    % Define o nó inicial
    Src = (0,0),
    
    % Executa Bellman-Ford
    bellmanFord(Nodes, Graph, Src, Result),
    
    % Mostra resultado
    ( Result = left(Msg) ->
        writeln('Erro:'), writeln(Msg)
    ; Result = right(Dist, Pred) ->
        writeln('Distâncias:'), writeln(Dist),
        writeln('Predecessores:'), writeln(Pred),
        
        % Testa buildPath: caminho do (0,0) até (2,1)
        End = (2,1),
        buildPath(Src, End, Pred, Path),
        writeln('Caminho do (0,0) até (2,1):'), writeln(Path)
    ).
main.
