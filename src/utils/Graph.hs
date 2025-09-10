module Graph where

-- importa coisas necessarias de types e game
import Types (Coord, Map, Tile(..))
import MapUtils

type Node = Coord
type Weight = Int
type Edge = (Node, Node, Weight)
type Graph = [Edge]
type Distance = [(Node, Int)]
type Predecessor = [(Node, Node)]

-- cria um grafo que representa um mapa
buildGraph :: Map -> Graph
buildGraph mp = concatMap tileEdges allTiles
  where
    allTiles = concat mp
    tileEdges tile =
      let firstNode = location tile
          neighbors = findNeighbors firstNode mp
      in [(firstNode, secondNode, passingCost (getElement secondNode mp)) | secondNode <- neighbors, verifyCoord secondNode mp]

listNodes :: Map -> [Node]
listNodes mp = map location (concat mp)

-- um "infinito" grande o suficiente
infinity :: Int
infinity = 1000000000

-- inicializa as distancias: 0 para "raiz", infinito para os demais
initialize :: [Node] -> Node -> (Distance, Predecessor)
initialize nodes src = ([(n, if n == src then 0 else infinity) | n <- nodes], [])

-- retorna a distancia atual ate um vertice, infinito se nao for encontrado
getDist :: Node -> Distance -> Int
getDist _ [] = infinity
getDist v ((u,d):xs)
  | v == u = d
  | otherwise = getDist v xs

-- atualiza a distancia de v na lista (se for menor que a anterior)
updateDistAndPred :: Node -> Int -> Node -> Distance -> Predecessor -> (Distance, Predecessor)
updateDistAndPred v newDist predV [] predMap = ([], (v, predV) : predMap)
updateDistAndPred v newDist predV ((u,d):xs) predMap
  | u == v =
      let updatedDist = (u, min d newDist) : xs
          updatedPred = (v, predV) : filter ((/= v) . fst) predMap
      in (updatedDist, updatedPred)
  | otherwise =
      let (restDist, restPred) = updateDistAndPred v newDist predV xs predMap
      in ((u,d):restDist, restPred)

-- relaxa as arestas: atualiza a distancia entre vertices (crucial para o algoritmo)
relax :: Graph -> (Distance, Predecessor) -> (Distance, Predecessor)
relax [] state = state
relax ((u,v,w):es) (dist, predMap) =
  let du = getDist u dist
      dv = getDist v dist
  in if du + w < dv
        then let (newDist, newPred) = updateDistAndPred v (du + w) u dist predMap
             in relax es (newDist, newPred)
        else relax es (dist, predMap)

-- repete o relaxamento n vezes. o algoritmo exige que o grafo seja relaxado |V| -1 vezes, |V| = numero de vertices
relaxNTimes :: Int -> Graph -> (Distance, Predecessor) -> (Distance, Predecessor)
relaxNTimes 0 _ state = state
relaxNTimes n graph state = relaxNTimes (n - 1) graph (relax graph state)

-- verifica existencia de ciclos negativos
hasNegativeCycle :: Graph -> Distance -> Bool
hasNegativeCycle [] _ = False
hasNegativeCycle ((u,v,w):es) dist =
  let du = getDist u dist
      dv = getDist v dist
  in if du + w < dv
        then True
        else hasNegativeCycle es dist

-- algoritmo de bellman-ford. retorna distancia, se houver sucesso, ou uma mensagem de erro, se falhar.
-- Left e Right: vem do tipo Either, padrao de haskell, utilizado para capturar erros, possibilitando 2 tipos de retorno
bellmanFord :: [Node] -> Graph -> Node -> Either String (Distance, Predecessor)
bellmanFord nodes graph src =
  let initState = initialize nodes src
      (distFinal, predFinal) = relaxNTimes (length nodes - 1) graph initState
  in if hasNegativeCycle graph distFinal
        then Left "Ciclo negativo detectado!"
        else Right (distFinal, predFinal)

-- constroi o caminho percorrido
buildPath :: Node -> Node -> Predecessor -> [Node]
buildPath start end preds = reverse (go end)
  where
    go current
      | current == start = [start]
      | otherwise = case lookup current preds of
          Nothing -> []
          Just prev -> current : go prev
