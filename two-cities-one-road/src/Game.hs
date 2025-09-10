module Game where

import Types
import Graph
import MapUtils (stringsToMap, charToNextCoord, stringToCoord, coordToString,
                 mapToString, getElement, findNeighbors, verifyCoord)
import Map (getPlayerCash, getPlayerCoord, getMapMatrix, setPlayerCash,
            setPlayerCoord, setMapMatrix)

-- Loop de execução do jogo
gameLoop :: Char -> IO (Char, [String])
gameLoop key = do
  playerCoordStr <- getPlayerCoord
  playerCashStr <- getPlayerCash
  mapMatrixStr <- getMapMatrix

  let playerCoord = stringToCoord playerCoordStr
  let nextCoord = charToNextCoord key playerCoord
  let mapMatrix = stringsToMap mapMatrixStr
  let playerCash = read playerCashStr :: Int

  let available = getAvailableBuilds playerCoord mapMatrix
  let affordable = filter (\c -> buildingCost (getElement c mapMatrix) <= playerCash) available

  if null available then do
    return ('P', ["Você perdeu. Não existem painéis livres."])

  else if null affordable then do
    return ('P', ["Você perdeu. Não há dinheiro suficiente."])

  else if playerCash <= 0 then do
    return ('P', ["Você perdeu. O dinheiro acabou."])

  else do
    let tile = getElement nextCoord mapMatrix
    let cost = buildingCost tile
    let currentplayerCash = playerCash - cost

    -- Analisa orçamento 
    if currentplayerCash >= 0 then do 
      let newMap = buildOnMap nextCoord mapMatrix
      setMapMatrix (mapToString newMap)
      setPlayerCash (show currentplayerCash)
      setPlayerCoord (coordToString nextCoord)

      if arrivedAt nextCoord secondCity newMap then
        return ('G', ["Você ganhou. Cidade destino alcançada"])
      else
        return ('C',  ["Continua jogando..."])
    else
      return ('P', ["Você perdeu. Dinheiro insuficiente para construção"])
  where
    secondCity = (11, 5)

-- processa informacoes do painel para exibir na tela
printTile :: Tile -> [Coord] -> Char
printTile tile available
  | built tile = 'O'
  | location tile `elem` available = '*'
  | otherwise = case terrain tile of
      Plain -> 'p'
      C_Plain -> 'P'
      Lake -> 'l'
      C_Lake -> 'L'
      Mountain -> 'm'
      C_Mountain -> 'M'
      Forest -> 'f'
      C_Forest -> 'F'
      City -> 'c'
      C_City -> 'F'

-- funcao build retorna o mesmo tile, so que com atributo de construido
buildTile :: Coord -> Map -> Tile
buildTile coord mp = newTile 
  where
    current = getElement coord mp
    newTile = Tile (terrain current) (buildingCost current) (passingCost current) (location current) True 

-- atualiza o mapa quando algum tile for construido
buildOnMap :: Coord -> Map -> Map
buildOnMap (lat, long) mp =
  let row = mp !! lat
      newRow = take long row ++ [buildTile (lat, long) mp] ++ drop (long + 1) row
  in take lat mp ++ [newRow] ++ drop (lat + 1) mp

-- verifica se um painel esta disponivel, ou ja foi construido
canBuild :: Coord -> Map -> Bool
canBuild coord mp = 
  verifyCoord coord mp && not (built (getElement coord mp))

-- aplica o filtro de canBuild na lista de vizinhos de um painel
getAvailableBuilds :: Coord -> Map -> [Coord]
getAvailableBuilds coord mp = 
  filter (\c -> canBuild c mp) (findNeighbors coord mp)

-- verifica se chegou ao painel destino
arrivedAt :: Coord -> Coord -> Map -> Bool
arrivedAt current endCity mp = endCity `elem` (findNeighbors current mp)

-- funcao auxiliar para evitar erros ao ler inteiros da do jogador
readInputInt :: String -> Maybe Int
readInputInt s = case reads s of
  [(n, "")] -> Just n
  _-> Nothing

getNodes :: Map -> [Coord]
getNodes mp = map location (concat mp)

-- funcao que avalia o custo do trajeto do jogador
evaluatePath :: [Coord] -> Map -> Coord -> Coord -> IO()
evaluatePath playerPath mp start end = do
  let startCost = passingCost (getElement start mp)
  let endCost = passingCost (getElement end mp)
  let playerCost = sum (map (\coord -> passingCost (getElement coord mp)) playerPath) - startCost

  let nodes = getNodes mp
  let graph = buildGraph mp

  case bellmanFord nodes graph start of
    Left err -> putStrLn ("Erro ao computar caminho: " ++ err)
    Right (distances, preds) -> case lookup end distances of

      Nothing -> putStrLn "Erro: Destino inalcançável"
      
      Just optimalCost -> do
        let bestPath = buildPath start end preds
        putStrLn ("Sua ferrovia passa por: " ++ show playerPath)
        putStrLn ("Seus custos: " ++ show playerCost)
        putStrLn ("Ferrovia ideal (Bellman-Ford): " ++ show bestPath)
        putStrLn ("Custo ideal (Bellman-Ford): " ++ show optimalCost)
        
        let diff = playerCost - optimalCost
        if diff == 0
          then putStrLn "Trabalho perfeito! Você fez a ferrovia mais rápida possível!"
        
        else putStrLn ("Viagens custam " ++ show diff ++ " a mais que o melhor trajeto possível.")
