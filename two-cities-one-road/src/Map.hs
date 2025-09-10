module Map where

import System.Random (randomRIO)

import Persistence (writeString, readString, writeInt, readInt)

-- | Gera um novo mapa aleatório
generateRandomMap :: IO ()
generateRandomMap = do
  new_matrix <- getRandomMatrix
  new_cash <- getRandomCash

  writeString cash_loc new_cash
  writeString matrix_loc new_matrix
  writeString coord_loc coord_default

-- | Retorna informações de dinheiro do jogador do mapa salvo
getPlayerCash :: IO String
getPlayerCash = readString cash_loc

-- | Retorna informações de localização do jogador do mapa salvo
getPlayerCoord :: IO String
getPlayerCoord = readString coord_loc

-- | Retorna informações de matriz do mapa salvo
getMapMatrix :: IO [String]
getMapMatrix = fmap sliceString (readString matrix_loc)

-- | ALtera informações de dinheiro do jogador do mapa salvo
setPlayerCash :: String -> IO ()
setPlayerCash str = writeString cash_loc str

-- | Altera informações de localização do jogador do mapa salvo
setPlayerCoord :: String -> IO ()
setPlayerCoord str = writeString coord_loc str

-- | Altera informações de matriz do mapa salvo
setMapMatrix :: [String] -> IO ()
setMapMatrix strs = writeString matrix_loc (unlines strs)

-- | Gera e retorna um mapa aleatório
getRandomMatrix :: IO String
getRandomMatrix = do
  middle <- sequence (replicate (lin*col - 2) (randomChar alphabet))
  return ("C" ++ middle ++ "c")
  where
    randomChar chars = do
      i <- randomRIO (0, length chars - 1)
      return (chars !! i)

-- | Gera e retorna um valor aleatório de dinheiro para o jogador
getRandomCash :: IO String
getRandomCash = do
  randomNum <- randomRIO (cash_min, cash_max) :: IO Int
  return (show randomNum)

-- | Corta uma String
sliceString :: String -> [String]
sliceString s = [take 11 (drop (i * 11) s) | i <- [0..4]]

-- Alfabeto de porcentagens
alphabet :: String
alphabet = "ppppPmmffl"

-- | Localização do estado de dinheiro do jogador na memória
cash_loc :: String
cash_loc = "data/cash.txt"

-- | Localização do estado de localização do jogador na memória
coord_loc :: String
coord_loc = "data/coord.txt"

-- | Localização da matriz do mapa na memória
matrix_loc :: String
matrix_loc = "data/matrix.txt"

-- | Número mínimo de dinheiro para iniciar uma partida
cash_min :: Int
cash_min = 50

-- | Número máximo de dinheiro para iniciar uma partida
cash_max :: Int
cash_max = 150

-- |
coord_default :: String
coord_default = "00"

-- Número de colunas
col :: Int
col = 5

-- Número de linhas
lin :: Int
lin = 11
