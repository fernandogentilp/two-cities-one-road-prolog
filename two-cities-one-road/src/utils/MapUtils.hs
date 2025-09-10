module MapUtils where

import Types
import Data.Char (toUpper, ord, chr)
import Map (getPlayerCoord)

-- pega um tile a partir das coordenadas
getElement :: Coord -> Map -> Tile
getElement (lat, long) mp = mp !! lat !! long

verifyCoord :: Coord -> Map -> Bool
verifyCoord (lat, long) mp =
  lat >= 0 && lat < length mp &&
  long >= 0 && long < length (head mp)

-- retorna os 4 vizinhos (norte sul leste oeste), independente de existirem
findNeighbors :: Coord -> Map -> [Coord]
findNeighbors (lat, long) _ = 
  [(lat - 1, long), (lat + 1, long), (lat, long - 1), (lat, long + 1)]

-- | Converte String em Coord
stringToCoord :: String -> Coord
stringToCoord [x, y] = (hexCharToInt x, hexCharToInt y)

-- | Recebe key (Char) e Coord antiga e converte em próxima Coord
charToNextCoord :: Char -> Coord -> Coord
charToNextCoord c (x, y) = case c of
  'W' -> (x + 1, y + 1)
  'A' -> (x - 1, y + 1)
  'S' -> (x - 1, y - 1)
  'D' -> (x + 1, y - 1)
  _ -> (x, y)

-- | Converte [String] em Map
stringsToMap :: [String] -> Map
stringsToMap strs =  map (map charToTile) (zipWith (\i -> zipWith (\j c -> (i,j,c)) [0..]) [0..] strs)
  where
    charToTile (i,j,c) = case c of
      'p' -> Tile Plain 10 1 (i,j) False
      'P' -> Tile C_Plain 10 1 (i,j) True
      'l' -> Tile Lake 20 2 (i,j) False
      'L' -> Tile C_Lake 20 2 (i,j) True
      'm' -> Tile Mountain 30 3 (i,j) False
      'M' -> Tile C_Mountain 30 3 (i,j) True
      'f' -> Tile Forest 15 2 (i,j) False
      'F' -> Tile C_Forest 15 2 (i,j) True
      'c' -> Tile City 0 1 (i,j) False
      'C' -> Tile C_City 0 1 (i,j) True
      _ -> Tile Plain 10 1 (i,j) False

-- | Converte Map em [String]
mapToString :: Map -> [String]
mapToString mp = map (map tileToChar) mp
  where
    tileToChar tile
      | built tile = 'O'
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
          C_City -> 'C'

-- | Converte Coord em String
coordToString :: Coord -> String
coordToString (x, y) = [intToHexChar x, intToHexChar y]
  where
    intToHexChar :: Int -> Char
    intToHexChar n
      | n >= 0 && n <= 9 = chr (ord '0' + n)
      | n >= 10 && n <= 15 = chr (ord 'A' + n - 10)
      | otherwise = error "Coordinate value out of hex range (0-15)"


-- Função para converter um caractere hexadecimal em um inteiro
hexCharToInt :: Char -> Int
hexCharToInt c
    | c >= '0' && c <= '9' = ord (toUpper c) - ord '0'
    | c >= 'A' && c <= 'F' = ord (toUpper c) - ord 'A' + 10
    | otherwise = error " "
