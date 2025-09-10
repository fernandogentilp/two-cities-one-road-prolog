module Persistence where

import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.IO
import Control.Monad (unless)
import Control.Exception

-- | Verifica e cria os arquivos do programa
verifyAndCreateFiles :: IO ()
verifyAndCreateFiles = do
  createDirectoryIfMissing True "data"

  exists1 <- doesFileExist file1
  exists2 <- doesFileExist file2
  exists3 <- doesFileExist file3

  unless exists1 (writeFile file1 default_matrix)
  unless exists2 (writeFile file2 default_cash)
  unless exists3 (writeFile file3 default_coord)

  where
    file1 = "data/matrix.txt"
    file2 = "data/cash.txt"
    file3 = "data/coord.txt"

-- | Salva uma String em um arquivo
writeString :: FilePath -> String -> IO ()
writeString path content =
  withFile path WriteMode $ \handle -> do
    hPutStr handle content
    hFlush handle

-- | Salva um Int em um arquivo
writeInt :: FilePath -> Int -> IO ()
writeInt path content = writeString path (show content)

-- | Lê uma String de um arquivo
readString :: FilePath -> IO String
readString path =
  withFile path ReadMode $ \handle -> do
    content <- hGetContents handle
    length content `seq` return content

-- | Lê um Int de um arquivo
readInt :: FilePath -> IO Int
readInt path = do
  content <- readString path
  return (read content)

-- | Valor padrão da matriz de um mapa
default_matrix :: String
default_matrix = "CPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPc"

-- | Valor padrão de dinheiro inicial de um jogador
default_cash :: String
default_cash = "100"

-- | Valor padrão de coordenada inicial de um jogador
default_coord :: String
default_coord = "00"
