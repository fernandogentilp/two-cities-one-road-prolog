module IOUtils where

import System.Console.Haskeline -- Para capturar comandos de teclado

-- | Limpa a tela
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"

-- Retorna em Char a tecla pressionada
getKey :: IO Char
getKey = do
  result <- runInputT defaultSettings (getInputChar "")
  case result of
    Just c -> return c
    Nothing -> return '\0'
