module Main where

import Persistence (verifyAndCreateFiles)
import Interfaces (homeScreen, gameScreen, endScreen)
import Map (generateRandomMap)
import Game (gameLoop)
import IOUtils (getKey, clearScreen)

-- | Função principal do programa
main :: IO ()
main = do
  verifyAndCreateFiles
  runHomeScreen

-- Executa a tela inicial
runHomeScreen :: IO ()
runHomeScreen = do
  clearScreen
  putStr homeScreen
  key <- getKey
  case key of
    'a' -> runGameScreen
    'g' -> do
      generateRandomMap
      runGameScreen
    'q' -> return ()
    _ -> runHomeScreen

-- | Executa tela de jogo
runGameScreen :: IO ()
runGameScreen = do
  clearScreen
  screen <- gameScreen
  putStr screen
  key <- getKey
  case key of
    'q' -> runHomeScreen
    'w' -> handleMovement 'w'
    'a' -> handleMovement 'a'
    's' -> handleMovement 's'
    'd' -> handleMovement 'd'
    _ -> runGameScreen

-- | Executa a tela de fim de jogo
runEndScreen :: String -> [String] -> IO ()
runEndScreen status info = do
  clearScreen
  let screen = endScreen status info
  putStr screen
  key <- getKey
  case key of
    'q' -> runHomeScreen
    _ -> runEndScreen status info

-- | Executa a lógica de jogo para cada direção ou encerra, quando acabar
handleMovement :: Char -> IO ()
handleMovement direction = do
  (status, infos) <- gameLoop direction
  case status of
    'P' -> runEndScreen "Derrota" infos
    'G' -> runEndScreen "Vitória" infos
    'C' -> runGameScreen
