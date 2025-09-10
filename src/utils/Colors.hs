module Colors where

import System.Console.ANSI
  ( setSGR,
    Color(..),
    ColorIntensity(..),
    ConsoleLayer(..),
    SGR(..),
    setSGRCode
  )
import Control.Monad (when)

-- | Muda a cor do texto no terminal
colorize :: Color -> String -> String
colorize color str =
  setSGRCode [SetColor Foreground Vivid color] 
    ++ str 
    ++ setSGRCode [Reset]

-- Pré definição de cores
green, blue, brown, darkGreen, white :: String -> String
green   = colorize Green
blue    = colorize Blue
brown   = colorizeWithIntensity Dull Red
darkGreen   = colorizeWithIntensity Dull Green
white   = colorize White

-- Função auxiliar para especificar intensidade da cor
colorizeWithIntensity :: ColorIntensity -> Color -> String -> String
colorizeWithIntensity intensity color str =
  setSGRCode [SetColor Foreground intensity color]
    ++ str
    ++ setSGRCode [Reset]
