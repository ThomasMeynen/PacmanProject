module Controller where

import Pacman

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
--import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step _ gstate = return gstate

input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey _ gstate = gstate 