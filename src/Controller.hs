module Controller where

import Pacman

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
--import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step _ gstate@(GameState {pacman=(Pacman (x,y) d s)}) = case d of
    N -> return gstate {pacman=Pacman (x,y+s) d s}
    O -> return gstate {pacman=Pacman (x+s,y) d s}
    Z -> return gstate {pacman=Pacman (x,y-s) d s}
    W -> return gstate {pacman=Pacman (x-s,y) d s}


input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey _ gstate = gstate 