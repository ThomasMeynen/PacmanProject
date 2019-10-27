module Controller where

import Pacman
import BreathFirstSearch

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
--import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step _ gstate@(GameState {pacman=(Pacman (x,y) d s), paused=Paused}) = return gstate
step _ gstate@(GameState {pacman=(Pacman (x,y) d s)}) = case d of
    N -> return gstate {pacman=Pacman (x,y+s) d s}
    O -> return gstate {pacman=Pacman (x+s,y) d s}
    Z -> return gstate {pacman=Pacman (x,y-s) d s}
    W -> return gstate {pacman=Pacman (x-s,y) d s}



input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) Down _ _) gstate@(GameState {pacman=(Pacman (x,y) d s), paused=Playing})
  = -- Pacman changes its direction based on key pressed
    case c of
    'a' -> gstate { pacman=Pacman (x,y) W s}
    'w' -> gstate { pacman=Pacman (x,y) N s}
    's' -> gstate { pacman=Pacman (x,y) Z s}
    'd' -> gstate { pacman=Pacman (x,y) O s}
    otherwise -> gstate
inputKey (EventKey (SpecialKey KeySpace) Down _ _) gstate@(GameState {paused=p})
  =
    case p of
    Paused -> gstate {paused=Playing}
    Playing -> gstate {paused=Paused}
inputKey _ gstate = gstate -- Otherwise keep the same