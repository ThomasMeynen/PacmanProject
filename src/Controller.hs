module Controller where

import Pacman
import BreathFirstSearch
import Collision

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
--import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step _ gstate@(GameState {paused=Paused}) = return gstate
step _ gstate@(GameState {paused=Playing,buffer=buffer,maze=m,pacman=pacman@(Pacman (x,y) d s),blinky=blinky,pinky=pinky,inky=inky,clyde=clyde })
  = return gstate {pacman=move (pacmantransformed p buffer),maze=(checkDot m x y), blinky=move (blinkytransformed blinky), pinky=move pinky, inky=move inky, clyde=move clyde} where
    p :: Pacman
    p = case checkDirection m d x y of
      Just (True) -> Pacman (x, y) d 0
      otherwise -> pacman
    pacmantransformed :: Pacman -> Buffer -> Pacman
    pacmantransformed pac (Buffer newdirection)= case checkDirection m newdirection x y of
        Just (False) -> Pacman (x,y) newdirection basespeed
        otherwise -> pac
    blinkytransformed :: Ghost -> Ghost
    blinkytransformed g@(Ghost (x,y) d s) = Ghost (x,y) (search m (g) (pacmanToPos p)) s


input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) Down _ _) gstate@(GameState {pacman=(Pacman (x,y) d s), paused=Playing})
  = -- Pacman changes its direction based on key pressed
    case c of
    'a' -> gstate { buffer=Buffer W}
    'w' -> gstate { buffer=Buffer N}
    's' -> gstate { buffer=Buffer Z}
    'd' -> gstate { buffer=Buffer O}
    otherwise -> gstate
inputKey (EventKey (SpecialKey KeySpace) Down _ _) gstate@(GameState {paused=p})
  =
    case p of
    Paused -> gstate {paused=Playing}
    Playing -> gstate {paused=Paused}
inputKey _ gstate = gstate -- Otherwise keep the same