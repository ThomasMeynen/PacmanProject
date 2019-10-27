module Controller where

import Pacman
import BreathFirstSearch

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
--import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step _ gstate@(GameState {pacman=(Pacman (x,y) d s), paused=Paused}) = return gstate
step _ gstate = 
  do  pacman' <- (pacmanstep (gstate))
      blinky' <- (blinkystep (gstate))
      pinky' <- (pinkystep (gstate))
      inky' <- (inkystep (gstate))
      clyde' <- (clydestep (gstate))
      return gstate {pacman = pacman', blinky = blinky', pinky = pinky', inky = inky', clyde = clyde'}  

pacmanstep :: GameState -> IO Pacman
pacmanstep gstate@(GameState {pacman=(Pacman (x,y) d s)}) = case d of
  N -> return (Pacman (x,y+s) d s)
  O -> return (Pacman (x+s,y) d s)
  Z -> return (Pacman (x,y-s) d s)
  W -> return (Pacman (x-s,y) d s)

blinkystep :: GameState -> IO Ghost
blinkystep gstate@(GameState {blinky =(Ghost (x,y) d s)}) = case d of
  N -> return (Ghost (x,y+s) d s)
  O -> return (Ghost (x+s,y) d s)
  Z -> return (Ghost (x,y-s) d s)
  W -> return (Ghost (x-s,y) d s)

pinkystep :: GameState -> IO Ghost
pinkystep gstate@(GameState {pinky =(Ghost (x,y) d s)}) = case d of
  N -> return (Ghost (x,y+s) d s)
  O -> return (Ghost (x+s,y) d s)
  Z -> return (Ghost (x,y-s) d s)
  W -> return (Ghost (x-s,y) d s)  

inkystep :: GameState -> IO Ghost
inkystep gstate@(GameState {inky =(Ghost (x,y) d s)}) = case d of
  N -> return (Ghost (x,y+s) d s)
  O -> return (Ghost (x+s,y) d s)
  Z -> return (Ghost (x,y-s) d s)
  W -> return (Ghost (x-s,y) d s)

clydestep :: GameState -> IO Ghost
clydestep gstate@(GameState {clyde =(Ghost (x,y) d s)}) = case d of
  N -> return (Ghost (x,y+s) d s)
  O -> return (Ghost (x+s,y) d s)
  Z -> return (Ghost (x,y-s) d s)
  W -> return (Ghost (x-s,y) d s)

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