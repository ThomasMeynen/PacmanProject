module Controller where

import Pacman
import BreathFirstSearch
import Collision

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
--import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step _ gstate@(GameState {lives=l, 
                          score=points, 
                          highscore = hscore,
                          paused=Playing,
                          buffer=buffer,
                          maze=m,
                          pacman=pacman@(Pacman (x,y) d s),
                          blinky=blinky,
                          pinky=pinky,
                          inky=inky,
                          clyde=clyde})
  | hitAGhost = return gstate {paused=Dood}
  | won && fst eaten == 10 = return (initialState hscore (points+10))
  | otherwise   = return gstate {pacman=move (pacmanturn pacmanwall buffer),
                                 score=points+(fst eaten),
                                 maze=snd eaten, 
                                 blinky=move (blinkytransformed blinky), 
                                 pinky=move (pinkytransformed pinky),
                                 inky=move (inkytransformed inky),
                                 clyde=move (clydetransformed clyde)}
      where
      won :: Bool
      won | points == 0 = False
          | otherwise = (mod points 2990) == 0
      eaten :: (Int, Maze)
      eaten = checkDot m x y
      pacmandir :: Direction 
      pacmandir = d
      pacmanwall :: Pacman --Checks if pacman walks into a wall or out of bounds and returns an updated pacman base on results
      pacmanwall = case checkDirection m d x y of
        Just (True) -> Pacman (outOfBounds x, y) d 0
        otherwise -> Pacman (outOfBounds x, y) d s
      pacmanturn :: Pacman -> Buffer -> Pacman--checks if pacman can turn, returns an updated pacman based on results
      pacmanturn pac (Buffer newdirection)= case checkDirection m newdirection x y of
          Just (False) -> Pacman (x,y) newdirection basespeed
          Nothing -> case d of
            N -> if newdirection == Z then Pacman (x,y) newdirection basespeed else pac
            Z -> if newdirection == N then Pacman (x,y) newdirection basespeed else pac
            O -> if newdirection == W then Pacman (x,y) newdirection basespeed else pac
            W -> if newdirection == O then Pacman (x,y) newdirection basespeed else pac
          otherwise -> pac
      blinkytransformed :: Ghost -> Ghost
      blinkytransformed g@(Ghost (x,y) d s) = Ghost (x,y) (search m (g) (pacmanToPos pacmanwall (0,0))) s
      pinkytransformed :: Ghost -> Ghost
      pinkytransformed g@(Ghost (x,y) d s) = Ghost (x,y) (search m (g) (pacmanToPos pacmanwall (goTo pacmandir))) s
      inkytransformed :: Ghost -> Ghost
      inkytransformed g@(Ghost (x,y) d s) = Ghost (x,y) (search m (g) (pacmanToPos pacmanwall (0,0))) s -- deze moet nog random
      clydetransformed :: Ghost -> Ghost
      clydetransformed g@(Ghost (x,y) d s)  | fst (distance (ghostToPos g) (pacmanToPos pacmanwall (0,0))) <= 100 = Ghost (x,y) (search m (g) (1,29)) s
                                            | otherwise = Ghost (x,y) (search m (g) (pacmanToPos pacmanwall (0,0))) s
      hitAGhost :: Bool
      hitAGhost = (ghostCollision pacman blinky) || (ghostCollision pacman pinky) || (ghostCollision pacman inky) || (ghostCollision pacman clyde)
step _ gstate@(GameState {animation=a,
                          lives=l, 
                          score=points, 
                          highscore = highscore,
                          paused=Dood,
                          buffer=buffer,
                          maze=m,
                          pacman=pacman@(Pacman (x,y) d s),
                          blinky=blinky,
                          pinky=pinky,
                          inky=inky,
                          clyde=clyde}) | a>1 = return gstate {animation=a-1} 
                                        | l>1 = return (GameState 50
                                                        (l-1) 
                                                        points 
                                                        highscore
                                                        Paused 
                                                        (Buffer W) 
                                                        m 
                                                        (Pacman (10,(-40)) W basespeed)
                                                        (Ghost (0,80) W basespeed)
                                                        (Ghost (80,0) O basespeed)
                                                        (Ghost (80,80) W basespeed)
                                                        (Ghost (80,-40) W basespeed)) 
                                        | otherwise = do  writeFile "highscore.txt" (show points)
                                                          return gstate {lives=(l-1),paused=GameOver}
step _ gstate = return gstate 

input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) Down _ _) gstate@(GameState {pacman=(Pacman (x,y) d s), paused=Playing})
  = -- Buffer changes based on key pressed
    case c of
    'a' -> gstate { buffer=Buffer W}
    'w' -> gstate { buffer=Buffer N}
    's' -> gstate { buffer=Buffer Z}
    'd' -> gstate { buffer=Buffer O}
    otherwise -> gstate
inputKey (EventKey (SpecialKey KeySpace) Down _ _) gstate@(GameState {paused=p})
  = -- If spacebar is pressed pause the game
    case p of
    Paused -> gstate {paused=Playing}
    Playing -> gstate {paused=Paused}
    otherwise -> gstate
inputKey _ gstate = gstate -- Otherwise keep the same