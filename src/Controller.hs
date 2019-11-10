module Controller where

import Pacman
import GhostAI
import Collision

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step _ gstate@(GameState {lives=l, 
                          score=points, 
                          highscore = hscore,
                          paused=Playing, --only goes into this step if the game is playing
                          buffer=buffer,
                          maze=m,
                          pacman=pacman@(Pacman (x,y) d s),
                          blinky=blinky,
                          pinky=pinky,
                          inky=inky,
                          clyde=clyde,
                          generator=gen})
  | hitAGhost = return gstate {paused=Dood} --If you hit a ghost set paused to dood and play an animation
  | won && fst eaten == 10 = return (initialState hscore (points+10) gen) --if you've eaten all the dots start a new level
  | otherwise   = return gstate {pacman=move (pacmanturn pacmanwall buffer), --Otherwise move everything 1 step
                                 score=points+(fst eaten),
                                 maze=snd eaten, 
                                 blinky=move (blinkytransformed blinky), 
                                 pinky=move (pinkytransformed pinky),
                                 inky=move (inkytransformed inky),
                                 clyde=move (clydetransformed clyde),
                                 generator=newgen}
      where
      won :: Bool --checks if you have eaten all the dots in the level
      won | points == 0 = False
          | otherwise = (mod points 2990) == 0
      eaten :: (Int, Maze) --gets a number and an updated maze base on if you have eaten a dot or not
      eaten = checkDot m x y
      pacmandir :: Direction --gets pacmans direction
      pacmandir = d
      pacmanwall :: Pacman --Checks if pacman walks into a wall or out of bounds and returns an updated pacman base on results
      pacmanwall = case checkDirection m d x y of
        Just (True) -> Pacman (outOfBounds x, y) d 0
        otherwise -> Pacman (outOfBounds x, y) d s
      pacmanturn :: Pacman -> Buffer -> Pacman--checks if pacman can turn, returns an updated pacman based on results
      pacmanturn pac (Buffer newdirection)= case checkDirection m newdirection x y of
          Just (False) -> Pacman (x,y) newdirection basespeed --if no wall is seen in buffer direction update pacmans direction
          Nothing -> case d of --If pacman is not in the middle of a tile he can still turn backwards.
            N -> if newdirection == Z then Pacman (x,y) newdirection basespeed else pac
            Z -> if newdirection == N then Pacman (x,y) newdirection basespeed else pac
            O -> if newdirection == W then Pacman (x,y) newdirection basespeed else pac
            W -> if newdirection == O then Pacman (x,y) newdirection basespeed else pac
          otherwise -> pac
      blinkytransformed :: Ghost -> Ghost --gets blinkys ai and updates blinkys direction accordingly
      blinkytransformed g@(Ghost (x,y) d s) = Ghost (x,y) (search m (g) (pacmanToPos pacmanwall (0,0))) s
      pinkytransformed :: Ghost -> Ghost --gets pinkys ai and updates pinkys direction accordingly
      pinkytransformed g@(Ghost (x,y) d s) = Ghost (x,y) (search m (g) (pacmanToPos pacmanwall (goTo pacmandir))) s
      inkytransformed :: Ghost -> Ghost --gets a random number and a random direction
      inkytransformed g@(Ghost (x,y) d s) = Ghost (x,y) (randomDirection m (fst generatednumber) g) s
      clydetransformed :: Ghost -> Ghost --checks if pacman is close and moves accordingly
      clydetransformed g@(Ghost (x,y) d s)  | fst (distance (ghostToPos g) (pacmanToPos pacmanwall (0,0))) <= 100 = Ghost (x,y) (search m (g) (1,29)) s
                                            | otherwise = Ghost (x,y) (search m (g) (pacmanToPos pacmanwall (0,0))) s
      generatednumber :: (Int, StdGen)
      generatednumber = getRandomNumber gen 50 --getting a new random number
      newgen :: StdGen
      newgen = snd(generatednumber) --getting our updated rng
      hitAGhost :: Bool -- checks if pacman is hitting any ghost in this frame.
      hitAGhost = (ghostCollision pacman blinky) || (ghostCollision pacman pinky) || (ghostCollision pacman inky) || (ghostCollision pacman clyde)
step _ gstate@(GameState {animation=a,
                          lives=l, 
                          score=points, 
                          highscore = highscore,
                          paused=Dood,--This is our animation step
                          buffer=buffer,
                          maze=m,
                          pacman=pacman@(Pacman (x,y) d s),
                          blinky=blinky,
                          pinky=pinky,
                          inky=inky,
                          clyde=clyde,
                          generator=gen}) | a>1 = return gstate {animation=a-1} --It either goes through the animation
                                          | l>1 = return (GameState 50 --subtracts 1 life and resets the board
                                                          (l-1) 
                                                          points 
                                                          highscore
                                                          Paused 
                                                          (Buffer W) 
                                                          m 
                                                          (Pacman (10,(-40)) W basespeed)
                                                          (Ghost (10,80) W basespeed)
                                                          (Ghost (90,0) O basespeed)
                                                          (Ghost (90,80) W basespeed)
                                                          (Ghost (90,-40) W basespeed)
                                                          gen) 
                                          | otherwise = do  writeFile "data/highscore.txt" (show (max points (read highscore :: Int)))
                                                            return gstate {lives=(l-1),paused=GameOver} --or ends the game
step _ gstate = return gstate 

input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) Down _ _) gstate@(GameState {pacman=(Pacman (x,y) d s), paused=Playing})
  = -- Buffer changes based on wasd key pressed
    case c of
    'a' -> gstate { buffer=Buffer W}
    'w' -> gstate { buffer=Buffer N}
    's' -> gstate { buffer=Buffer Z}
    'd' -> gstate { buffer=Buffer O}
    otherwise -> gstate
inputKey (EventKey (SpecialKey key) Down _ _) gstate@(GameState {paused=p})
  = -- Buffer changes based on arrow keys aswell
    case key of
    KeyLeft -> gstate { buffer=Buffer W}
    KeyUp -> gstate { buffer=Buffer N}
    KeyDown -> gstate { buffer=Buffer Z}
    KeyRight -> gstate { buffer=Buffer O}
    KeySpace -> case p of -- If spacebar is pressed pause or unpause the game
      Paused -> gstate {paused=Playing}
      Playing -> gstate {paused=Paused}
      otherwise -> gstate
    otherwise -> gstate
inputKey _ gstate = gstate -- Otherwise keep the same