module View where

import Graphics.Gloss
import Pacman

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = pictures ((pacmanview gstate) : ((pauseview gstate) : ((ghostview gstate) ++ (boardview gstate)))) where
    pauseview :: GameState -> Picture
    pauseview GameState {paused=p} = case p of
        Paused -> translate (-200) (200) (color green (text ("Paused")))
        Playing -> Blank
    pacmanview :: GameState -> Picture 
    pacmanview GameState {pacman = (Pacman (x,y) _ _)} = translate x y (color yellow (circleSolid ((fromIntegral fieldsize) / 2 - 1 )))
    ghostview gstate = []
    boardview GameState {maze = m} = concat (map (map transform) m) where
        transform f = case f of
            M -> color blue (rectangleUpperWire 20 20)
            D -> color white (circleSolid ((fromIntegral fieldsize) / 4))
            L -> Blank 

