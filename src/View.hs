module View where

import Graphics.Gloss
import Pacman

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = pictures ((pacmanview gstate) : ((ghosts gstate) ++ (board gstate))) where
    pacmanview GameState {pacman = ( Pacman (x,y) _ _)} = translate x y (color yellow (circleSolid (9)))
    ghosts gstate = []
    board GameState {maze=M} = []