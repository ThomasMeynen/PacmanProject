module View where

import Graphics.Gloss
import Pacman

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = pictures ((pacmanview gstate) : ((pauseview gstate) : ((ghostview gstate) ++ (boardview gstate)))) where
    pauseview :: GameState -> Picture
    pauseview GameState {paused=p}
        =
            case p of
            Paused -> translate (-200) (200) (color green (text ("Paused")))
            Playing -> Blank
    pacmanview GameState {pacman = (Pacman (x,y) _ _)} = translate x y (color yellow (circleSolid (9)))
    ghostview gstate = []
    boardview gstate = []