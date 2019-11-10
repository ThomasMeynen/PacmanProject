module View where

import Graphics.Gloss
import Pacman
import Collision


view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure (GameState {animation=a,
                     lives=l, 
                     score=points, 
                     highscore=highpoints,
                     paused=p,
                     buffer=buffer,
                     maze=m,
                     pacman=pacman@(Pacman (x,y) d s),
                     blinky=blinky,
                     pinky=pinky,
                     inky=inky,
                     clyde=clyde,
                     generator=gen})
  = pictures (boardview++ghostview++[highscoreview]++[pacmanview]++(livesview l)++[scoreview]++[pauseview]) where
    scoreview :: Picture
    scoreview = translate ((fromIntegral xsize / 2)*(-1)) ((fromIntegral ysize / 2)+3) (scale 0.15 0.15 (color white (text ("Score: "++(show points)))))
    highscoreview :: Picture
    highscoreview = translate ((fromIntegral xsize / 2 - 200)*(-1)) ((fromIntegral ysize / 2)+3) (scale 0.15 0.15 (color white (text ("Highscore: "++(show (max points (read highpoints :: Int)))))))
    livesview :: Int -> [Picture]
    livesview 0 = [translate ((fromIntegral xsize / 2)-140) ((fromIntegral ysize / 2)+3) (scale 0.15 0.15 (color white (text ("Lives:"))))]
    livesview x = (translate ((fromIntegral xsize / 2)-105+(30*fromIntegral x)) ((fromIntegral ysize / 2)+10) (color yellow (circleSolid ((fromIntegral fieldsize) / 2 - 1 )))) : livesview (x-1)
    pauseview :: Picture
    pauseview = case p of
        Paused -> translate (-220) (200) (color green (text ("Paused")))
        GameOver -> translate (-50) (-47) (scale 0.15 0.15 (color red (text ("Game Over"))))
        otherwise -> Blank
    pacmanview :: Picture 
    pacmanview = case p of 
        GameOver -> Blank
        otherwise -> draw a pacman yellow
    ghostview :: [Picture]
    ghostview = [draw 0 blinky red, draw 0 pinky rose, draw 0 inky aquamarine, draw 0 clyde orange]
    boardview :: [Picture]
    boardview = dots m 0 0 where --iterates over the entire board, makes pictures and translates the correctly for every wall and dot
        dots :: Maze -> Int -> Int -> [Picture]
        dots m x y 
            | yfields <= y = []
            | xfields <= x = dots m 0 (y+1)
            | otherwise = [translate (gridToPos x xfields) ((-1)*(gridToPos y yfields)) (field ((m!!y)!!x))] ++ dots m (x+1) y where
                field :: Field -> Picture
                field (M) = color blue (rectangleWire 20 20)
                field (D) = color white (circleSolid ((fromIntegral fieldsize)/7))
                field (L) = Blank 