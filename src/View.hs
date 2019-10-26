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
    boardview GameState {maze = m} = dots m 0 0 where
        dots :: Maze -> Int -> Int -> [Picture]
        dots m x y 
            | yfields <= y = []
            | xfields <= x = dots m 0 (y+1)
            | otherwise = [translate (transform x xfields) (transform y yfields) (field ((m!!y)!!x))] ++ dots m (x+1) y where
                field :: Field -> Picture
                field (M) = color blue (rectangleUpperWire 20 20)
                field (D) = color white (circleSolid ((fromIntegral fieldsize) / 4))
                field (L) = Blank 
                transform :: Int -> Int -> Float
                transform pos length = fromIntegral ((pos - (half length)) * fieldsize) :: Float
                half :: Int -> Int
                half int = round ((fromIntegral int :: Float) / 2)