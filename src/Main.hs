module Main where

import Controller
import Pacman
import View


import Graphics.Gloss.Interface.IO.Game
    
main :: IO ()
main = do
            highscore <- readFile "highscore.txt"
            playIO (InWindow "Pacman" (xsize+2, ysize+40) (0, 0)) -- Or FullScreen
                black            -- Background color
                45               -- Frames per second
                (initialState highscore 0) -- Initial state
                view             -- View function
                input            -- Event function
                step             -- Step function

