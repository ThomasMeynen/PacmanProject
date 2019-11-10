module Main where

import Controller
import Pacman
import View

import System.Random
import Graphics.Gloss.Interface.IO.Game
    
main :: IO ()
main = do
            highscore <- readFile "data/highscore.txt"
            g <- getStdGen
            playIO (InWindow "Pacman" (xsize+2, ysize+40) (0, 0)) -- Or FullScreen
                black            -- Background color
                45               -- Frames per second
                (initialState highscore 0 g) -- Initial state
                view             -- View function
                input            -- Event function
                step             -- Step function

