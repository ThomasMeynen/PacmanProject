module Main where

import Controller
import Pacman
import View
    
import Graphics.Gloss.Interface.IO.Game
    
main :: IO ()
main = playIO (InWindow "Pacman" (xsize, ysize) (0, 0)) -- Or FullScreen
                black            -- Background color
                10               -- Frames per second
                initialState     -- Initial state
                view             -- View function
                input            -- Event function
                step             -- Step function