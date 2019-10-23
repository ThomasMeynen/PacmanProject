module Pacman where

data GameState = GameState {
    maze :: Maze,
    pacman :: Pacman,
    blinky :: Ghost,
    pinky :: Ghost,
    inky :: Ghost,
    clyde :: Ghost
    }

data Pacman = Pacman (Float, Float) Direction Speed
data Field  = M|D|L
data Direction = N|O|Z|W
data Ghost  = Ghost (Float, Float) Direction Speed

type Row   = [Field]
type Maze  = [Row]
type Speed = Float

xsize :: Int
xsize = 400
ysize :: Int
ysize = 400

initialState :: GameState
initialState = GameState replicate 31 (replicate 27 M)
                         (Pacman (-100,-100) W 3)
                         (Ghost (1.0,0) W 2)
                         (Ghost (1.0,0) W 2)
                         (Ghost (1.0,0) W 2)
                         (Ghost (1.0,0) W 2)
