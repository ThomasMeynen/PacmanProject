module Pacman where

data GameState = GameState {
    paused :: Paused,
    maze :: Maze,
    pacman :: Pacman,
    blinky :: Ghost,
    pinky :: Ghost,
    inky :: Ghost,
    clyde :: Ghost
    }

data Paused = Playing|Paused
data Pacman = Pacman (Float, Float) Direction Speed
data Ghost  = Ghost (Float, Float) Direction Speed
data Field  = M|D|L --Muren, Dots, Leeg
data Direction = N|O|Z|W --Noord, Oost, Zuid, West


type Row   = [Field]
type Maze  = [Row]
type Speed = Float

xfields :: Int
xfields = 28
yfields :: Int
yfields = 31
fieldsize :: Int
fieldsize = 20
xsize :: Int
xsize = xfields * fieldsize
ysize :: Int
ysize = yfields * fieldsize


initialState :: GameState
initialState = GameState Paused
                         (replicate yfields (replicate xfields L))
                         (Pacman (0,0) W 3)
                         (Ghost (0,0) W 2)
                         (Ghost (0,0) W 2)
                         (Ghost (0,0) W 2)
                         (Ghost (0,0) W 2)
