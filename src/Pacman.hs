module Pacman where

import Graphics.Gloss
data GameState = GameState {
    lives :: Int,
    score :: Int,
    paused :: Paused,
    buffer :: Buffer,
    maze :: Maze,
    pacman :: Pacman,
    blinky :: Ghost,
    pinky :: Ghost,
    inky :: Ghost,
    clyde :: Ghost
    }

type Row   = [Field]
type Maze  = [Row]
type Speed = Float
data Paused = Playing|Paused|GameOver
  deriving (Eq)
data Pacman = Pacman (Float, Float) Direction Speed
data Ghost  = Ghost (Float, Float) Direction Speed
data Field  = M|D|L --Muren, Dots, Leeg
  deriving (Eq)
data Direction = N|O|Z|W --Noord, Oost, Zuid, West
  deriving (Eq)
data Buffer    = Buffer Direction

class Movables a where
  move :: a -> a
  draw :: a -> Color -> Picture

instance Movables Pacman where
  move (Pacman (x,y) d s) = case d of
    N -> Pacman (x,y+s) d s
    O -> Pacman (x+s,y) d s
    Z -> Pacman (x,y-s) d s
    W -> Pacman (x-s,y) d s
  draw (Pacman (x,y) _ _) c = translate x y (color c (circleSolid ((fromIntegral fieldsize) / 2 - 1 )))

instance Movables Ghost where
  move (Ghost (x,y) d s) = case d of
    N -> Ghost (x,y+s) d s
    O -> Ghost (x+s,y) d s
    Z -> Ghost (x,y-s) d s
    W -> Ghost (x-s,y) d s
  draw (Ghost (x,y) _ _) c = translate x y (color c (circleSolid ((fromIntegral fieldsize) / 2 - 1 )))

basespeed :: Speed
basespeed = 2
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

createMaze :: [[Char]] -> Maze
createMaze string = map createRows string where
    createRows :: [Char] -> Row
    createRows [] = []
    createRows (x:xs)  
        | x == '#' = M : (createRows xs)
        | x == '.' = D : (createRows xs)
        | otherwise = L : (createRows xs)

initialState :: GameState
initialState = GameState 3
                         0
                         Paused
                         (Buffer W)
                         (createMaze          [ "############################",
                                                "#............##............#",
                                                "#.####.#####.##.#####.####.#",
                                                "#.####.#####.##.#####.####.#",
                                                "#.####.#####.##.#####.####.#",
                                                "#..........................#",
                                                "#.####.##.########.##.####.#",
                                                "#.####.##.########.##.####.#",
                                                "#......##....##....##......#",
                                                "######.#####.##.#####.######",
                                                "######.#####.##.#####.######",
                                                "######.##..........##.######",
                                                "######.##.########.##.######",
                                                "######.##.########.##.######",
                                                "..........########..........",
                                                "######.##.########.##.######",
                                                "######.##.########.##.######",
                                                "######.##..........##.######",
                                                "######.##.########.##.######",
                                                "######.##.########.##.######",
                                                "#............##............#",
                                                "#.####.#####.##.#####.####.#",
                                                "#.####.#####.##.#####.####.#",
                                                "#...##................##...#",
                                                "###.##.##.########.##.##.###",
                                                "###.##.##.########.##.##.###",
                                                "#......##....##....##......#",
                                                "#.##########.##.##########.#",
                                                "#.##########.##.##########.#",
                                                "#..........................#",
                                                "############################"])
                         (Pacman (10,(-40)) W basespeed)
                         (Ghost (0,80) N basespeed)
                         (Ghost (80,0) O basespeed)
                         (Ghost (80,80) Z basespeed)
                         (Ghost (80,-40) W basespeed)

