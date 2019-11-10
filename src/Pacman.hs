module Pacman where

import Graphics.Gloss
import System.IO
import Control.Monad
import System.Random

data GameState = GameState {
    animation :: Float, --used to guide our animation
    lives :: Int, --the amount of lives you have left
    score :: Int, --the current score
    highscore :: String, --the highscore, saved in data
    paused :: Paused, --The state the game is in.
    buffer :: Buffer, --input buffer for the movement of pacman
    maze :: Maze, --the current situation of the board
    pacman :: Pacman, --Our pacman (includes location direction and its speed)
    blinky :: Ghost, --Our first ghost (includes location direction and its speed)
    pinky :: Ghost, --second ghost
    inky :: Ghost, -- third ghost
    clyde :: Ghost, --last ghost
    generator :: StdGen --our RNG seed used to draw random numbers.
    }

--Some Types we need
type Row   = [Field]
type Maze  = [Row]
type Speed = Float
data Paused = Playing|Paused|GameOver|Dood 
  deriving (Eq)
data Pacman = Pacman (Float, Float) Direction Speed
data Ghost  = Ghost (Float, Float) Direction Speed
data Field  = M|D|L --Muren, Dots, Leeg
  deriving (Eq)
data Direction = N|O|Z|W --Noord, Oost, Zuid, West
  deriving (Eq)
data Buffer    = Buffer Direction

--The class of movable objects in our game. Used to move and draw the to the screen.
class Movables a where
  move :: a -> a
  draw :: Float -> a -> Color -> Picture

instance Movables Pacman where
  move (Pacman (x,y) d s) = case d of
    N -> Pacman (x,y+s) d s
    O -> Pacman (x+s,y) d s
    Z -> Pacman (x,y-s) d s
    W -> Pacman (x-s,y) d s
  draw i (Pacman (x,y) _ _) c =  translate x y (color c (circleSolid ((fromIntegral fieldsize) * i / 100 - 1 )))

instance Movables Ghost where
  move (Ghost (x,y) d s) = case d of
    N -> Ghost (x,y+s) d s
    O -> Ghost (x+s,y) d s
    Z -> Ghost (x,y-s) d s
    W -> Ghost (x-s,y) d s
  draw _ (Ghost (x,y) _ _) c = translate x y (color c (circleSolid ((fromIntegral fieldsize) / 2 - 1 )))

--Some initial variables used to determine the size of the screen. Useful in case you are making different levels etc.

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


--function to get the initial state of our game. It starts paused so you have time to plan ahead.
--The main function produces the highscore from file and a generator from IO.
initialState :: String -> Int -> StdGen -> GameState
initialState highscore score generator = GameState 50
                         3
                         score
                         highscore
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
                         (Ghost (10,80) W basespeed)
                         (Ghost (90,0) O basespeed)
                         (Ghost (90,80) W basespeed)
                         (Ghost (90,-40) W basespeed)
                         generator

