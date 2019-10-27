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
                         (createMaze (reverse [ "############################",
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
                                                "############################"]))
                         (Pacman (0,-40) W 3)
                         (Ghost (0,80) N 2)
                         (Ghost (80,0) O 2)
                         (Ghost (80,80) Z 2)
                         (Ghost (80,-40) W 2)

createMaze :: [[Char]] -> Maze
createMaze string = map createRows string where
    createRows :: [Char] -> Row
    createRows [] = []
    createRows (x:xs)  
        | x == '#' = M : (createRows xs)
        | x == '.' = D : (createRows xs)
        | otherwise = L : (createRows xs)