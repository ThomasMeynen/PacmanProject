module Pacman where

data GameState = GameState {
    maze :: Maze
    }
    
data Player = PacMan|Ghost
data PacMan = Pacman
data Field  = W|D|O|G|P
data Ghost  = Blinky|Pinky|Inky|Clyde

type Row  = [Field]
type Maze = [Row]

initialState :: GameState
initialState = GameState [[W,W,W],[O,O,O],[O,O,O]]