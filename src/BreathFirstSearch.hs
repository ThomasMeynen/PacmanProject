module BreathFirstSearch where 

    import Pacman

    search :: Maze -> (Int, Int) -> (Int, Int) -> Direction 
    search maze posfrom posto = N

    pacmanToPos :: Pacman -> (Int, Int)
    pacmanToPos (Pacman (x, y) _ _) = divByFieldSize (x, y)

    ghostToPos :: Ghost -> (Int, Int)
    ghostToPos (Ghost (x, y) _ _) = divByFieldSize (x, y)

    divByFieldSize :: (Float, Float) -> (Int, Int)
    divByFieldSize (x, y) = (div x, div y) where
        div :: Float -> Int
        div number = floor (number / fromIntegral fieldsize :: Float)