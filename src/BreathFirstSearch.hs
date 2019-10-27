module BreathFirstSearch where 

    import Pacman
    import Data.Maybe

    data Rose a = MkRose a [Rose a]

    root :: Rose a -> a
    root (MkRose a rs) = a
    
    children :: Rose a -> [Rose a]
    children (MkRose a rs) = rs

    search :: Maze -> (Int, Int) -> (Int, Int) -> Direction 
    search maze posfrom@(gx, gy) posto@(px, py) = origin (gentree (px, py) maze (gx, gy)) where 
        origin :: Rose (Int, Int) -> Direction
        origin mazetree = getdirection (mini (zip (getlength mazetree) (map root (children mazetree))))
        getlength :: Rose (Int, Int) -> [Int]
        getlength tree = [0] -- map minimum' (map (shortestpath 0) (children tree))
        shortestpath :: Int -> Rose(Int, Int) -> [Int]
        shortestpath number (MkRose r []) = [number]
        shortestpath number tree' = (shortestpath (number + 1)) ((children tree')!!0)
        getdirection :: (Int, Int) -> Direction
        getdirection direction@(sx, sy)
            | gy < sy = N
            | gx < sx = O
            | gy > sy = Z
            | otherwise = W
        gentree :: (Int, Int) -> Maze -> (Int, Int) -> Rose (Int, Int)
        gentree posto' board posfrom'
            | posfrom' == posto' = MkRose posto' [gentree posto' board posfrom']
            | otherwise = MkRose posfrom' (map (gentree posto' board) (moves board posfrom'))   
    
    mini :: [(Int, (Int, Int))] -> (Int, Int)
    mini (x:[]) = snd x
    mini (x:(y:ys)) 
        | fst x <= fst y = mini (x:ys)
        | otherwise = mini (y:ys)
    
    minimum' :: [Int] -> Int
    minimum' list@(x:xs) = minimum x xs 
            where   minimum (-1) _ = -1
                    minimum min [] = min
                    minimum min xs@(y:ys) 
                            | min <= y = minimum min ys
                            | otherwise = minimum y ys    

    moves :: Maze -> (Int, Int) -> [(Int, Int)]
    moves maze pos@(x,y) = catMaybes [n,e,s,w] where
        n = checkmove maze (x,y+1)
        e = checkmove maze (x+1,y)
        s = checkmove maze (x,y-1)
        w = checkmove maze (x-1,y)
                                
    
    checkmove :: Maze -> (Int, Int) -> Maybe (Int, Int)
    checkmove maze pos@(x, y) 
        | ((maze!!y)!!x) == M = Nothing
        | otherwise = Just pos

    pacmanToPos :: Pacman -> (Int, Int)
    pacmanToPos (Pacman (x, y) _ _) = divByFieldSize (x, y)

    ghostToPos :: Ghost -> (Int, Int)
    ghostToPos (Ghost (x, y) _ _) = divByFieldSize (x, y)

    divByFieldSize :: (Float, Float) -> (Int, Int)
    divByFieldSize (x, y) = (div x, div y) where
        div :: Float -> Int
        div number = floor (number / fromIntegral fieldsize :: Float)