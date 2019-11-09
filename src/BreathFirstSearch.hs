module BreathFirstSearch where 

    import Pacman
    import Data.Maybe
    import Collision
    
    data Rose a = MkRose a [Rose a]

    root :: Rose a -> a
    root (MkRose a rs) = a
    
    children :: Rose a -> [Rose a]
    children (MkRose a rs) = rs

    search :: Maze -> Ghost -> (Int, Int) -> Direction 
    search maze ghost@(Ghost (x,y) d s) posto@(px, py) -- = getdirection (ghostToPos ghost) (shortestpath( moves maze (ghostToPos ghost)) posto) 
         | length (moves maze (ghostToPos ghost)) < 3 = bestdirection (map (getdirection (ghostToPos ghost)) (moves maze (ghostToPos ghost))) d
         | otherwise =  getdirection (ghostToPos ghost) (shortestpath( moves maze (ghostToPos ghost)) posto) 
   
    getdirection :: (Int, Int) -> (Int,Int) -> Direction
    getdirection (gx,gy) direction@(sx, sy) 
        | gy > sy = N
        | gx < sx = O
        | gy < sy = Z
        | otherwise = W

    bestdirection :: [Direction] -> Direction -> Direction
    bestdirection (x:xs:xss) N  | x == Z = xs
                                | otherwise = x
    bestdirection (x:xs:xss) O  | x == W = xs
                                | otherwise = x
    bestdirection (x:xs:xss) Z  | x == N = xs
                                | otherwise = x
    bestdirection (x:xs:xss) W  | x == O = xs
                                | otherwise = x

    shortestpath :: [(Int, Int)] -> (Int, Int) -> (Int, Int)
    shortestpath posibilities to = minimum'( map (distance to) posibilities)

    distance :: (Int, Int) -> (Int, Int) -> (Int, (Int, Int))
    distance from@(x, y) to@(x', y') = ((x-x')^2 + (y-y')^2, to) 
    
    minimum' :: [(Int,(Int, Int))] -> (Int, Int)
    minimum' [] = (0,0)
    minimum' list@(x:xs) = minimum x xs 
            where   minimum (0,y) _ = y
                    minimum (_,y) [] = y
                    minimum x@(xs,xss) z@(y@(ys,yss):zs)
                            | xs <= ys = minimum x zs
                            | otherwise = minimum y zs  

    moves :: Maze -> (Int, Int) -> [(Int, Int)]
    moves maze pos@(x,y) = catMaybes [n,e,s,w] where
        n = checkmove maze (x,y+1)
        e = checkmove maze (x+1,y)
        s = checkmove maze (x,y-1)
        w = checkmove maze (x-1,y)
                                
    
    checkmove :: Maze -> (Int, Int) -> Maybe (Int, Int)
    checkmove maze pos@(x, y) 
        | x >= xfields || y >= yfields || x <= 0 || y <= 0 = Nothing
        | ((maze!!y)!!x) == M = Nothing
        | otherwise = Just pos

    pacmanToPos :: Pacman -> (Int, Int)
    pacmanToPos (Pacman (x, y) _ _ _) = divByFieldSize (x, y)

    ghostToPos :: Ghost -> (Int, Int)
    ghostToPos (Ghost (x, y) _ _) = divByFieldSize (x, y)

    divByFieldSize :: (Float, Float) -> (Int, Int)
    divByFieldSize (x, y) = (14 + div (x), 15 - div y) where
        div :: Float -> Int
        div number 
            | number >= 0 = ceiling (number / fromIntegral fieldsize :: Float)
            | otherwise = floor  (number / fromIntegral fieldsize :: Float)