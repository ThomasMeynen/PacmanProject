module GhostAI where 

import Pacman
import Collision

import Data.Maybe
import System.Random

search :: Maze -> Ghost -> (Int, Int) -> Direction --Returns best direction for a ghost to a point in the maze
search maze ghost@(Ghost (x,y) d s) posto@(px, py) 
        | notcenter(x,y) = d            -- move to the center of a field
        | length (moves maze (ghostToPos ghost)) < 3 = bestdirection (map (getdirection (ghostToPos ghost)) (moves maze (ghostToPos ghost))) d --move to next crosssection
        | otherwise =  getdirection (ghostToPos ghost) (shortestpath( moves maze (ghostToPos ghost)) posto) --getting the right direction on a crosssection

notcenter::(Float,Float) -> Bool --checks if location is the middle of a field and returns false if it is. True if it isn't
notcenter (x,y) = not (div' (x+10) (fromIntegral fieldsize :: Float) && div' y (fromIntegral fieldsize :: Float))

div' :: Float -> Float-> Bool --reimplementation of mod
div' pos x 
    | pos >= (x) = div' (pos - x) x
    | pos < 0 = div' (-pos) x
    | pos == 0 = True
    | otherwise = False

getdirection :: (Int, Int) -> (Int,Int) -> Direction --calculate direction from 2 adjacent fields
getdirection (gx,gy) direction@(sx, sy) 
    | gy > sy = N
    | gx < sx = O
    | gy < sy = Z
    | otherwise = W

goTo :: Direction -> (Float,Float) --calculates translation from direction
goTo N = (0,-1)
goTo O = (1,0)
goTo Z = (0,1)
goTo W = (-1,0)

bestdirection :: [Direction] -> Direction -> Direction --returns the direction that isn't going backwards
bestdirection [x] _ = x
bestdirection (x:xs:xss) N  | x == Z = xs
                            | otherwise = x
bestdirection (x:xs:xss) O  | x == W = xs
                            | otherwise = x
bestdirection (x:xs:xss) Z  | x == N = xs
                            | otherwise = x
bestdirection (x:xs:xss) W  | x == O = xs
                            | otherwise = x

shortestpath :: [(Int, Int)] -> (Int, Int) -> (Int, Int) --returns the closest field to a given field
shortestpath posibilities to = minimum'( map (distance to) posibilities)

distance :: (Int, Int) -> (Int, Int) -> (Int, (Int, Int)) --squared distance between 2 points and the point moving from
distance to@(x, y) from@(x', y') = ((x-x')^2 + (y-y')^2, from) 

minimum' :: [(Int,(Int, Int))] -> (Int, Int) --returns the points with least squared distance
minimum' [] = (0,0)
minimum' list@(x:xs) = minimum x xs 
        where   minimum (0,y) _ = y
                minimum (_,y) [] = y
                minimum x@(xs,xss) z@(y@(ys,yss):zs)
                        | xs <= ys = minimum x zs
                        | otherwise = minimum y zs  

moves :: Maze -> (Int, Int) -> [(Int, Int)] --returns a list of all possible moves
moves maze pos@(x,y) = catMaybes [n,e,s,w] where
    n = checkmove maze (x,y+1)
    e = checkmove maze (x+1,y)
    s = checkmove maze (x,y-1)
    w = checkmove maze (x-1,y)
                            

checkmove :: Maze -> (Int, Int) -> Maybe (Int, Int) --checks if a move is within bounds and not into a wall
checkmove maze pos@(x, y) 
    | x >= xfields || y >= yfields || x <= 0 || y <= 0 = Nothing
    | ((maze!!y)!!x) == M = Nothing
    | otherwise = Just pos

pacmanToPos :: Pacman -> (Float, Float) -> (Int, Int) --gets pacmans grid position from its location and a translation
pacmanToPos (Pacman (x, y) _ _ ) (xs,ys) = divByFieldSize (x+xs, y+ys)

ghostToPos :: Ghost -> (Int, Int) --gets ghosts grid position from its location
ghostToPos (Ghost (x, y) _ _) = divByFieldSize (x, y)

divByFieldSize :: (Float, Float) -> (Int, Int) --gets a grid position from a location
divByFieldSize (x, y) = (14 + divx (x), 15 - divy y) where
    divx :: Float -> Int
    divx number 
        | number >= 0 = floor (number / fromIntegral fieldsize :: Float)
        | otherwise = floor  (number / fromIntegral fieldsize :: Float)
    divy :: Float -> Int
    divy number 
        | number >= 0 = ceiling (number / fromIntegral fieldsize :: Float)
        | otherwise = floor  (number / fromIntegral fieldsize :: Float)
    
    
getRandomNumber :: StdGen -> Int -> (Int, StdGen) --getting a random number from our rng
getRandomNumber g range = randomR (0,range) g

randomDirection :: Maze -> Int -> Ghost -> Direction --getting a new direction for our ghost based on our maze and generated number.
randomDirection maze i ghost@(Ghost (x,y) d s)  
    | notcenter(x,y)                 = d
    | elem d possiblemoves && i < 30 = d
    | otherwise                      = possiblemoves!!(mod i (length possiblemoves)) where
        possiblemoves = (map (getdirection (ghostToPos ghost)) (moves maze (ghostToPos ghost)))