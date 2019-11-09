module Collision where

import Pacman
import Control.Lens (element, (.~), (&))

gridToPos :: Int -> Int -> Float -- Takes a position in the grid [0 .. xfields/yfields] and an amount of fields and transforms to the correct screen position.
gridToPos i f | mod i 2 == 0 = (fromIntegral fieldsize / 2) + (fromIntegral i - (fromIntegral f / 2)) * fromIntegral fieldsize
              | otherwise    = (fromIntegral i - (fromIntegral (f-1) / 2)) * fromIntegral fieldsize

posToGrid :: Float -> Int -> Maybe Int --Takes a coordinate and an amount of fields and returns Just field if that coordinate is the center of a field or nothing otherwise.
posToGrid pos f | mod (round pos) fieldsize == 0  && mod f 2 == 1 = Just (floor (fromIntegral f - (fromIntegral f+1)/2 + (pos / (fromIntegral (fieldsize)))))
                | mod (round pos) fieldsize == 10 && mod f 2 == 0 = Just (floor (fromIntegral f - (fromIntegral f)/2 + ((pos - (half fieldsize)) / (fromIntegral (fieldsize)))))
                | otherwise                                       = Nothing where
                    half i = fromIntegral i / 2

onEdge :: Maybe Int -> Maybe Int -> Maybe (Int, Int) --support function to not get index out of range when searching the maze
onEdge Nothing _         = Nothing
onEdge _ Nothing         = Nothing
onEdge (Just x) (Just y) | x == 0 || x == xfields - 1 || y == 0 || y == yfields -1 = Nothing
                         | otherwise                                               = Just (x,y)

outOfBounds :: Float -> Float
outOfBounds x | x > halfx      = halfx*(-1) + 2
              | x < halfx*(-1) = halfx - 2 
              | otherwise      = x where
  halfx = (fromIntegral xsize / 2)

wallCollision :: Maze -> Direction -> Maybe (Int, Int) -> Maybe Bool --returns true if facing a wall and false otherwise and nothing if the target isn't on a grid position
wallCollision _ _ Nothing       = Nothing
wallCollision m d (Just (x,y))  = case d of 
    N -> Just ((m!!(y-1))!!x == M)
    O -> Just ((m!!y)!!(x+1) == M)
    Z -> Just ((m!!(y+1))!!x == M)
    W -> Just ((m!!y)!!(x-1) == M)

dotCollision :: Maze -> Maybe Int -> Maybe Int -> Maze
dotCollision m Nothing _ = m
dotCollision m _ Nothing = m
dotCollision m (Just(x)) (Just(y)) = case (m!!y)!!x of
    D -> m & (element y) . (element x) .~ L
    otherwise -> m

ghostCollision :: Pacman -> Ghost -> Bool
ghostCollision (Pacman (px,py) _ _) (Ghost (gx,gy) _ _) | abs (px - gx) < 10 || abs (py - gy) < 10 = True
                                                        | otherwise                                = False

checkDirection :: Maze -> Direction -> Float -> Float -> Maybe Bool
checkDirection m d x y = wallCollision m d (onEdge (posToGrid x xfields) (posToGrid (y*(-1)) yfields))

checkDot :: Maze -> Float -> Float -> Maze
checkDot m x y = dotCollision m (posToGrid x xfields) (posToGrid (y*(-1)) yfields)
