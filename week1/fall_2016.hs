{-# LANGUAGE OverloadedStrings #-}

import CodeWorld

main :: IO ()
main = exercise2

botCircle, topCircle, midCircle :: Color -> Picture
botCircle c = colored c (translated 0 (-2.5) (solidCircle 1))
midCircle c = colored c (translated 0 0 (solidCircle 1))
topCircle c = colored c (translated 0   2.5  (solidCircle 1))

frame :: Picture
frame = rectangle 2.5 7.5

trafficLight :: Int -> Picture
trafficLight 1  = botCircle green & midCircle black & topCircle black & frame
trafficLight 2  = botCircle black & midCircle yellow & topCircle black & frame
trafficLight 3 = botCircle black & midCircle black & topCircle red   & frame
trafficLight 4 = botCircle black & midCircle yellow & topCircle red   & frame

trafficController :: Double -> Picture
trafficController t
  | round (t) `mod` 10 <= 3 = trafficLight 1
  | round (t) `mod` 10 <= 5 = trafficLight 2
  | round (t) `mod` 10 <= 8 = trafficLight 3
  | otherwise                = trafficLight 4
                                                  
exercise1 :: IO ()
exercise1 = animationOf trafficController

-- Exercise 2
tree :: Integer -> Double -> Picture
tree (-1) bs = blank
tree 0 bs = colored yellow (solidCircle bs)
tree n bs = path [(0,0),(0,1)] & translated 0 1 (
  rotated (pi/10) (tree (n-1) bs) & rotated (- pi/10) (tree (n-1) bs))

treeController t = tree 8 ((t `mod` 10) *0.03)
exercise2 :: IO ()
-- exercise2 = animationOf (tree 8 . sin)
exercise2 = animationOf treeController


-- Exercise 3

wall, ground, storage, box :: Picture
wall =    undefined
ground =  undefined
storage = undefined
box =     undefined

drawTile :: Integer -> Picture
drawTile = undefined

         
pictureOfMaze :: Picture
pictureOfMaze = undefined

exercise3 :: IO ()
exercise3 = undefined
         
maze :: Integer -> Integer -> Integer 
maze x y
  | abs x > 4  || abs y > 4  = 0
  | abs x == 4 || abs y == 4 = 1
  | x ==  2 && y <= 0        = 1
  | x ==  3 && y <= 0        = 3
  | x >= -2 && y == 0        = 4
  | otherwise                = 2
 
