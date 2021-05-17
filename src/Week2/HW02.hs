{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches [] _ = 0
exactMatches _ [] = 0
exactMatches (x:xs) (y:ys) 
    | x == y = 1 + exactMatches xs ys 
    | otherwise = exactMatches xs ys 

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors = foldl (helper) [0,0,0,0,0,0]
    where 
        helper :: [Int] -> Peg -> [Int]
        helper [r,g,b,y,o,p] x
            | x == Red =[r+1,g,b,y,o,p]
            | x == Green = [r,g+1,b,y,o,p]
            | x == Blue = [r,g,b+1,y,o,p]
            | x == Yellow = [r,g,b,y+1,o,p]
            | x == Orange = [r,g,b,y,o+1,p]
            | x == Purple = [r,g,b,y,o,p+1]
            | otherwise = [r,g,b,y,o,p]
            

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches c1 c2 = foldl (+) 0 finLst
    where 
        zippedLst :: [(Int,Int)]
        zippedLst = zip (countColors c1) (countColors c2)
        finLst :: [Int]
        finLst = map bothPresent zippedLst
        bothPresent :: (Int, Int) -> Int
        bothPresent (x,y)
            | x > 0 && y > 0 = 1
            | otherwise = 0
        

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove = undefined

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent = undefined

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes = undefined

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes = undefined

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve = undefined

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined