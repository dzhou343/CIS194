{-# OPTIONS_GHC -Wall #-}
module Week2.HW02 where

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
-- ugly single pass function
countColors :: Code -> [Int]
countColors = foldl helper [0,0,0,0,0,0]
    where
        helper :: [Int] -> Peg -> [Int]
        helper [r,g,b,y,o,p] peg = 
            case peg of 
                Red -> [r+1,g,b,y,o,p]
                Green -> [r,g+1,b,y,o,p]
                Blue -> [r,g,b+1,y,o,p]
                Yellow ->[r,g,b,y+1,o,p]
                Orange -> [r,g,b,y,o+1,p]
                Purple -> [r,g,b,y,o,p+1]

-- elegant, slightly less efficient
countColors' :: Code -> [Int]
countColors' x = map helper colors
    where
        helper c = length $ filter (==c) x         

-- Count number of matches between the actual code and the guess
{- matches :: Code -> Code -> Int
matches c1 c2 = sum finLst
    where 
        zippedLst :: [(Int,Int)]
        zippedLst = zip (countColors c1) (countColors c2)
        finLst :: [Int]
        finLst = map bothPresent zippedLst
        bothPresent :: (Int, Int) -> Int
        bothPresent (x,y)
            | x > 0 && y > 0 = 1
            | otherwise = 0

matches' :: Code -> Code -> Int 
matches' c1 c2 = sum finLst
    where 
        bothPresent :: Int -> Int -> Int
        bothPresent x y 
            | x > 0 && y > 0 = 1
            | otherwise = 0
        finLst :: [Int]
        finLst = zipWith bothPresent (countColors c1) (countColors c2) -}

-- Correct matches 
matches'' :: Code -> Code -> Int
matches'' c1 c2 = sum (zipWith min (countColors c1) (countColors c2))
        
        

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove x y = Move y exact actual
    where
        exact, actual :: Int 
        exact = exactMatches x y 
        actual = matches'' x y - exact

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent mv@(Move x _ _ ) y = mv == getMove x y 

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes x = filter (isConsistent x)

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 0 = [[]]
allCodes i = [x:xs | x <- colors, xs <- allCodes (i-1)]

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve code = helper $ allCodes $ length code
    where
        helper :: [Code] -> [Move]
        helper [] = []
        helper c@(guess : _) = m : helper (filterCodes m c)
            where 
                m = getMove code guess

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined