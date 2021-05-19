{-# OPTIONS_GHC -Wall #-}
module Week1.HW01 where
import Data.Function

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10
-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit n = n `div` 10

-- Exercise 2 -----------------------------------------

{- toRevDigits should convert positive Integers to a list of digits. (For
0 or negative inputs, toRevDigits should return the empty list.) -}

-- non tail recursive
toRevDigits :: Integer -> [Integer]
toRevDigits n 
        | n <= 0 = [] 
        | otherwise = lastDigit n : toRevDigits (dropLastDigit n)

-- tail recursive version, assuming reverse is tail recursive
toRevDigits' :: Integer -> [Integer]
toRevDigits' n = helper n []
    where
        helper :: Integer -> [Integer] -> [Integer]
        helper x acc
            | x <= 0 = reverse acc
            | otherwise = helper (dropLastDigit x) (lastDigit x : acc)

-- tail recursive non reversed digits
toDigits :: Integer -> [Integer]
toDigits n = helper n []
    where
        helper :: Integer -> [Integer] -> [Integer]
        helper x acc
            | x <= 0 = acc
            | otherwise = helper (dropLastDigit x) (lastDigit x : acc)
    

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
-- Original solution, note to self, avoid using head and tail in the future, can cause runtime exceptions
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther lst = helper 0 lst [] 
    where 
        helper :: Integer -> [Integer] -> [Integer] -> [Integer]
        helper ctr trav acc
            | null trav = reverse acc
            | odd ctr = helper (ctr + 1) (tail trav) (head trav * 2 : acc)
            | otherwise = helper (ctr + 1) (tail trav) (head trav : acc)


doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther' = zipWith helper [1..] 
    where
        helper :: Integer -> Integer -> Integer
        helper x y 
            | even x = y*2
            | otherwise = y

-- Version using pattern matching within the helper
doubleEveryOther'' :: [Integer] -> [Integer]
doubleEveryOther'' lst = helper 0 lst [] 
    where 
        helper :: Integer -> [Integer] -> [Integer] -> [Integer]
        helper _ [] acc = reverse acc
        helper ctr (x:xs) acc 
            | odd ctr = helper (ctr + 1) (xs) (x * 2 : acc)
            | otherwise = helper (ctr + 1) (xs) (x:acc)

-- Version using zipWith and cycle

doubleEveryOther''' :: [Integer] -> [Integer]
doubleEveryOther''' = zipWith helper (cycle [1,2])
    where 
        helper :: Integer -> Integer -> Integer
        helper x y 
            | even x = y*2
            | otherwise = y

        

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
-- Performs in one pass
sumDigits = foldr (helper) 0
    where
        helper :: Integer -> Integer -> Integer
        helper n = 
            toDigits n & sum & (+)

sumDigits' :: [Integer] -> Integer 
sumDigits' = sum . map (sum . toDigits)
{- sumDigits' = sum . map sum . toDigits 
 -}

 -- make explicit argument version with lst arguments as xs

sumDigits'' :: [Integer] -> Integer
sumDigits'' xs = sum $ map (sum . toDigits) xs

-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.

luhn :: Integer -> Bool
luhn n = toRevDigits n & doubleEveryOther & sumDigits & (`mod` 10)  & (==0)

luhn' :: Integer -> Bool
luhn' = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toRevDigits

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n source target spare
    | n == 1 = [(source,target)]
    | otherwise = 
        hanoi (n-1) source spare target ++ [(source,target)] ++ hanoi (n-1) spare target source 

        

