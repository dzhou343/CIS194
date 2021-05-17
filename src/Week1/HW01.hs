{-# OPTIONS_GHC -Wall #-}
module HW01 where
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
-- If only there was a mapi in haskell

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther lst = helper 0 lst [] 
    where 
        helper :: Integer -> [Integer] -> [Integer] -> [Integer]
        helper ctr trav acc
            | null trav = reverse acc
            | ctr `mod` 2 == 1 = helper (ctr + 1) (tail trav) (head trav * 2 : acc)
            | otherwise = helper (ctr + 1) (tail trav) (head trav : acc)


doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther' = zipWith helper [1..] 
    where
        helper :: Integer -> Integer -> Integer
        helper x y 
            | x `mod` 2 == 1 = y*2
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

        

