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

toRevDigits :: Integer -> [Integer]
toRevDigits n = helper n []
    where
        helper x acc 
            | x <= 0 = reverse acc 
            | otherwise = helper (dropLastDigit x) (lastDigit x : acc)
    

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther lst = helper 0 lst [] 
    where 
        helper ctr trav acc
            | trav == [] = reverse acc
            | ctr `mod` 2 == 1 = helper (ctr + 1) (tail trav) (head trav * 2 : acc)
            | otherwise = helper (ctr + 1) (tail trav) (head trav : acc)

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits lst = sum lst 


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.

luhn :: Integer -> Bool
luhn n = toRevDigits n & doubleEveryOther & sumDigits & (`mod` 10)  & (==10)

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n source target spare
    | n == 1 = [(source,target)]
    | otherwise = 
        hanoi (n-1) source spare target ++ [(source,target)] ++ hanoi (n-1) spare target source 

        

