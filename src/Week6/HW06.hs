{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall #-}
module Week6.HW06 where

import Data.List
import Data.Functor

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = fib <$> [0..]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)


-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons x y) = x : streamToList y

-- Exercise 4 -----------------------------------------

instance Functor Stream where
    fmap f (Cons x y) = Cons (f x) (fmap f y)

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat x = Cons x (sRepeat x)

sIterate :: (a -> a) -> a -> Stream a
sIterate f x = Cons x (sIterate f (f x))

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons x st1) st2 = Cons x (sInterleave st2 st1)

sTake :: Int -> Stream a -> [a]
sTake n st = take n $ streamToList st

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = sIterate (+1) 0

ruler :: Stream Integer
ruler = sRepeat 0 `sInterleave` (sRepeat 1 `sInterleave` (sRepeat 2 `sInterleave` (sRepeat 3 `sInterleave` sRepeat 4)))

ruler' :: Stream Integer
ruler' = helper 0
    where helper n = sRepeat n `sInterleave` helper (n + 1)

-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand = sIterate helper
    where
        helper n = (1103515245 * n + 12345) `mod` 2147483648

-- Exercise 8 -----------------------------------------

{- Total Memory in use: 201 MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: 2 MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax [] = Nothing
minMax (x:xs) = Just (helper xs x x)
    where
        helper [] mini maxi = (mini,maxi)
        helper (x:xs) !mini !maxi = helper xs (x `min` mini) (x `max` maxi)


main :: IO ()
main = print $ minMax $ sTake 1000000 $ rand 7666532

-- Exercise 10 (Optional) ----------------------------------------

fastFib :: Int -> Integer
fastFib = undefined