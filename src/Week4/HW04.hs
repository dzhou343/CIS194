{-# OPTIONS_GHC -Wall #-}
module Week4.HW04 where

import Data.List

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

-- reverse input lists, drop all leading 0s. check for equality.


instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P a) (P b) = helper a ==  helper b
        where 
            helper :: (Eq a, Num a) => [a] -> [a]
            helper = dropWhile (==0) . reverse 
 
-- Exercise 3 -----------------------------------------

-- P [1,2,3] == [3x^2 + 2x + 1]

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P []) = ""
    show (P b) = plusLst
        where 
            helper :: (Num a, Eq a, Show a) => Integer -> a -> String
            helper _ n  | n == 0 = ""
            helper 0 coeff = show coeff
            helper 1 1 = "x"
            helper 1 (-1) = "-x"
            helper 1 coeff = show coeff ++ "x"
            helper degree 1 = "x^" ++ show degree
            helper degree coeff = show coeff ++ "x^" ++ show degree
            plusLst = intercalate " + " $ filter (/=[]) $ reverse $ zipWith helper [0..] b
 

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P a) (P b) = P (helper a b)
    where
        helper [] ys = ys
        helper xs [] = xs
        helper (x:xs) (y:ys) = x + y : helper xs ys 


-- Exercise 5 -----------------------------------------
-- function to multiply an input with a whole polynomial
-- input should be a coefficient and degree

-- finLst should be [[poly][poly][poly]]

-- helper coeff degree inputPoly -> outputPoly
-- Need to extend outputPoly by degree


times :: Num a => Poly a -> Poly a -> Poly a
times (P a) (P b) = sum finLst
    where 
        helper coeff degree inputPoly = P (replicate degree 0 ++ map (coeff*) inputPoly)
        -- now need to do this for each element in a 
        finLst = zipWith (\x y -> helper y x b) [0..] a


-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P a) = P $ map ((-1)*) a 
    fromInteger n = P [fromInteger n]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P a) n = sum $ zipWith (\degree coeff -> n^degree * coeff ) [0..] a

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv 0 x = x
    nderiv n x = nderiv (n-1) (deriv x)

-- Exercise 9 -----------------------------------------

-- deriv 0 1 1 = 1 2
-- deriv 0 0 2 1 = 0 4 3 
-- deriv 1 0 2 1 = 1 + 2x^2 + x^3 -> 4x + 3x^2 

instance Num a => Differentiable (Poly a) where
    deriv (P a) = P $ drop 1 $ zipWith (\index coeff -> fromInteger index * coeff) [0..] a 

