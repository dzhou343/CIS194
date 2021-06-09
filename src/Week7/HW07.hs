{-# LANGUAGE MonadComprehensions, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module Week7.HW07 where

import Prelude hiding (mapM)
import Week7.Cards

import Control.Monad hiding (mapM, liftM)
import Control.Monad.Random
import Data.Functor
import Data.Monoid
import Data.Vector (Vector, cons, (!), (!?), (//))
import System.Random

import qualified Data.Vector as V


-- Exercise 1 -----------------------------------------

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f m =
    do
        f <$> m

liftM' :: Monad m => (a -> b) -> m a -> m b
liftM' f m = f <$> m

swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV i j vec =
    liftM2 (\v1 v2 -> vec // [(i,v2), (j,v1)]) (vec !? i) (vec !? j)

-- Exercise 2 -----------------------------------------

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f xs = sequence $ f <$> xs

getElts :: [Int] -> Vector a -> Maybe [a]
getElts xs vec = Week7.HW07.mapM (vec !?) xs

-- Exercise 3 -----------------------------------------

type Rnd a = Rand StdGen a

randomElt :: Vector a -> Rnd (Maybe a)
randomElt vec =
    do
        ind <- getRandomR (0, V.length vec -1)
        return $ vec !? ind


-- Exercise 4 -----------------------------------------

randomVec :: Random a => Int -> Rnd (Vector a)
randomVec n = V.fromList <$> replicateM n getRandom

randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR n (lo,hi) = V.fromList <$> replicateM n (getRandomR (lo,hi))

-- Exercise 5 -----------------------------------------

swap :: (Int, Int) -> Vector a -> Vector a
swap (i,j) vec = 
    let jv = vec ! j in 
    let iv = vec ! i in 
    vec // [(i, jv), (j, iv)]

shuffle :: Vector a -> Rnd (Vector a)
shuffle vec = do
    swaps <- Week7.HW07.mapM swapPartner [n-1,n-2 .. 1]
    return $ foldr swap vec swaps
        where
            n = V.length vec
            swapPartner i = (\j -> (i,j)) <$> getRandomR (0,n)


-- Exercise 6 -----------------------------------------

partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt vec i = (less, pivotVal, more)
    where
        pivotVal = vec ! i
        (less,more) = V.partition (< pivotVal) vec


-- Exercise 7 -----------------------------------------

-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ y | y <- xs, y < x ]
                   <> (x : quicksort [ y | y <- xs, y >= x ])

qsort :: Ord a => Vector a -> Vector a
qsort = V.fromList . quicksort . V.toList


-- Exercise 8 -----------------------------------------

qsortR :: Ord a => Vector a -> Rnd (Vector a)
qsortR vec
    | V.null vec = return V.empty
    | otherwise = details >>= helperSort
        where
            details = getRandomR (0, V.length vec -1) >>= return . partitionAt vec
            helperSort (l,m,h) = liftM2 (<>) (qsortR l) (Control.Monad.Random.liftM (cons m) (qsortR h))




-- Exercise 9 -----------------------------------------

-- Selection
select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select idx vec 
    | V.null vec = return Nothing 
    | otherwise = chosen >>= helper
        where 
            chosen = getRandomR (0, V.length vec -1) >>= return . partitionAt vec
            helper (l,m,h)
                | idx < lLen = select idx l
                | idx > lLen = select (idx - lLen - 1) h
                | otherwise = return $ Just m 
                where 
                    lLen = V.length l

-- Exercise 10 ----------------------------------------

allCards :: Deck
allCards = [Card label suit | suit <- suits, label <- labels ]

newDeck :: Rnd Deck
newDeck =  shuffle allCards

-- Exercise 11 ----------------------------------------

nextCard :: Deck -> Maybe (Card, Deck)
nextCard d 
    | V.null d = Nothing 
    | otherwise = Just (V.head d, V.tail d)

-- Exercise 12 ----------------------------------------

getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards n d = helper n d []
    where 
        helper 0 d acc = return (acc,d)
        helper n d acc = 
                do
                    (c,de) <- nextCard d
                    helper (n-1) de (c : acc)

-- Exercise 13 ----------------------------------------

data State = State { money :: Int, deck :: Deck }

repl :: State -> IO ()
repl s@State{..} | money <= 0  = putStrLn "You ran out of money!"
                 | V.null deck = deckEmpty
                 | otherwise   = do
  putStrLn $ "You have \ESC[32m$" ++ show money ++ "\ESC[0m"
  putStrLn "Would you like to play (y/n)?"
  cont <- getLine
  if cont == "n"
  then putStrLn $ "You left the casino with \ESC[32m$"
           ++ show money ++ "\ESC[0m"
  else play
    where deckEmpty = putStrLn $ "The deck is empty. You got \ESC[32m$"
                      ++ show money ++ "\ESC[0m"
          play = do
            putStrLn "How much do you want to bet?"
            amt <- read <$> getLine
            if amt < 1 || amt > money
            then play
            else do
              case getCards 2 deck of
                Just ([c1, c2], d) -> do
                  putStrLn $ "You got:\n" ++ show c1
                  putStrLn $ "I got:\n" ++ show c2
                  case () of
                    _ | c1 >  c2  -> repl $ State (money + amt) d
                      | c1 <  c2  -> repl $ State (money - amt) d
                      | otherwise -> war s{deck = d} amt
                _ -> deckEmpty
          war (State m d) amt = do
            putStrLn "War!"
            case getCards 6 d of
              Just ([c11, c21, c12, c22, c13, c23], d') -> do
                putStrLn $ "You got\n" ++ ([c11, c12, c13] >>= show)
                putStrLn $ "I got\n" ++ ([c21, c22, c23] >>= show)
                case () of
                  _ | c13 > c23 -> repl $ State (m + amt) d'
                    | c13 < c23 -> repl $ State (m - amt) d'
                    | otherwise -> war (State m d') amt
              _ -> deckEmpty

main :: IO ()
main = evalRandIO newDeck >>= repl . State 100