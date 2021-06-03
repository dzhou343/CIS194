{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Week5.HW05 where

import Data.List
import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

import Data.Bits ( Bits(xor) )

import Week5.Parser

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret path1 path2 =
  do
    f1 <- BS.readFile path1
    f2 <- BS.readFile path2
    return $ BS.filter (/=0) $ BS.pack $ BS.zipWith xor f1 f2



-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key output =
  do
    input <- BS.readFile $ output ++ ".enc"
    BS.writeFile output $ BS.pack $ BS.zipWith xor input $ BS.cycle key


-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile fp = do
  f <- BS.readFile fp
  return $ decode f

-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs victimPath transactionPath =
  do
    vf <- parseFile victimPath
    tf <- parseFile transactionPath
    let res = case (vf, tf) of
          (Nothing , _) -> Nothing
          (_, Nothing ) -> Nothing
          (Just v, Just t) -> Just $ helper v t
    return res
      where
        helper :: [TId] -> [Transaction] -> [Transaction]
        helper victim transactions = filter ((`elem` victim) . tid) transactions


getBadTs' :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs' victimPath transactionPath = 
  do 
    vf <- parseFile victimPath
    tf <- parseFile transactionPath
    return $ helper <$> vf <*> tf
      where 
        helper :: [TId] -> [Transaction] -> [Transaction]
        helper victim transactions = filter ((`elem` victim) . tid) transactions 

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow ts = helper ts Map.empty
  where
    helper :: [Transaction] -> Map String Integer -> Map String Integer
    helper [] mp = mp
    helper (x:xs) mp =
      let amt = amount x in
        helper xs $ Map.insertWith (+) (from x) amt $ Map.insertWith (+) (to x) (negate amt) mp

getFlow' :: [Transaction] -> Map String Integer
getFlow' = foldr helper Map.empty
  where
    helper x mp =
      let amt = amount x in
        Map.insertWith (+) (from x) amt $ Map.insertWith (+) (to x) (negate amt) mp


-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal mp = helper (Map.toList mp) 0 "all is good"
  where
    helper :: [(String, Integer)] -> Integer -> String -> String
    helper [] _ crim = crim
    helper ((k,v):xs) cntr crim =
      if v > cntr then
        helper xs v k
      else
        helper xs cntr crim



-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs mp idx =
  let (payer,payee) = partition ((> 0) . snd ) $ reverse $ sort $  Map.toList mp in
  helper payer payee idx
    where
      helper [] _ _ = []
      helper _ [] _ = []
      helper _ _ [] = []
      helper (pa:payers) (py:payees) (i:ids)
        | snd pa == 0 = helper payers (py:payees) (i:ids)
        | snd py == 0 = helper (pa:payers) payees (i:ids)
        | otherwise =
            let amt = min (abs $ snd $ pa) (abs $ snd $ py) in
              Transaction (fst pa) (fst py) amt i :
              helper ((fst pa, snd pa - amt) : payers) ((fst py, snd py - amt) : payees) ids



-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON fp = BS.writeFile fp . encode

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <-
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim
