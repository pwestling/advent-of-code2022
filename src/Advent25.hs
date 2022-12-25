module Advent25 where

import System.CPUTime (getCPUTime)
import Control.Monad.IO.Class (liftIO)
import Util
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Char
import Debug.Trace
import Data.Char
import Data.Maybe
import Safe
import Control.Monad (foldM, join)
import Data.List
import Graph
import Search
import Data.Hashable

import qualified Data.Map as Map


fromSNAFU :: String -> Int
fromSNAFU s = result where
  toDig :: Char -> Int
  toDig '-' = -1
  toDig '=' = -2
  toDig c = digitToInt c
  indexed = zip [0..] (fmap toDig (reverse s))
  pow5 (index, val) = 5 ^ index * val
  result = sum (fmap pow5 indexed)

toSNAFU :: Int -> String
toSNAFU i = reverse (toSNAFU' i)

toSNAFU' :: Int -> String
toSNAFU' n = result where
  nextVals s = fmap (\c -> c:s) ['=', '-', '0', '1', '2']
  isNum s = (fromSNAFU (reverse s)) == n
  result = head $ fromJust $ bfs nextVals isNum ""


pushI :: Int -> Int -> [Int] -> [Int]
pushI index carry ints = result where
  valAt = carry + atDef 0 ints index
  result
      | index >= length ints && carry == 0 = []
      | valAt == 3 = (-2) : (pushI (index + 1) 1 ints)
      | valAt == 4 = (-1) : (pushI (index + 1) 1 ints)
      | valAt == 5 = (0) : (pushI (index + 1) 1 ints)
      | otherwise = valAt : (pushI (index + 1) 0 ints)



toSNAFU2 :: Int -> String
toSNAFU2 i = result where
  base5 = toNormalBase5 i
  modified = pushI 0 0 base5
  fromDig :: Int -> Char
  fromDig (-1) = '-'
  fromDig (-2) = '='
  fromDig i = intToDigit i
  result = fmap fromDig $ reverse modified



toNormalBase5 :: Int -> [Int]
toNormalBase5 0 = [] where
toNormalBase5 n = result where
  (nextN, nextDig) = n `quotRem` 5
  result = nextDig : toNormalBase5 nextN

main :: IO ()
main = do
  t1 <- liftIO getCPUTime
  s <- resource "snafu"
  let entries = lines s
  let snafus = fmap (fromSNAFU) entries
  print (sum snafus)
  let result = toSNAFU2 (sum snafus)
  print $ result
  print $ fromSNAFU result

  t2 <- liftIO getCPUTime
  let diff = (fromIntegral (t2 - t1)) / (10^12)
  putStrLn $ "Computation time: " ++ show diff ++ " sec"