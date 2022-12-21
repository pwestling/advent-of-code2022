{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Advent20(main) where

import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Char
import Data.Functor
import Util
import Safe
import Data.Either
import Debug.Trace
import Data.Maybe
import Data.List
import qualified Data.Map as Map
import qualified Data.List.Split as Split
import System.CPUTime
import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Data.HashMap.Strict as HMap
import qualified Data.Text as Text
import Data.Hashable
import qualified Data.Set as Set
import Graph
import Data.Function
import Memo
import BTreeList


myMod :: Int -> Int -> Int
myMod x y = (x `mod` y + y) `mod` y

mix :: Int -> BTreeList (Int, Int) -> BTreeList (Int, Int)
mix n tree = result where
  (val, indexOfVal) = fromJust $ BTreeList.findIndex (\(i, _) -> i == n) tree
  withoutVal = remove indexOfVal tree
  adjustment = snd val
  newIndex = indexOfVal + adjustment
  len = BTreeList.size withoutVal
  circularNewIndex = newIndex `mod` len
  result = BTreeList.insert circularNewIndex val withoutVal
  trueResult = if ((toList result) !! circularNewIndex) == val then result else (error ("Failed consistency check " ++ show (toList result) ++ " " ++ show result ++ " " ++ show circularNewIndex))

doMix :: BTreeList (Int, Int) -> BTreeList (Int, Int)
doMix tree = foldl' (flip mix) tree [0..((BTreeList.size tree) - 1)]

main :: IO ()
main = do
  t1 <- liftIO getCPUTime
  s <- resource "mix-enc"
  let entries = runParser (parseNumber `sepBy` newline) () "input" s
  let stuff = case entries of
          Left err -> error $ show err
          Right stuff -> stuff
--  print entries
  let enckey = 811589153
  let indexedNumbers = toBTreeList (zip [0..] (fmap (* enckey) stuff)) :: BTreeList (Int, Int)

  let mixedTree = [runNTimes 10 doMix indexedNumbers] :: [BTreeList (Int, Int)]
  let resultAll = fmap (fmap snd) $ fmap toList mixedTree
  if (length resultAll < 30) then mapM_ print resultAll else print "too long"
  let result = last resultAll


  let zeroIndex = fromJust $ Data.List.findIndex (== 0) $ result
  print $ result !! ((zeroIndex + 1000) `mod` length result) + result !! ((zeroIndex + 2000) `mod` length result) + result !! ((zeroIndex + 3000) `mod` length result)


  t2 <- liftIO getCPUTime
  let diff = (fromIntegral (t2 - t1)) / (10^12)
  putStrLn $ "Computation time: " ++ show diff ++ " sec"