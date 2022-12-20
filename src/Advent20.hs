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

data ListZipper a = ListZipper {
    left :: [a],
    current :: a,
    right :: [a]
}

instance Functor ListZipper where
    fmap f (ListZipper l c r) = ListZipper (fmap f l) (f c) (fmap f r)

instance (Show a) => Show (ListZipper a) where
    show (ListZipper l c r) = show (reverse l) ++ " " ++ show c ++ " " ++ show r

toList :: ListZipper a -> [a]
toList (ListZipper l c r) = reverse l ++ [c] ++ r

toZipper :: [a] -> ListZipper a
toZipper (x:xs) = ListZipper [] x xs

zLeft :: ListZipper a -> ListZipper a
zLeft lz = case lz.left of
    [] -> let (r:rs) = reverse $ lz.right in ListZipper (rs ++ [lz.current]) (r) []
    (x:xs) -> ListZipper xs x (lz.current : lz.right)

zRight :: ListZipper a -> ListZipper a
zRight lz = case lz.right of
    [] -> let (l:ls) = reverse $ lz.left in ListZipper [] (l) (ls ++ [lz.current])
    (x:xs) -> ListZipper (lz.current : lz.left) x xs

moveRelative :: Int -> ListZipper a -> ListZipper a
moveRelative n lz = if n < 0 then moveRelative (n+1) (zLeft lz) else if n > 0 then moveRelative (n-1) (zRight lz) else lz

removeCurrent :: ListZipper a -> ListZipper a
removeCurrent (ListZipper left current (r:rs)) = ListZipper left r rs
removeCurrent lz@(ListZipper (l:ls) current []) = let (l:ls) = reverse $ lz.left in ListZipper [] (l) (ls)

zPop :: ListZipper a -> (a, ListZipper a)
zPop lz = (lz.current, removeCurrent lz)

tr :: Show a => String -> a -> a
tr s a = trace (s ++ show a) a

moveElementOver :: (Show a) => Int -> ListZipper a -> ListZipper a
moveElementOver to lz = let
    (e, lz') = zPop lz
    moved = moveRelative to lz'
    normalResult = ListZipper (moved.left) e (moved.current : moved.right)
    leftNullResult = ListZipper (reverse $ moved.current : moved.right) e []
    in if null moved.left then leftNullResult else normalResult


searchAndFocus :: (a -> Bool) -> ListZipper a -> ListZipper a
searchAndFocus f lz = if f (lz.current) then lz else searchAndFocus f (zRight lz)

mixNum :: Int -> ListZipper (Int, Int) -> ListZipper (Int, Int)
mixNum n lz = let
    lz' = searchAndFocus (\(i, _) -> i == n) lz
    in moveElementOver (snd $ (lz'.current)) (lz')

zModify :: (a -> a) -> ListZipper a -> ListZipper a
zModify f lz = ListZipper (lz.left) (f $ lz.current) (lz.right)

mixNum2 :: ListZipper (Int, Int, Int) -> ListZipper (Int, Int, Int)
mixNum2 lz = let
    (i, n, _) = lz.current
    in zRight $ zModify (\(ci, si, n) -> (ci+n, si, n)) lz

sortFn :: ValueState -> (Int, Int)
sortFn v = (v.currentIndex, v.subOrdering)


data IntState = IntState {
    mapByCurrentIndex :: Map.Map Int [ValueState],
    mapByStartIndex :: Map.Map Int ValueState
} deriving (Show)

data ValueState = ValueState {
    startIndex :: Int,
    currentIndex :: Int,
    subOrdering :: Int,
    value :: Int
} deriving (Eq)

instance Show ValueState where
    show (ValueState si ci so v) = show si ++ " " ++ show ci ++ " " ++ show so ++ " " ++ show v


data ArrivalDir = FromLeft | FromRight | None

insertValue :: IntState -> ValueState -> IntState
insertValue s t = result where
  destGroup = Map.findWithDefault [] t.currentIndex s.mapByCurrentIndex
  arrivalDirection = if t.value < 0 then FromRight else if t.value > 0 then FromLeft else None
  assignedSubOrder = case arrivalDirection of
    FromLeft -> (maximumDef 0 $ map (\v -> v.subOrdering) destGroup) + 1
    FromRight -> (minimumDef 0 $ map (\v -> v.subOrdering) destGroup) - 1
    None -> 0
  newTriple = ValueState t.startIndex t.currentIndex assignedSubOrder t.value
  newGroup = newTriple : destGroup
  newCurrentIndexMap = Map.insert t.currentIndex newGroup s.mapByCurrentIndex
  startIndexMap = Map.insert t.startIndex newTriple s.mapByStartIndex
  result = IntState newCurrentIndexMap startIndexMap

getValuesByCurrentIndex :: IntState -> Int -> [ValueState]
getValuesByCurrentIndex s i = Map.findWithDefault [] i s.mapByCurrentIndex

getValueByStartIndex :: IntState -> Int -> ValueState
getValueByStartIndex s i = Map.findWithDefault (ValueState i i 0 0) i s.mapByStartIndex

removeValue :: IntState -> ValueState -> IntState
removeValue s t = IntState (Map.adjust (delete t) t.currentIndex s.mapByCurrentIndex) (Map.delete t.startIndex s.mapByStartIndex)


mixNum3 :: IntState -> Int -> IntState
mixNum3 s n = let
    t = getValueByStartIndex (tr ("Start Op " ++ show n ++": ") s) n
    currGroup = getValuesByCurrentIndex s t.currentIndex
    adjustNum = if t.value < 0 then -1 else if t.value > 0 then 1 else 0
    newIndex = t.currentIndex + t.value
    newT = t { currentIndex = newIndex }
    in tr ("End Op " ++ show n ++": ") $ if t.value == 0 then s else insertValue (removeValue s t) newT


intStateToList :: IntState -> [Int]
intStateToList s = let
    values = concat $ Map.elems s.mapByCurrentIndex
    in fmap (.value) $ sortOn sortFn values

listToIntState :: [Int] -> IntState
listToIntState l = let
    values = zipWith (\i v -> ValueState i i 0 v) [0..] l
    in foldl insertValue (IntState Map.empty Map.empty) values

main :: IO ()
main = do
  t1 <- liftIO getCPUTime
  s <- resource "mix-enc-example"
  let entries = runParser (parseNumber `sepBy` newline) () "input" s
  let stuff = case entries of
          Left err -> error $ show err
          Right stuff -> stuff
--  print entries
  let indexedNumbers = listToIntState stuff

  let lz' = take (length stuff) $ scanl mixNum3 indexedNumbers [0..]
  let resultAll = fmap intStateToList lz'
  mapM_ print resultAll
  let result = last resultAll



--  let zeroIndex = fromJust $ findIndex (== 0) $ result
--  print $ result !! ((zeroIndex + 1000) `mod` length result) + result !! ((zeroIndex + 2000) `mod` length result) + result !! ((zeroIndex + 3000) `mod` length result)


  t2 <- liftIO getCPUTime
  let diff = (fromIntegral (t2 - t1)) / (10^12)
  putStrLn $ "Computation time: " ++ show diff ++ " sec"