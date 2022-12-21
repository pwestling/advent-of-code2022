{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module BTreeList where

import Data.List.Split
import Debug.Trace
import Safe (headMay)
import Data.Maybe (isNothing)


data BTreeList a = BTreeBranch { branchSize :: Int, branches :: [BTreeList a] } | BTreeLeaf{leafSize :: Int, values :: [a] } deriving (Show, Eq, Ord)

size :: BTreeList a -> Int
size (BTreeBranch s _) = s
size (BTreeLeaf s _) = s

get :: Int -> BTreeList a -> a
get index (BTreeLeaf _ xs) = xs !! index
get index (BTreeBranch _ xs) = result where
  next n (branch: branches) = if n >= size branch then next (n - size branch) branches else (n, branch)
  (n, branch) = next index xs
  result = get n branch

targetSize :: Int
targetSize = 64

maybeSplit :: (Show a) => BTreeList a -> BTreeList a
maybeSplit b@(BTreeLeaf n xs) = if n > (targetSize * targetSize) then (result) else b where
  groups = chunksOf targetSize xs
  makeLeaf xs = BTreeLeaf (length xs) xs
  result = BTreeBranch n (map makeLeaf groups)

toList :: BTreeList a -> [a]
toList (BTreeLeaf _ xs) = xs
toList (BTreeBranch _ xs) = concatMap toList xs

insert :: (Show a) => Int -> a -> BTreeList a -> BTreeList a
insert index value (BTreeLeaf s xs) = maybeSplit $ BTreeLeaf (s + 1) (take index xs ++ [value] ++ drop index xs)
insert index value (BTreeBranch s xs) = BTreeBranch (s + 1) (result) where
  next :: Int -> Int -> [BTreeList a] -> (Int, Int, BTreeList a)
  next i n (branch: branches) = if ((n >= size branch)) then next (i+1) (( n) - (size ( branch))) branches else ((i, n, branch))
  next i n [] = (i, n, BTreeLeaf 0 [])
  (i, n, branch) = next 0 index xs
  newBranch = insert ( n) value ( branch)
  result = take i xs ++ [newBranch] ++ drop (i+1) xs

remove :: (Show a) => Int -> BTreeList a -> BTreeList a
remove index l@(BTreeLeaf s xs) = BTreeLeaf (s - 1) (take index xs ++ drop (index + 1) xs)
remove index (BTreeBranch s xs) = BTreeBranch (s - 1) (result) where
  next :: Int -> Int -> [BTreeList a] -> (Int, Int, BTreeList a)
  next i n (branch: branches) = if ((n >= size branch)) then next (i+1) (( n) - (size ( branch))) branches else (i, n, branch)
  next i n [] = ((error "index out of bounds"))
  (i, n, branch) = next 0 index xs
  newBranch = remove ( n) ( branch)
  result = take i xs ++ [newBranch] ++ drop (i+1) xs

findIndex :: (a -> Bool) -> BTreeList a -> Maybe (a, Int)
findIndex f (BTreeLeaf _ xs) = result where
  result = headMay $ filter (f . fst) $ zip xs [0..]
findIndex f (BTreeBranch _ xs) = result where
  searches = map (findIndex f) xs
  noMatchBranches = takeWhile (isNothing) searches
  indexOfMatch = length noMatchBranches
  result = if indexOfMatch >= length xs then Nothing else fmap (\(value, i) -> (value, i + sum (map size (take indexOfMatch xs)))) (searches !! indexOfMatch)


empty :: BTreeList a
empty = BTreeLeaf 0 []

toBTreeList :: (Show a) => [a] -> BTreeList a
toBTreeList xs = result where
  result = foldl (\btreeList (i, x) -> insert i x btreeList) empty (zip [0..] xs)



example = BTreeBranch 6 [BTreeLeaf 4 [1,2,3,4], BTreeLeaf 1 [5], BTreeLeaf 1 [6]]

