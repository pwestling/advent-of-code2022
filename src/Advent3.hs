module Advent3(main) where

import Util
import qualified Data.Set as Set
import Data.List.Split
import Data.Char (isAsciiUpper)

priority :: Char -> Int
priority c = if isAsciiUpper c then
      fromEnum c - 64 + 26
    else
      fromEnum c - 96


findDuplicate :: String -> Char
findDuplicate s = x where
  frontHalf = take (length s `div` 2) s
  backHalf = drop (length s `div` 2) s
  x = head $ filter (`elem` frontHalf) backHalf


findShared :: [String] -> Char
findShared (xs : ys : zs : []) = x where
  s1 = Set.fromList xs
  s2 = Set.fromList ys
  s3 = Set.fromList zs
  x = head $ Set.toList $ Set.intersection s1 (Set.intersection s2 s3)
findShared _ = error "Invalid input"

main :: IO ()
main = do
  s <- resource "compartments"
  let inLines = lines s
  let result = sum $ fmap (priority . findDuplicate) inLines
  print result
  let groups = chunksOf 3 inLines
  let result2 = sum $ fmap (priority . findShared) groups
  print result2


