{-# LANGUAGE OverloadedStrings #-}

module Advent8(main) where

import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Char
import Data.Functor
import Util
import Data.List
import Safe
import Data.Char
import Data.Either
import Debug.Trace

data Point = Point Int Int deriving (Show, Eq)

up :: Point -> Point
up (Point x y) = Point x (y + 1)

down :: Point -> Point
down (Point x y) = Point x (y - 1)

left :: Point -> Point
left (Point x y) = Point (x - 1) y

right :: Point -> Point
right (Point x y) = Point (x + 1) y

get :: Point -> [[Int]] -> Maybe Int
get (Point x y) grid = do
  row <- grid `atMay` y
  row `atMay` x

example = "30373\n\
           \25512\n\
           \65332\n\
           \33549\n\
           \35390"

parseGrid :: Parser [[Int]]
parseGrid = do
  rows <- many1 digit `sepBy` newline
  return $ map (map digitToInt) rows

isVisible' :: [[Int]] -> (Point -> Point) -> Int -> Point -> Bool
isVisible' grid direction height point = result where
    heightHere = get point grid
    result =  case heightHere of
      Nothing -> True
      Just heightHere -> heightHere < height && isVisible' grid direction height (direction point)

isVisible :: [[Int]] -> Point -> Bool
isVisible grid point = result where
  height = get point grid
  result = case height of
    Nothing -> False
    Just height -> isVisible' grid up height (up point) ||
                   isVisible' grid down height (down point) ||
                   isVisible' grid left height (left point) ||
                   isVisible' grid right height (right point)


scenic' :: [[Int]] -> (Point -> Point) -> Int -> Point -> Int
scenic' grid direction height point = result where
    heightHere = get point grid
    result =  case heightHere of
      Nothing -> 0
      Just heightHere -> case heightHere < height of
         True -> 1 + scenic' grid direction height (direction point)
         False -> 1

scenic :: [[Int]] -> Point -> Int
scenic grid point = result where
  height = get point grid
  result = case height of
    Nothing -> 0
    Just height -> scenic' grid up height (up point) *
                   scenic' grid down height (down point) *
                   scenic' grid left height (left point) *
                   scenic' grid right height (right point)

main :: IO ()
main = do
  s <- resource "tree-grid"
  let entries = runParser parseGrid () "ls" s
  print entries
  let grid = fromRight [] entries
  let points = [Point x y | x <- [0..length (head grid) - 1], y <- [0..length grid - 1]]
  print $ length points
  print $ isVisible grid (Point 0 0)
  let visible = filter (isVisible grid) points
  print $ length visible
  let maxScenic = maximum $ fmap (scenic grid) points
  let vals = fmap (\p -> (p, (scenic grid p))) points
  print maxScenic

