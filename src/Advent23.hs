{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Advent23 where

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

import qualified Data.Map as Map

data Elf = Elf  deriving (Show, Eq, Ord)

data Grid = Grid {
  cells :: Map.Map Point Elf
} deriving (Show, Eq, Ord)

data Direction = N | E | S | W | NE | SE | SW | NW deriving (Show, Eq, Ord)


parseGrid :: Parser Grid
parseGrid = do
  rows <- (many1 (oneOf " .#")) `sepEndBy` newline
  return $ toGrid rows

toGrid :: [[Char]] -> Grid
toGrid s = result where
  toRow :: Int -> [Char] -> [(Point, Elf)]
  toRow y = map (\(p,c) -> (p, Elf)) . filter (\(p,c) -> c == '#') . map (\(x, c) -> (Point x y, c)) . zip [0..]
  toRows :: [[Char]] -> [(Point, Elf)]
  toRows = concat . zipWith toRow [0..]
  rows = toRows s
  rowMap = Map.fromList rows
  result = Grid rowMap

renderGrid :: Grid -> String
renderGrid grid = result where
  maxX = 13 -- (maximum $ map (getX . fst) rows)
  maxY = 13 --(maximum $ map (getY . fst) rows)
  minX = 0 -- (minimum $ map (getX . fst) rows)
  minY = 0 -- (minimum $ map (getY . fst) rows)
  rows = Map.toList $ grid.cells
  toRow :: Int -> [Char]
  toRow y = map (\x -> toChar $ Map.lookup (Point x y) (grid.cells)) [minX..maxX]
  toRows :: [[Char]]
  toRows = map toRow [minY..maxY]
  result = unlines toRows
  toChar :: Maybe Elf -> Char
  toChar Nothing = '.'
  toChar (Just Elf) = '#'



direction :: Int -> [Direction]
direction time = result where
  index = time `mod` 4
  result = take 4 $ drop index $ cycle [N, S, W, E]

move :: Point -> Direction -> Point
move p N = Point (getX p) (getY p - 1)
move p E = Point (getX p + 1) (getY p)
move p S = Point (getX p) (getY p + 1)
move p W = Point (getX p - 1) (getY p)
move p NE = Point (getX p + 1) (getY p - 1)
move p SE = Point (getX p + 1) (getY p + 1)
move p SW = Point (getX p - 1) (getY p + 1)
move p NW = Point (getX p - 1) (getY p - 1)

cardinalToSide :: Direction -> [Direction]
cardinalToSide N = [N, NE, NW]
cardinalToSide E = [E, NE, SE]
cardinalToSide S = [S, SE, SW]
cardinalToSide W = [W, NW, SW]

point8Neighbors :: Point -> [Point]
point8Neighbors p = map (move p) [N, E, S, W, NE, SE, SW, NW]

elfDirectionProposal :: [Direction] -> Grid -> Point -> Maybe (Point, Point)
elfDirectionProposal dirOrder grid p = if shouldMove then (\prop -> (prop, p)) <$> proposal else Nothing where
  shouldMove = any (\pt -> Map.member pt (grid.cells)) $ point8Neighbors p
  sideHasNoElf d = not $ any (\pt -> Map.member pt (grid.cells)) $ map (move p) $ cardinalToSide d
  db x = if shouldMove then trace (show p ++ " detected elf at " ++ show (head $ filter (\pt -> Map.member pt (grid.cells)) $ point8Neighbors p) ++ " and should move") x else x
  proposal = fmap (\d -> move p d) $ headMay $ filter sideHasNoElf (dirOrder)

moveElfToLoc :: Grid -> (Point, Point) -> Grid
moveElfToLoc grid (to, from)  = result where
  newCells = Map.insert to Elf $ Map.delete from (grid.cells)
  result = grid { cells = newCells }

evolveGrid :: Maybe Grid -> Int -> Maybe Grid
evolveGrid Nothing _ = Nothing
evolveGrid (Just grid) time = if didMove then Just result else Nothing where
  dirOrder = direction time
  allProposals = mapMaybe (elfDirectionProposal dirOrder grid) $ Map.keys $ grid.cells
  validProposal (proposal, elf) = not $ any (\(p, e) -> p == proposal && e /= elf) allProposals
  db k@(proposal, elf) = if validProposal k then trace ("Elf at " ++ show elf ++ " moving to " ++ show proposal) k else trace ("Elf at " ++ show elf ++ " staying put ") k
  validProposals = filter validProposal $ allProposals
  didMove = length validProposals > 0
  result = foldl moveElfToLoc grid validProposals

findBounds :: Grid -> (Point, Point)
findBounds grid = result where
  maxX = (maximum $ map (getX . fst) rows)
  maxY = (maximum $ map (getY . fst) rows)
  minX = (minimum $ map (getX . fst) rows)
  minY = (minimum $ map (getY . fst) rows)
  rows = Map.toList $ grid.cells
  result = (Point minX minY, Point maxX maxY)

findEmptySpaceWithinBounds :: Grid -> Int
findEmptySpaceWithinBounds grid = result where
  (Point minX minY, Point maxX maxY) = findBounds grid
  allPoints = [Point x y | x <- [minX..maxX], y <- [minY..maxY]]
  result = length $ filter (\p -> not $ Map.member p (grid.cells)) allPoints

main :: IO ()
main = do
  t1 <- liftIO getCPUTime
  s <- resource "elf-grid-example"
  let entries = runParser (parseGrid ) () "input" s
  let stuff = case entries of
          Left err -> error $ show err
          Right stuff -> stuff


  let result = last $ zip [1..] $ takeWhile isJust $ scanl (evolveGrid) (Just stuff) [0..]
--  putStrLn $ renderGrid result
  print $ findEmptySpaceWithinBounds (fromJust $ (snd result))
  print $ fst result


  t2 <- liftIO getCPUTime
  let diff = (fromIntegral (t2 - t1)) / (10^12)
  putStrLn $ "Computation time: " ++ show diff ++ " sec"