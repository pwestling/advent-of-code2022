module Advent14(main) where

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
import Data.List.Index
import Data.List
import qualified Data.Map as Map
import qualified Data.List.Split as Split

data Fill = Rock | FallingSand | Sand | Air deriving (Show, Eq)
data Grid = Grid (Map.Map Point Fill) (Maybe Point) deriving (Show)

getAt :: Point -> Grid -> Fill
getAt p (Grid grid fs) =  fromMaybe Air (Map.lookup p grid)

setGridAt :: Point -> Fill -> Grid -> Grid
setGridAt p FallingSand (Grid grid fs) = Grid (Map.insert p FallingSand grid) (Just p)
setGridAt p value (Grid grid fs) = Grid (Map.insert p value grid) fs

parsePoint :: Parser Point
parsePoint = Point <$> (read <$> many1 digit) <*> (char ',' *> (read <$> many1 digit))

parseSection :: Parser [Point]
parseSection = parsePoint `sepBy` try ( spaces >> string "->" >> spaces)

parseSections :: Parser [[Point]]
parseSections = parseSection `sepBy` newline


drawLineInGrid :: Grid -> Point -> Point -> Grid
drawLineInGrid grid (Point x1' y1') (Point x2' y2') = foldl' (\g p -> setGridAt p Rock g) grid points where
  x1 = min x1' x2'
  x2 = max x1' x2'
  y1 = min y1' y2'
  y2 = max y1' y2'
  points = if x1 == x2 then [Point x1 y | y <- [y1..y2]] else [Point x y1 | x <- [x1..x2]]

drawSectionsInGrid :: Grid -> [Point] -> Grid
drawSectionsInGrid grid points = foldl' (\g (p1, p2) -> drawLineInGrid g p1 p2) grid pairs where
  pairs = zip points (tail points)




drawGrid :: Int -> Int -> Int -> Grid -> String
drawGrid lowY width height (Grid grid _) = unlines $ map (map (\p@(Point px py) -> case getAt p (Grid grid Nothing ) of
  Rock -> '#'
  Sand -> 'O'
  FallingSand -> '$'
  Air -> if py == lowY + 2 then '#' else '.')) $ Split.chunksOf (2*width+1) [Point x y | y <- [0..height], x <- [(500-width)..(500+width)]]


stabilize :: Grid -> Grid
stabilize g@(Grid m (Just fs@(Point _ fsy))) = Grid g' Nothing where
  (Grid g' _) = setGridAt fs Sand g


evolveFallingSand :: Int -> Grid -> Grid
evolveFallingSand caveFloor g@(Grid grid Nothing) = g where
evolveFallingSand caveFloor g@(Grid grid (Just fallingSand@(Point _ fsy))) = result where
  belowFs = goUp fallingSand
  belowLeftFs = goUp $ goLeft fallingSand
  belowRightFs = goUp $ goRight fallingSand
  pointToMoveTo = case (getAt belowFs g, getAt belowLeftFs g, getAt belowRightFs g) of
    (Air, _, _) -> Just belowFs
    (_, Air, _) -> Just belowLeftFs
    (_, _, Air) -> Just belowRightFs
    _ -> Nothing
  result = if fsy == caveFloor then stabilize g else
    case pointToMoveTo of
      Just p -> setGridAt p FallingSand $ setGridAt fallingSand Air g
      Nothing -> stabilize g

lowestY :: Grid -> Int
lowestY (Grid grid _) = maximum $ map (\(Point _ y) -> y) $ filter (\p -> Map.lookup p grid == Just Rock) $ Map.keys grid

converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

runOneSand :: Int -> Grid -> Grid
runOneSand caveFloor g = result where
  start = setGridAt (Point 500 0) FallingSand g
  result = until (\(Grid _ fs) -> isNothing fs) (evolveFallingSand caveFloor) start


countSand :: Grid -> Int
countSand (Grid grid _) = length $ filter (== Sand) $ Map.elems grid

pointsInCone :: Grid -> [Point]
pointsInCone g = do
  let floorY = lowestY g + 1
  y <- [0..floorY]
  x <- [(500 - y)..(500 + y)]
  let p = Point x y
  return p

coneEvolve :: Grid -> Point -> Grid
coneEvolve g p = result where
  isSand p = getAt p g == Sand
  isAir p = getAt p g == Air
  shouldBeSand = isAir p &&
                 (isSand (goDown p) ||
                 isSand (goDown $ goLeft p) ||
                 isSand (goDown $ goRight p))
  result = if shouldBeSand then setGridAt p Sand g else g

runCone :: Grid -> Grid
runCone g = foldl' coneEvolve g' (pointsInCone g) where
  g' = setGridAt (Point 500 0) Sand g


main :: IO ()
main = do
  s <- resource "falling-sand"
  let entries = runParser parseSections () "input" s
  let stuff = case entries of
          Left err -> error $ show err
          Right stuff -> stuff
  print entries
  let grid = foldl' drawSectionsInGrid (Grid Map.empty Nothing) stuff
  print $ grid
--  putStr $ drawGrid 7 12 grid
  let caveFloor = lowestY grid
  let result = runCone grid
  let graphic =  drawGrid caveFloor 160 156 result
  writeFile "graphic.txt" graphic
  print $ countSand result
  let slowResult = until (\g -> getAt (Point 500 0) g == Sand) (runOneSand (caveFloor+1)) grid
  let graphic2 =  drawGrid caveFloor 160 156 slowResult
  writeFile "graphic2.txt" graphic2
  print $ countSand slowResult


