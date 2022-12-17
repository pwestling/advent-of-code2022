{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Advent17(main) where

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

data Dir = L | R deriving (Show, Eq, Ord)

data Fill = Air | Rock | FallingRock | OOB deriving (Show, Eq, Ord)

data Tetris = Tetris {
    tetris :: Map.Map Point Fill,
    fallingPoints :: [Point],
    tetrisWidth :: Int,
    highestRockPoint :: Point,
    jets :: String,
    jetIndex :: Int,
    rocks :: [[Point]],
    rockIndex :: Int
} deriving (Show, Eq, Ord)

mkTetris :: String -> Tetris
mkTetris jets = Tetris {
    tetris = Map.empty,
    fallingPoints = [],
    tetrisWidth = 7,
    highestRockPoint = Point 0 (-1),
    jets = jets,
    jetIndex = 0,
    rocks = rockSequence,
    rockIndex = 0
}

tr :: Show a => String -> a -> a
tr s a = trace (s ++ show a) a

getAt :: Point -> Tetris -> Fill
getAt (Point x y) t = if isOOB then OOB else Map.findWithDefault Air (Point x y) t.tetris where
  isOOB = x < 0 || x >= t.tetrisWidth || y < 0

setAt :: Point -> Fill -> Tetris -> Tetris
setAt (Point x y) fill t = t { tetris = (Map.insert (Point x y) fill t.tetris), highestRockPoint = newHR } where
  newHR = if fill == Rock && y > getY t.highestRockPoint then Point x y else t.highestRockPoint

fallDown :: Point -> Point
fallDown (Point x y) = Point x (y - 1)

setFP :: [Point] -> Tetris -> Tetris
setFP fp t = t {fallingPoints = fp}

moveFallTo :: [Point] -> Tetris -> Tetris
moveFallTo fp tetris = setFP fp asFall where
    asAir = foldl' (\t p -> setAt p Air t) tetris (tetris.fallingPoints)
    asFall = foldl' (\t p -> setAt p FallingRock t) asAir fp

simulateFall :: Tetris -> Tetris
simulateFall tetris = result where
  newFallingPoints = fmap (fallDown) (tetris.fallingPoints)
  newFallFill = fmap (\p -> getAt p tetris) newFallingPoints
  areAllAir = all (\f -> f == Air || f == FallingRock) newFallFill
  newTetris = moveFallTo newFallingPoints tetris
  stableTetris = setFP [] $ foldl' (\t p -> setAt (p) Rock t) tetris (tetris.fallingPoints)
  result = if areAllAir then (newTetris) else (stableTetris)

simulateSideways :: Dir -> Tetris -> Tetris
simulateSideways dir tetris = result where
  newFallingPoints = fmap (\p -> case dir of
    L -> Point (getX p - 1) (getY p)
    R -> Point (getX p + 1) (getY p)) (tetris.fallingPoints)
  newFallFill = fmap (\p -> getAt p tetris) newFallingPoints
  areAllAir = all (\f -> f == Air || f == FallingRock) newFallFill
  newTetris = moveFallTo newFallingPoints tetris
  result = if areAllAir then setFP newFallingPoints newTetris else tetris

drawTetris :: Tetris -> String
drawTetris tetris = result where
  maxY = maximum $ fmap getY (Map.keys (tetris.tetris))
  rows = fmap (\y -> fmap (\x -> getAt (Point x y) tetris) [-1..(tetris.tetrisWidth)]) [maxY+1, maxY .. -1]
  result = unlines (fmap (\row -> fmap (\fill -> case fill of
    Air -> '.'
    Rock -> '#'
    FallingRock -> 'O'
    OOB -> 'X') row) rows)


getNextJet :: Tetris -> Dir
getNextJet tetris = case (tetris.jets) !! (tetris.jetIndex) of
  '<' -> L
  '>' -> R

insertRock :: [Point] -> Tetris -> Tetris
insertRock points tetris =  newTetris where
  (Point _ y) = tetris.highestRockPoint
  positionedPoints = fmap (\p -> p <> (Point 2 (y+4))) points
  newTetris' = foldl' (\t p -> setAt p FallingRock t) tetris positionedPoints
  newTetris = newTetris' { fallingPoints = positionedPoints }

runNTimes :: Int -> (a -> a) -> a -> a
runNTimes n f a = if n <= 0 then a else runNTimes (n-1) f (f a)

rockSequence :: [[Point]]
rockSequence = [
                  [(Point 0 0), (Point 1 0), (Point 2 0), (Point 3 0)],
                  [(Point 1 0), (Point 0 1), (Point 1 1), (Point 1 2), (Point 2 1)],
                  [(Point 0 0), (Point 1 0), (Point 2 0), (Point 2 1), (Point 2 2)],
                  [(Point 0 0), (Point 0 1), (Point 0 2), (Point 0 3)],
                  [(Point 0 0), (Point 0 1), (Point 1 0), (Point 1 1)]
               ]

data RockState = RockState Int deriving (Show, Eq, Ord)

getNextRock :: Tetris -> [Point]
getNextRock tetris = tetris.rocks !! tetris.rockIndex

incrementRockIndex :: Tetris -> Tetris
incrementRockIndex tetris = tetris { rockIndex = (tetris.rockIndex + 1) `mod` length (tetris.rocks) }

incrementJetIndex :: Tetris -> Tetris
incrementJetIndex tetris = tetris { jetIndex = (tetris.jetIndex + 1) `mod` length (tetris.jets) }

simulateOneMotion :: Tetris -> Tetris
simulateOneMotion tetris = result where
  jet = getNextJet tetris
  newTetris = incrementJetIndex $ simulateSideways jet tetris
  result = simulateFall newTetris

simulateOneRock :: Tetris -> Tetris
simulateOneRock tetris = result where
  rock = getNextRock tetris
  newTetris = insertRock rock tetris
  result = incrementRockIndex $ until (\t -> null (t.fallingPoints)) simulateOneMotion newTetris



main :: IO ()
main = do
  t1 <- liftIO getCPUTime
  s <- resource "elephant-tetris-example"
  let tetris = mkTetris s
  let result = (runNTimes 100000 simulateOneRock tetris)
  putStrLn $ drawTetris $ result
  print $ (getY result.highestRockPoint + 1)

  t2 <- liftIO getCPUTime
  let diff = (fromIntegral (t2 - t1)) / (10^12)
  putStrLn $ "Computation time: " ++ show diff ++ " sec"