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
import qualified Data.HashMap.Strict as HMap
import qualified Data.Text as Text

data Dir = L | R deriving (Show, Eq, Ord)

data Fill = Air | Rock | FallingRock | OOB deriving (Show, Eq, Ord)

data Tetris = Tetris {
    tetris :: HMap.HashMap Point Fill,
    fallingPoints :: [Point],
    tetrisWidth :: Int,
    highestRockPoint :: Point,
    jets :: Text.Text,
    jetIndex :: Int,
    rocks :: [[Point]],
    rockIndex :: Int
} deriving (Show, Eq, Ord)

mkTetris :: Text.Text -> Tetris
mkTetris jets = Tetris {
    tetris = HMap.empty,
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
getAt (Point x y) t = if isOOB then OOB else HMap.findWithDefault Air (Point x y) t.tetris where
  isOOB = x < 0 || x >= t.tetrisWidth || y < 0

setAt :: Point -> Fill -> Tetris -> Tetris
setAt (Point x y) fill t = t { tetris = (HMap.insert (Point x y) fill t.tetris), highestRockPoint = newHR } where
  newHR = if fill == Rock && y > getY t.highestRockPoint then Point x y else t.highestRockPoint

fallDown :: Point -> Point
fallDown (Point x y) = Point x (y - 1)

setFP :: [Point] -> Tetris -> Tetris
setFP fp t = t {fallingPoints = fp}

moveFallTo :: [Point] -> Tetris -> Tetris
moveFallTo fp tetris = setFP fp tetris
--where
--    asAir = foldl' (\t p -> setAt p Air t) tetris (tetris.fallingPoints)
--    asFall = foldl' (\t p -> setAt p FallingRock t) asAir fp

simulateFall :: Tetris -> Tetris
simulateFall tetris = result where
  newFallingPoints = fmap (fallDown) (tetris.fallingPoints)
  newFallFill = fmap (\p -> getAt p tetris) newFallingPoints
  areAllAir = all (\f -> f == Air || f == FallingRock) newFallFill
  newTetris = moveFallTo newFallingPoints tetris
  stableTetris = eliminateLines (tetris.fallingPoints) $ setFP [] $ foldl' (\t p -> setAt (p) Rock t) tetris (tetris.fallingPoints)
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
  maxY = getY $ tetris.highestRockPoint
  rows = fmap (\y -> fmap (\x -> getAt (Point x y) tetris) [-1..(tetris.tetrisWidth)]) [maxY+1, maxY .. -1]
  result = unlines (fmap (\row -> fmap (\fill -> case fill of
    Air -> '.'
    Rock -> '#'
    FallingRock -> 'O'
    OOB -> 'X') row) rows)


getNextJet :: Tetris -> Dir
getNextJet tetris = case (tetris.jets) `Text.index` (tetris.jetIndex) of
  '<' -> L
  '>' -> R

insertRock :: [Point] -> Tetris -> Tetris
insertRock points tetris =  newTetris where
  (Point _ y) = tetris.highestRockPoint
  positionedPoints = fmap (\p -> p <> (Point 2 (y+4))) points
  newTetris = tetris { fallingPoints = positionedPoints }

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
incrementJetIndex tetris = tetris { jetIndex = (tetris.jetIndex + 1) `mod` Text.length (tetris.jets) }

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

data TetrisKey = TetrisKey {
    rockIndex :: Int,
    jetIndex :: Int,
    tetris :: [(Point,Fill)]
} deriving (Show, Eq, Ord)

tetrisKey :: Tetris -> TetrisKey
tetrisKey tetris = TetrisKey {
    rockIndex = tetris.rockIndex,
    jetIndex = tetris.jetIndex,
    tetris = transformedPoints

} where
      tetrisPoints = HMap.toList (tetris.tetris)
      minY = minimum $ fmap (getY . fst) (tetrisPoints)
      transformedPoints = fmap (\(p,f) -> (p <> (Point 0 (-minY)), f)) tetrisPoints

eliminateLines :: [Point] -> Tetris -> Tetris
eliminateLines pointsThatWereSet tetris = newTetris where
  maxY = getY tetris.highestRockPoint
  minY = minimum $ fmap getY pointsThatWereSet
  -- get the minimum row that is all rocks
  minRow = fromMaybe 0 $ minimum $ fmap (\y -> if all (\x -> getAt (Point x y) tetris == Rock) [-1..(tetris.tetrisWidth)] then Just y else Nothing) [minY..maxY]
  -- remove all keys from the tetris map that are lower than the minRow
  minRowGuess = minY - 50
  newTetris = trace ("ELiminating lines below " ++ show minRowGuess ) $ tetris { tetris = HMap.filterWithKey (\p _ -> getY p >= minRowGuess) (tetris.tetris) } :: Tetris

findCycle :: Map.Map TetrisKey (Tetris, Int) -> Int -> Tetris -> (Int, Tetris, (Tetris, Int))
findCycle cache n t = result where
  nextResult = simulateOneRock t
--  halfWayResult = Map.lookup (n `div` 2) cache
--  halfWayResultIsHalfHeight = getY (fromJust halfWayResult).highestRockPoint == (getY (nextResult.highestRockPoint) `div` 2)
  currentKey = tetrisKey nextResult
  isRepeat = seq (if n `mod` 100000 == 0 then (traceShow n $ ()) else ()) $ Map.member currentKey cache

  result = if isRepeat then
              (n, nextResult, cache Map.! currentKey) else
              findCycle (Map.insert currentKey (nextResult, n) cache ) (n+1) nextResult

main :: IO ()
main = do
  t1 <- liftIO getCPUTime
  s <- resource "elephant-tetris"
  let tetris = mkTetris (Text.pack s)
--  let result = (runNTimes 8000 simulateOneRock tetris)
--  putStrLn $ drawTetris $ result
--  let simulatedHeight = (getY result.highestRockPoint + 1)
--  print $ simulatedHeight
  let (cycleN, endCycle, (startCycle, startN)) = findCycle Map.empty 0 tetris

  putStrLn $ "Cycle start at: " ++ (show startN)
  putStrLn $ "Cycle ends at: " ++ (show cycleN)

  putStrLn $ "Cycle troughs at: " ++ show (getY startCycle.highestRockPoint)
  putStrLn $ "Cycle peaks at: " ++ show (getY endCycle.highestRockPoint)

  let targetN = 1000000000000
  let cycleLength = cycleN - startN
  let cycleHeight = (getY endCycle.highestRockPoint) - (getY startCycle.highestRockPoint)
  putStrLn $ "Cycle has a height of: " ++ show cycleHeight
  putStrLn $ "Cycle has a length of: " ++ show cycleLength
  let remainingCycles = targetN - cycleN
  let heightIncrement = remainingCycles `div` cycleLength
  let remainder = remainingCycles `mod` cycleLength

  let remainderSim = (runNTimes remainder simulateOneRock startCycle)
  let additionalHeight = (getY remainderSim.highestRockPoint) - (getY startCycle.highestRockPoint)

  let remainingHeight = heightIncrement * cycleHeight
  let totalHeight = remainingHeight + (getY endCycle.highestRockPoint) + additionalHeight
  putStrLn $ "Awkward remained: " ++ show remainder
  putStrLn $ "Remaineder add height: " ++ show additionalHeight


  putStrLn $ "Height computed from cycle: " ++ show totalHeight

--  let actual = 1514285714288
--  print $ actual - totalHeight
--  putStrLn $ "Difference from exepectation: " ++ show (actual - totalHeight)
--  putStrLn $ "Difference from simulation: " ++ show (simulatedHeight - totalHeight)

  t2 <- liftIO getCPUTime
  let diff = (fromIntegral (t2 - t1)) / (10^12)
  putStrLn $ "Computation time: " ++ show diff ++ " sec"

