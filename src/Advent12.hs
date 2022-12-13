module Advent12(main) where

import Util
import Data.List
import Safe
import Data.Maybe
import Debug.Trace
import Control.Monad
import Data.List.Index
import Graph

findIn :: (a -> Bool) -> [[a]] -> Maybe Point
findIn f grid = result where
  l = length $ head grid
  result = case findIndex f (concat grid) of
    Nothing -> Nothing
    Just index -> Just $ Point (index `mod` l) (index `div` l)

findAll :: (a -> Bool) -> [[a]] -> [Point]
findAll f grid = result where
  l = length $ head grid
  hits = findIndices f (concat grid)
  result = map (\index -> Point (index `mod` l) (index `div` l)) hits

findStart :: [[Char]] -> Point
findStart grid = result where
  Just result = findIn (== 'S') grid

findEnd :: [[Char]] -> Point
findEnd grid = result where
  Just result = findIn (== 'E') grid

getAt :: Point -> [[a]] -> Maybe a
getAt (Point x y) grid = (grid `atMay` y) >>= (`atMay` x)

setGridAt :: Point -> a -> [[a]] -> [[a]]
setGridAt (Point x y) value grid = result where
  row = grid !! y
  newRow = setAt x value row
  result = setAt y newRow grid


traversible :: Char -> Char -> Bool
traversible 'S' next = traversible 'a' next
traversible current 'E' = traversible current 'z'
traversible current next = diff <= 1 where
  diff = (fromEnum next) - (fromEnum current)

candidate :: Point -> [[Char]] -> (Point -> Point) ->  Maybe Point
candidate point grid direction  = result where
  nextPoint = direction point
  currentHeight = getAt point grid
  nextHeight = getAt nextPoint grid
  result = case (currentHeight, nextHeight) of
    (Just currentHeight, Just nextHeight) -> if traversible currentHeight nextHeight then Just nextPoint else Nothing
    _ -> Nothing

rank :: [[Char]] -> Point -> Point -> Int
rank grid end candidate = traceShow (distance end candidate) $ distance end candidate

withinRange :: [[Char]] -> Int -> Point -> Point -> Bool
withinRange grid remSteps end candidate = result where
  cartDistGood = distance end candidate <= remSteps
  heightGood = case (getAt candidate grid, getAt end grid) of
    (Just candidateHeight, Just endHeight) -> (fromEnum 'z') - (fromEnum candidateHeight) <= remSteps
    _ -> False
  result = cartDistGood && heightGood

maxInt = maxBound :: Int


createCache :: [[a]] -> [[Maybe [Point]]]
createCache grid = result where
  l = length $ head grid
  k = length grid
  result = replicate k $ replicate l Nothing

pathIsShorter :: [[Maybe [Point]]] -> Int -> Point -> Bool
pathIsShorter grid depth point = result where
  path = join $ getAt point grid
  result = case path of
    Nothing -> True
    Just path -> depth < length path

allPathsOfLength :: [[Maybe [Point]]] -> [[Char]] -> Int -> Point -> Int -> [Point] -> [[Point]]
allPathsOfLength cache grid depthLimit current depth path = result where
  candidates = filter (pathIsShorter cache depth ) $ filter (not . (`elem` path)) $ mapMaybe (candidate current grid) [goUp, goDown, goLeft, goRight]
  allPaths = do
     candidate <- candidates
     return $ allPathsOfLength cache grid depthLimit candidate (depth + 1) (current:path)
  result
    | depth == depthLimit = [current:path]
    | otherwise = join allPaths


integrateIntoCache :: [[Maybe [Point]]] -> [Point] -> [[Maybe [Point]]]
integrateIntoCache cache path = result where
  h = head path
  result = setGridAt h (Just path) cache


printCache :: [[Maybe [Point]]] -> String
printCache cache = intercalate "\n" result where
  result = do
    row <- cache
    return $ do
      cell <- row
      case cell of
        Nothing -> " "
        Just _ -> "X"



runStep :: [[Char]] -> [[Maybe [Point]]] -> Int -> [[Maybe [Point]]]
runStep grid cache depthLimit = result where
  start = findStart grid
  paths = allPathsOfLength cache grid (traceShow depthLimit depthLimit) start 0 []
  result = foldl integrateIntoCache cache paths

getEnd :: Point -> [[Maybe [Point]]] -> Maybe [Point]
getEnd endPoint cache  = traceShowId $ join $ getAt endPoint cache


findPath :: [[Char]] -> Maybe [Point]
findPath grid = result where
  start = findStart grid
  startCache = createCache grid :: [[Maybe [Point]]]
  djkstraSeq = (scanl (runStep grid) startCache [1..]) :: [[[Maybe [Point]]]]
  endPoint = findEnd grid
  result = head $ filter isJust $ fmap (getEnd endPoint) djkstraSeq


mkGridCandidates :: [[Char]] -> Point -> [Point]
mkGridCandidates grid point = result where
  candidates = mapMaybe (candidate point grid) [goUp, goDown, goLeft, goRight]
  result = candidates

isEnd :: [[Char]] -> Point -> Bool
isEnd grid point = result where
  Just result = (== 'E') <$> getAt point grid

main :: IO ()
main = do
  s <- resource "height-map"
  let grid = lines s
  let path = fromJust $ findPath grid
  let shortestPath = bfs (mkGridCandidates grid) (isEnd grid) (findStart grid)
  print $ length $ fromJust shortestPath
  let aLocs = findAll (\x -> x == 'a' || x == 'S') grid
  let shortestPaths = map (bfs (mkGridCandidates grid) (isEnd grid)) aLocs
  let shortestPathLengths = map (length . fromJust) $ filter isJust shortestPaths
  print $ minimum shortestPathLengths

