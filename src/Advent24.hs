{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Advent24 where

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
import Data.List
import Graph
import Search
import Data.Hashable

import qualified Data.Map as Map

data Direction = N | E | S | W | Wait deriving (Show, Eq, Ord)

instance Hashable Direction where
    hashWithSalt salt N = salt + 1
    hashWithSalt salt E = salt + 2
    hashWithSalt salt S = salt + 3
    hashWithSalt salt W = salt + 4

data Blizzard = Blizzard {
  dir :: Direction
} deriving (Show, Eq, Ord)

instance Hashable Blizzard where
  hashWithSalt salt blizzard = hashWithSalt salt (blizzard.dir)

data Cell = Wall | Blizz [Blizzard] | Open deriving (Show, Eq, Ord)

instance Hashable Cell where
  hashWithSalt salt Wall = salt
  hashWithSalt salt Open = salt + 1
  hashWithSalt salt (Blizz blizzards) = salt + 2 + hash blizzards

data MapState = MapState {
  cells :: Map.Map Point Cell,
  partLoc :: Point,
  time :: Int
}

instance Eq MapState where
  (==) a b = (a.time, a.partLoc) == (b.time, b.partLoc)

instance Ord MapState where
  compare a b = compare (a.time, a.partLoc) (b.time, b.partLoc)

beamCompare :: MapState -> MapState -> Ordering
beamCompare a b = compare (b.time) (a.time)

instance Show MapState where
  show (MapState cells partLoc time) = "MS: "++show partLoc++" "++show time

instance Hashable MapState where
  hashWithSalt salt (MapState cells partLoc time) = hashWithSalt salt (partLoc, time)

parseCells :: Parser (Map.Map Point Cell)
parseCells = do
  rows <- (many1 (oneOf " .#><v^")) `sepEndBy` newline
  return $ toCells rows

toCell :: Char -> Cell
toCell c = case c of
  '#' -> Wall
  '.' -> Open
  '>' -> Blizz [Blizzard E]
  '<' -> Blizz [Blizzard W]
  'v' -> Blizz [Blizzard S]
  '^' -> Blizz [Blizzard N]

toCells :: [[Char]] -> Map.Map Point Cell
toCells s = result where
  toRow :: Int -> [Char] -> [(Point, Cell)]
  toRow y = map (\(p,c) -> (p, toCell c)) . map (\(x, c) -> (Point x y, c)) . zip [0..]
  toRows :: [[Char]] -> [(Point, Cell)]
  toRows = concat . zipWith toRow [0..]
  rows = toRows s
  rowMap = Map.fromList rows
  result = rowMap

renderMap :: MapState -> String
renderMap map = result where
  maxX = (maximum $ fmap (getX . fst) rows)
  maxY = (maximum $ fmap (getY . fst) rows)
  minX = (minimum $ fmap (getX . fst) rows)
  minY = (minimum $ fmap (getY . fst) rows)
  rows = Map.toList $ map.cells
  toRow :: Int -> [Char]
  toRow y = fmap (\x -> if (Point x y) /= map.partLoc then toChar $ Map.lookup (Point x y) (map.cells) else 'E') [minX..maxX]
  toRows :: [[Char]]
  toRows = fmap toRow [minY..maxY]
  result = show map.time ++ "\n" ++ unlines toRows
  toChar :: Maybe Cell -> Char
  toChar Nothing = '.'
  toChar (Just Wall) = '#'
  toChar (Just Open) = '.'
  toChar (Just (Blizz ((Blizzard E):[]))) = '>'
  toChar (Just (Blizz ((Blizzard W):[]))) = '<'
  toChar (Just (Blizz ((Blizzard S):[]))) = 'v'
  toChar (Just (Blizz ((Blizzard N):[]))) = '^'
  toChar (Just (Blizz (b:bs))) = intToDigit $ length $ b:bs


findLeftTopMostOpenCell :: Map.Map Point Cell -> Point
findLeftTopMostOpenCell cells = result where
  rows = Map.toList cells
  result = head $ sort $ fmap fst $ filter (\(_, c) -> c == Open) $ rows

move :: Point -> Direction -> Point
move p N = Point (getX p) (getY p - 1)
move p E = Point (getX p + 1) (getY p)
move p S = Point (getX p) (getY p + 1)
move p W = Point (getX p - 1) (getY p)
move p Wait = p

opposite :: Direction -> Direction
opposite N = S
opposite E = W
opposite S = N
opposite W = E
opposite Wait = Wait

resetToOppositeWall :: MapState -> Point -> Direction -> (Blizzard, Point)
resetToOppositeWall map p d = result where
  next = move p (opposite d)
  nextCell = Map.lookup next (map.cells)
  result = case nextCell of
    Just Wall -> (Blizzard d, p)
    Just Open -> resetToOppositeWall map next d
    Just (Blizz (b:bs)) -> resetToOppositeWall map next d
    Nothing -> error "resetToOppositeWall: Nothing"

addBlizzardToCell :: Blizzard -> Cell -> Maybe Cell
addBlizzardToCell b (Blizz bs) = Just $ Blizz (b:bs)
addBlizzardToCell b Open = Just $ Blizz [b]
addBlizzardToCell b Wall = Just $  error "addBlizzardToCell: Wall "

removeBlizzardFromCell :: Blizzard -> Cell -> Maybe Cell
removeBlizzardFromCell b (Blizz bs) = Just result where
  newBlizz = delete b bs
  result = if null newBlizz then Open else Blizz newBlizz

evolveBlizzards :: MapState -> MapState
evolveBlizzards map = result where
  blizzards = Map.toList $ Map.filter (\c -> case c of { Blizz _ -> True; _ -> False }) $ map.cells
  blizzardsExpanded = concatMap (\(p, Blizz bs) -> fmap (\b -> (p, b)) bs) blizzards
  evolveBlizzard :: (Point, Blizzard) -> MapState -> MapState
  evolveBlizzard (p, bz@(Blizzard d)) map = result where
    newP = case d of
      N -> Point (getX p) (getY p - 1)
      E -> Point (getX p + 1) (getY p)
      S -> Point (getX p) (getY p + 1)
      W -> Point (getX p - 1) (getY p)
    newCell = Map.lookup newP (map.cells)
    (newBlizzard, trueP) = case newCell of
      Just Wall -> resetToOppositeWall map p d
      Just Open -> (bz, newP)
      Just (Blizz bs) -> (bz, newP)
      Nothing -> error "evolveBlizzard: Nothing"
    newCells = Map.update (addBlizzardToCell newBlizzard) trueP $ Map.update (removeBlizzardFromCell (Blizzard d)) p $ map.cells
    result = MapState newCells (map.partLoc) (map.time)
  result = foldl (\m (p, c) -> evolveBlizzard (p, c) m) (map {time = map.time + 1}) blizzardsExpanded

findBottomMostOpenCell :: Map.Map Point Cell -> Point
findBottomMostOpenCell cells = result where
  rows = Map.toList cells
  result = last $ sort $ fmap fst $ filter (\(_, c) -> c == Open) $ rows

generateMovesSearch :: [Map.Map Point Cell] -> Point -> Maybe MapState -> MapState -> [MapState]
generateMovesSearch cachedMaps dest prevBest map = scResult where
  cachedCells = cachedMaps !! ((map.time + 1))
  nextMap = MapState cachedCells (map.partLoc) (map.time + 1)
  moves = [N, E, S, W, Wait]
  moveMap :: Direction -> MapState -> Maybe MapState
  moveMap d map = result where
    newP = move (map.partLoc) d
    newCell = Map.lookup newP (map.cells)
    result = case newCell of
      Just Wall -> Nothing
      Just Open -> Just $ MapState (map.cells) newP (map.time)
      Just (Blizz bs) -> Nothing
      Nothing -> Nothing
  best = fromJust prevBest
  bestPossibleTime = distance (map.partLoc) dest + map.time
  result = sortOn (\s -> distance (s.partLoc) dest) $ (mapMaybe (\d -> moveMap d nextMap) moves)
  scResult = if isJust prevBest && bestPossibleTime >= best.time then
                []
             else
               result

generateMoves :: [Map.Map Point Cell] -> Point -> MapState -> [MapState]
generateMoves cachedMaps dest map = scResult where
  cachedCells = cachedMaps !! ((map.time + 1))
  nextMap = MapState cachedCells (map.partLoc) (map.time + 1)
  moves = [N, E, S, W, Wait]
  moveMap :: Direction -> MapState -> Maybe MapState
  moveMap d map = result where
    newP = move (map.partLoc) d
    newCell = Map.lookup newP (map.cells)
    result = case newCell of
      Just Wall -> Nothing
      Just Open -> Just $ MapState (map.cells) newP (map.time)
      Just (Blizz bs) -> Nothing
      Nothing -> Nothing
  cutOff = 234
  bestPossibleTime = distance (map.partLoc) dest + map.time
  result = sortOn (\s -> distance (s.partLoc) dest) $ (mapMaybe (\d -> moveMap d nextMap) moves)
  scResult = result


main :: IO ()
main = do
  t1 <- liftIO getCPUTime
  s <- resource "blizzard"
  let entries = runParser (parseCells) () "input" s
  let stuff = case entries of
          Left err -> error $ show err
          Right stuff -> stuff
  let map = MapState stuff (findLeftTopMostOpenCell stuff) 0
  let mapCache = fmap (.cells) $ iterate evolveBlizzards map

--  mapM_ putStrLn $ fmap renderMap $ fmap (\(i,c) -> MapState c (Point 0 0) i) $ take 20 $ zip [0..] mapCache

  let dest = findBottomMostOpenCell stuff
  let startP = findLeftTopMostOpenCell stuff
  putStrLn $ "Dest: " ++ show dest
--  putStrLn $ renderMap map

  let searchResult = fromJust $ solutionSearch beamCompare (generateMovesSearch mapCache dest) (\s -> s.partLoc == dest) map
  putStrLn $ renderMap searchResult
  print $ (searchResult.time)

--  let bfsResult = fromJust $ bfs (generateMoves mapCache dest) (\s ->  s.partLoc == dest) map
--  mapM_ putStrLn $ fmap renderMap $ (reverse bfsResult)
--  let goalReach = head bfsResult
--  let bfsResult2 = fromJust $ bfs (generateMoves mapCache dest) (\s ->  s.partLoc == startP) goalReach
--  let startReach = head bfsResult2
--  let bfsResult3 = fromJust $ bfs (generateMoves mapCache dest) (\s ->  s.partLoc == dest) startReach
--  let goalReach2 = head bfsResult3

--  print $ (((goalReach2).time))

  t2 <- liftIO getCPUTime
  let diff = (fromIntegral (t2 - t1)) / (10^12)
  putStrLn $ "Computation time: " ++ show diff ++ " sec"

-- 234 too high
-- 205 too low