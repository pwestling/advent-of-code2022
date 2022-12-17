{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}


module Advent16(main) where

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
import Graph
import Data.Function
import System.Random
import qualified Control.Monad.State as St
import Control.Monad
import qualified Data.Set as Set

newtype ValveLabel = ValveLabel String deriving (Show, Eq, Ord)

data Valve = Valve {
   name :: ValveLabel,
   rate :: Int
} deriving (Show)

instance Eq Valve where
  (==) a b = a.name == b.name

instance Ord Valve where
  compare a b = compare (a.name) (b.name)

data VolcanoMap = VolcanoMap (Map.Map Valve [Valve]) deriving (Show, Eq, Ord)

data ValveStates = ValveStates (Map.Map Valve Int) [Bool] deriving (Show)

openV :: Valve -> ValveStates -> ValveStates
openV v (ValveStates states open) = (ValveStates states result) where
  index = states ! v
  result = setAt index True open

isValveOpen :: Valve -> ValveStates -> Bool
isValveOpen v (ValveStates states open) = open !! (states ! v)

toValveState :: Map.Map Valve Bool -> ValveStates
toValveState m = result where
  (valves, states) = unzip $ Map.toList m
  result = ValveStates (Map.fromList $ zip valves [0..]) states


instance Eq ValveStates where
  (==) (ValveStates _ a) (ValveStates _ b) = a == b

instance Ord ValveStates where
  compare (ValveStates _ a) (ValveStates _ b) = compare a b

data VolcanoState = VolcanoState {
   map :: VolcanoMap,
   valveStates :: ValveStates,
   currentValve :: Valve,
   releasedPressure :: Int,
   time :: Int,
   op :: Operation,
   flowRate :: Int
}

instance Show VolcanoState where
  show (VolcanoState _ _ currentValve releasedPressure time op flowRate) = "VolcanoState { currentValve = " ++ show currentValve ++ ", releasedPressure = " ++ show releasedPressure ++ ", time = " ++ show time ++ ", flowRate = " ++ show flowRate ++ "}"

data VolcanoStateCacheKey = VolcanoStateCacheKey {
  vstate :: [Bool],
--  time :: Int,
  currentValve :: Valve
} deriving (Show, Eq, Ord)

toCacheKey :: VolcanoState -> VolcanoStateCacheKey
toCacheKey s = VolcanoStateCacheKey vstate s.currentValve where
  (ValveStates _ vstate) = s.valveStates

instance Eq VolcanoState where
  (==) a b = a.currentValve == b.currentValve && a.time == b.time && a.valveStates == b.valveStates

instance Ord VolcanoState where
  compare a b = compare ( a.currentValve, a.time, a.valveStates) (b.currentValve, b.time, b.valveStates)

--instance Ord VolcanoState where
--  compare a b = compare a.currentValve b.currentValve

data VolcanoEntry = VolcanoEntry {
   valve :: Valve,
   neighbors :: [ValveLabel]
} deriving (Show)

parseVolcanoEntry :: Parser VolcanoEntry
parseVolcanoEntry = do
  string "Valve "
  name <- ValveLabel <$> many1 letter
  string " has flow rate="
  rate <- parseNumber
  string "; tunnel"
  optional $ string "s"
  string " lead"
  optional $ string "s"
  string " to valve"
  optional $ string "s"
  string " "
  neighbors <- (ValveLabel <$> many1 letter) `sepBy` (string ", ")
  return $ VolcanoEntry (Valve name rate) neighbors

parseVolcanoEntries :: Parser [VolcanoEntry]
parseVolcanoEntries = parseVolcanoEntry `sepBy` newline

compareRate :: Valve -> Valve -> Ordering
compareRate a b = compare (a.rate) (b.rate)

vlook :: VolcanoMap -> Valve -> [Valve]
vlook (VolcanoMap vmap) v = vmap Map.! v

asMap :: VolcanoMap -> Map.Map Valve [Valve]
asMap (VolcanoMap vmap) = vmap

fDiv :: Int -> Int -> Float
fDiv a b = fromIntegral a / fromIntegral b

fTimes :: Int -> Float -> Float
fTimes a b = fromIntegral a * b

ffTimes :: Float -> Int -> Float
ffTimes a b = a * fromIntegral b

maxOrZero :: Ord a => a -> [a] -> a
maxOrZero a [] = a
maxOrZero _ (xs) = maximum xs

tr :: Show a => String -> a -> a
tr s a = a -- trace (s ++ show a) a


asciiInt :: Valve -> Int
asciiInt (Valve (ValveLabel name) _) = read $ intercalate "" $ fmap show $ fmap fromEnum name

toHeadRate :: (Int, [[Valve]]) -> (Int, Int)
toHeadRate k@(l, ps) = (l, maximum $ fmap (.rate) $ tr ("Heads: " ++ show l) $ fmap head (tr ("aaa " ++ show k) ps))


groupOnKey :: Ord k => (a -> k) -> [a] -> Map.Map k [a]
groupOnKey f = Map.fromListWith (++) . fmap (\a -> (f a, [a]))

pathToNextBestDestination :: Float -> VolcanoState -> [Valve]
pathToNextBestDestination discount state = result where
   neighbors = state.map
   remainingTime = 30 - state.time
   parentPathMap = bfsFull (vlook neighbors) (state.currentValve)
   valves = filter (\v -> not $ isValveOpen v state.valveStates) $ Map.keys (asMap neighbors)
   paths = fmap (\p -> (p ++ [state.currentValve])) $ fmap (computePath parentPathMap) valves
   depthPenalty = tr " " $ Map.fromList $ fmap toHeadRate $ tr "grouped: " $ Map.toList $ groupOnKey (length) paths
   depthRank = fmap (\p -> (p, rankPath discount remainingTime depthPenalty p)) $ paths
   orderedValves = reverse $ sortOn (snd) $ depthRank
   result = fst $ head orderedValves

topNNextMoves :: Int -> VolcanoState -> [[Valve]]
topNNextMoves n state = result where
   neighbors = state.map
   remainingTime = 30 - state.time
   parentPathMap = bfsFull (vlook neighbors) (state.currentValve)
   valves = filter (\v -> not $ isValveOpen v state.valveStates) $ Map.keys (asMap neighbors)
   paths = fmap (\p -> (p ++ [state.currentValve])) $ fmap (computePath parentPathMap) valves
   depthPenalty = tr " " $ Map.fromList $ fmap toHeadRate $ tr "grouped: " $ Map.toList $ groupOnKey (length) paths
   depthRank = fmap (\p -> (p, rankPath 1.0 remainingTime depthPenalty p)) $ paths
   orderedValves = reverse $ sortOn (snd) $ depthRank
   result = fmap fst $ take n orderedValves

everyPath :: Int -> VolcanoState -> [[Valve]]
everyPath seed state = paths where
   neighbors = state.map
   remainingTime = 30 - state.time
   parentPathMap = bfsFull (vlook neighbors) (state.currentValve)
   valves = filter (\v -> v.rate > 0) $ filter (\v -> not $ isValveOpen v state.valveStates) $ Map.keys (asMap neighbors)
   paths = fmap (\p -> (p ++ [state.currentValve])) $ fmap (computePath parentPathMap) valves

incrementPressure :: VolcanoState -> VolcanoState
incrementPressure state = result where
  newPressure = state.releasedPressure + state.flowRate
  result = state { releasedPressure = newPressure, time = state.time + 1 }


openValve :: Valve -> VolcanoState -> VolcanoState
openValve v state = state { valveStates = openV v (state.valveStates), flowRate = state.flowRate + v.rate }

secondToLast :: [a] -> a
secondToLast = last . init


data Operation = Open Valve | Goto Valve deriving (Show, Eq, Ord)

iterateVolcanoStateOnOperation :: VolcanoState -> Operation -> VolcanoState
iterateVolcanoStateOnOperation state (Open v) = result where
  result = openValve v $ incrementPressure state
iterateVolcanoStateOnOperation state (Goto v) = result where
  result = incrementPressure $ state { currentValve = v }


iterateVolcanoState :: Float -> VolcanoState -> (VolcanoState, Operation)
iterateVolcanoState discount state = result where
  pathToTake = pathToNextBestDestination discount state
  result = if (head pathToTake == state.currentValve) then
      ( openValve (state.currentValve) $ incrementPressure state, Open (state.currentValve))
    else
      (incrementPressure $ state { currentValve = secondToLast pathToTake }, Goto (secondToLast pathToTake))


--bestOperationsFix :: (VolcanoState -> [(Operation, VolcanoState)]) -> VolcanoState -> [(Operation, VolcanoState)]
--bestOperationsFix f state = bestResult where
--  possibleNextStates = getPossibleNextStates state
--  results = fmap (\op -> op : f (iterateVolcanoStateOnOperation state op)) possibleNextStates
--  bestResult = maximumBy (compare `on` (sum . fmap (releasedPressure . snd))) results

bestOperationsState :: VolcanoState -> St.State (Map.Map VolcanoStateCacheKey [(Operation, VolcanoState)]) [(Operation, VolcanoState)]
bestOperationsState (VolcanoState _ _ _ _ 10 _ _) = return $ (tr "bottom: " [])
bestOperationsState state = do
  resultMap <- St.get
  let possibleNextOperations = if ((tr "tick: " $ state.time) >= 30) then [] else getPossibleNextOperations state
  let remainder op = if Map.member (toCacheKey (iterateVolcanoStateOnOperation state op)) resultMap then
        return $ (op, state):(resultMap Map.! (toCacheKey (iterateVolcanoStateOnOperation state op)))
      else
        fmap (\t -> (op, state):t) $ bestOperationsState (tr "itr: " $ (iterateVolcanoStateOnOperation state op))
  results <- sequence $ fmap remainder possibleNextOperations
  let bestResult = if null results then [] else maximumBy (compare `on` (\r -> ((.releasedPressure) . snd . last) r)) results
  St.put $ Map.insert (toCacheKey state) bestResult resultMap
  return $ tr "br: " bestResult

bestOperationsStateSingle :: VolcanoState -> St.State (Map.Map VolcanoState VolcanoState) (Maybe VolcanoState)
bestOperationsStateSingle state = do
  resultMap <- St.get
  let possibleNextOperations = if ((tr "tick :" $ state.time) >= 30) then [] else getPossibleNextOperations (tr "state: " state)
  let remainder op = if Map.member (iterateVolcanoStateOnOperation state op) resultMap then
        return $ Just (resultMap Map.! (iterateVolcanoStateOnOperation state op))
      else
        bestOperationsStateSingle (iterateVolcanoStateOnOperation state op)
  results <- sequence $ fmap remainder possibleNextOperations
  let realResults = filter isJust results
  let bestResult = join $ if null realResults then Nothing else Just (maximumBy (compare `on` (fmap (.releasedPressure))) $ realResults)
  St.put $ if isJust bestResult then Map.insert state (fromJust bestResult) resultMap else resultMap
  return $ bestResult

--memoize :: (Ord a) => [a] -> (a -> b) -> (a -> b)
--memoize domain f = (map Map.! ) . memo where
--  map = Map.fromList $ fmap (\a -> (a, f a)) domain
--
--bestOperations :: VolcanoState -> [(Operation, VolcanoState)]
--bestOperations state = fix ((memoize domain) . bestOperationsFix) $ state where
--  domain = Map.keys (asMap $ state.map)

getPossibleNextOperations :: VolcanoState -> [Operation]
getPossibleNextOperations state = possibleNextStates where
  currentValveIsOpen = isValveOpen (state.currentValve) state.valveStates
  candidatePaths = topNNextMoves 1 state
  currentValveNeighbors = vlook (state.map) (state.currentValve)
  neighborStates = fmap Goto currentValveNeighbors
  possibleNextStates = neighborStates ++ (if currentValveIsOpen then [] else [Open (state.currentValve)])

gotoValve :: Valve -> VolcanoState -> VolcanoState
gotoValve v state =  incrementPressure $ state { currentValve = v, op = Goto v }

openCurrentValve :: VolcanoState -> VolcanoState
openCurrentValve state = openValve (state.currentValve) $ incrementPressure (state {op = Open (state.currentValve)})

getPossibleNextStates :: VolcanoState -> [VolcanoState]
getPossibleNextStates state = results where
  currentValveIsOpen = isValveOpen  (state.currentValve) state.valveStates
  candidatePaths = topNNextMoves 1 state
  currentValveNeighbors = nub $ fmap secondToLast $ filter (\p -> length p > 1) candidatePaths
  neighborStates = fmap (\v -> gotoValve v state) currentValveNeighbors
  possibleNextStates = if currentValveIsOpen then neighborStates else openCurrentValve state : neighborStates
  results = filter (\s -> s.time <= 30) possibleNextStates

getArtifactsUntil :: (a -> (a,k)) -> (a -> Bool) -> a -> [(a,k)]
getArtifactsUntil f p a = result where
  (a', k) = f a
  result = if not (p a') then (a', k) : getArtifactsUntil f p a' else []


rankPath :: Float -> Int -> Map.Map Int Int -> [Valve] -> Int
rankPath discount remainingTime distCosts p =  floor rank where
  dest = head  p
--  penalty = maxOrZero 0 $ (fmap (\k -> (remainingTime - (k+1)) *  Map.findWithDefault 0 k distCosts) [1..length p - 1])
  rank =  (dest.rate * (remainingTime - length p)) `fDiv` (length p)


reduceGraphFW :: Valve -> Map.Map Valve [Valve] -> Map.Map Valve [(Valve, Int)]
reduceGraphFW start g = distCosts where
  targetNodes = filter (\v -> v.rate > 0 || v == start) $ Map.keys g
  fw = floydWarshall g
  pairWise = [(i,j) | i <- targetNodes, j <- targetNodes, i /= j]
  distPairs = fmap (computePathFW fw) pairWise
  getCosts = (\source -> (source, fmap (\sink -> (sink, length ((computePathFW fw) (source, sink)))) targetNodes))
  distCosts = Map.fromList $ fmap (\(s,ps) -> (s, filter (\(p,c) -> p /= s) ps)) $ fmap getCosts targetNodes

reduceGraphBFS :: Valve -> Map.Map Valve [Valve] -> Map.Map Valve [(Valve, Int)]
reduceGraphBFS start g = distCosts where
  targetNodes = filter (\v -> v.rate > 0 || v == start) $ Map.keys g
  getBFS node = bfsFull (\n -> g ! n) node
  getCosts = (\source -> (source, fmap (\sink -> (sink, length (computePath (getBFS source) sink))) targetNodes))
  distCosts = Map.fromList $ fmap (\(s,ps) -> (s, filter (\(p,c) -> p /= s) ps)) $ fmap getCosts targetNodes

rankDest :: Int -> (Valve,Int) -> Int
rankDest time (valve, cost) = valve.rate * (time - (cost * 3))

possiblePaths :: Map.Map Valve [(Valve, Int)] -> Int -> Valve -> Set.Set Valve -> [(Valve,Int)] -> [[(Valve,Int)]]
possiblePaths g time source pset path = result where
  sortedNeighbors = reverse $ sortOn (rankDest time) $ filter (\(n,_) -> not (n `Set.member` pset)) $ g Map.! source
  neighbors = filter (\(_,c) -> time - c >= 0) sortedNeighbors
  result =  if time <= 0 then [tail path] else
            if null neighbors then [path] else
            concat $ fmap (\(v, d) -> possiblePaths g (time - (d+1)) v (Set.insert v pset) ((v,d):path)) neighbors


runNTimes :: Int -> (a -> a) -> a -> a
runNTimes n f a = if n <= 0 then a else runNTimes (n-1) f (f a)

evalPath :: Int -> VolcanoState -> [(Valve,Int)] -> VolcanoState
evalPath t state [] = until (\s -> s.time >= t) incrementPressure state
evalPath t state (p:ps) = evalPath t ( evalPathStep state p) ps where

evalPathStep :: VolcanoState -> (Valve,Int) -> VolcanoState
evalPathStep state p = next' where
  (valve, cost) = p
  next = openValve valve $ runNTimes (cost+1) incrementPressure state
  next' = next {currentValve = valve} :: VolcanoState


main :: IO ()
main = do
  s <- resource "volcano-valve"
  let entries = runParser parseVolcanoEntries () "input" s
  let stuff = case entries of
          Left err -> error $ show err
          Right stuff -> stuff
--  print entries
  let vmap = Map.fromList $ fmap (\e -> (e.valve.name, e.valve)) stuff
  let neighbors = VolcanoMap $ Map.fromList $ fmap (\e -> (e.valve, fmap ((Map.!) vmap ) e.neighbors)) stuff
  let initialValveStates = Map.fromList $ fmap (\e -> (e.valve, False)) stuff
  let valves = Map.keys (asMap neighbors)
  let flowValves = filter (\v -> v.rate > 0) valves
  let start = (Valve (ValveLabel "AA") 0)
--  print neighbors
--  let fw = floydWarshall $ asMap neighbors
--  print fw
--  print $ computePathFW fw ((Valve (ValveLabel "AA") 0), (Valve (ValveLabel "GG") 0))
--
  let volcanoState = VolcanoState neighbors (toValveState initialValveStates) (Valve (ValveLabel "AA") 0) 0 0 (Goto  (Valve (ValveLabel "AA") 0)) 0
--  let results d = getArtifactsUntil (iterateVolcanoState d) (\s -> s.time > 30) volcanoState
--  let bestResult = head $ reverse $ sortOn ((.releasedPressure) . fst . last) $ fmap results [1.0]
--  mapM_ print $ fmap (\s -> (snd s, (fst s).time, (fst s).releasedPressure)) $ bestResult

--  let result = St.runState (bestOperationsState volcanoState) Map.empty
--  mapM_ print $ fmap (\(o,s) -> (o, s.releasedPressure, s.time)) $ fst $ result
--  print $ length $ fst result

  print "Starting..."
  let g = (reduceGraphBFS start $ asMap neighbors)
  let pp = fmap reverse $ possiblePaths g 30 start (Set.singleton start) []
  let bestpp = take 1000000 pp
  let ranked = fmap (\p -> (p, evalPath 30 volcanoState p)) bestpp
  let best = head $ reverse $ sortOn ((.releasedPressure) . snd) ranked


  mapM_ print $ scanl evalPathStep volcanoState (fst best)

  mapM_ print $ fst best
  print $ (snd best).releasedPressure
  print $ (snd best).time
  
  let openValves = filter (\v -> (snd v).open) $ Map.toList $ (snd best).valveState
  let epp = fmap reverse $ possiblePaths g 30 start (Set.singleton start) []


--  let knownBestPath =
--        [(Valve (ValveLabel "DD") 20,1),
--        (Valve (ValveLabel "BB") 13,2),
--        (Valve (ValveLabel "JJ") 21,3),
--        (Valve (ValveLabel "HH") 22,7),
--        (Valve (ValveLabel "EE") 3,3),
--        (Valve (ValveLabel "CC") 2,2)]
--
--  let test = evalPath volcanoState knownBestPath
--  print $ (test.time, test.releasedPressure)




--  let result = St.runState (bestOperationsStateSingle volcanoState) Map.empty
--  print $ fmap (.releasedPressure) (fst result)

--  print $ length $ results
--  print $ head $ reverse $ sortBy (compare `on` (.releasedPressure)) $ Map.keys $ bfsFull getPossibleNextStates volcanoState

-- example 2 should be 2640
-- example 3 should be 13468

-- 1742 is too low

 -- 1768 was wrong

 -- 2461 was wrong

