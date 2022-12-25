{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE ApplicativeDo #-}


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
import Control.Parallel.Strategies
import System.CPUTime
import Control.Monad.IO.Class (MonadIO(liftIO))
import GHC.Generics
import Control.DeepSeq
import Data.Hashable
import qualified Data.HashMap.Strict as HMap
import Memo

newtype ValveLabel = ValveLabel String deriving (Show, Eq, Ord,Generic, NFData)

data Valve = Valve {
   name :: ValveLabel,
   rate :: Int
} deriving (Show, Generic)

instance NFData Valve

instance Eq Valve where
  (==) a b = a.name == b.name

instance Ord Valve where
  compare a b = compare (a.name) (b.name)

data VolcanoMap = VolcanoMap (Map.Map Valve [Valve]) deriving (Show, Eq, Ord, Generic, NFData)

data ValveStates = ValveStates (Map.Map Valve Int) [Bool] deriving (Show, Generic, NFData)

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
   eleValve :: Valve,
   releasedPressure :: Int,
   time :: Int,
   op :: Operation,
   flowRate :: Int
} deriving (Generic)

instance NFData VolcanoState

instance Show VolcanoState where
  show (VolcanoState _ _ currentValve _ releasedPressure time op flowRate) = "VolcanoState { currentValve = " ++ show currentValve ++ ", releasedPressure = " ++ show releasedPressure ++ ", time = " ++ show time ++ ", flowRate = " ++ show flowRate ++ "}"

data VolcanoStateCacheKey = VolcanoStateCacheKey {
  vstate :: [Bool],
  time :: Int,
  currentValve :: Valve,
  eleValve :: Valve
} deriving (Show, Eq, Ord)

toCacheKey :: VolcanoState -> VolcanoStateCacheKey
toCacheKey s = VolcanoStateCacheKey vstate (s.time) s.currentValve s.eleValve where
  (ValveStates _ vstate) = s.valveStates

instance Hashable VolcanoStateCacheKey where
  hashWithSalt salt (VolcanoStateCacheKey vstate time currentValve eleValve) = hashWithSalt salt (vstate, time, currentValve, eleValve)

instance Hashable VolcanoState where
  hashWithSalt salt s = hashWithSalt salt (toCacheKey s)

instance Hashable Valve where
  hashWithSalt salt (Valve (ValveLabel s) _) = hashWithSalt salt s

instance Eq VolcanoState where
  (==) a b = a.releasedPressure == b.releasedPressure

instance Ord VolcanoState where
  compare a b = compare ( a.releasedPressure) (b.releasedPressure)

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
tr s a = trace (s ++ show a) a


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
openValve v state = if (isValveOpen v (state.valveStates)) then state else state { valveStates = openV v (state.valveStates), flowRate = state.flowRate + v.rate }

secondToLast :: [a] -> a
secondToLast = last . init


data Operation = Open Valve | Goto Valve deriving (Show, Eq, Ord,Generic, NFData)

instance Hashable Operation

iterateVolcanoStateOnOperation :: VolcanoState -> Operation -> VolcanoState
iterateVolcanoStateOnOperation state (Open v) = result where
  result = openValve v $ incrementPressure state
iterateVolcanoStateOnOperation state (Goto v) = result where
  result = incrementPressure $ state { currentValve = v }

applyOp :: VolcanoState -> Bool -> Operation -> VolcanoState
applyOp state isEle (Open v) = result where
  result = openValve v $ state
applyOp state isEle (Goto v) = result where
  result = if isEle then (state { eleValve = v } :: VolcanoState) else (state { currentValve = v } :: VolcanoState)

applyBothOps :: VolcanoState -> (Operation, Operation) -> VolcanoState
applyBothOps state (hop, eop) = result where
  postOp = applyOp (applyOp state False hop) True eop
  result = incrementPressure postOp

getPossibleNextOperations :: VolcanoState -> Valve -> [Operation]
getPossibleNextOperations state v = possibleNextStates where
  currentValveIsOpen = isValveOpen v state.valveStates || v.rate == 0
  currentValveNeighbors = vlook (state.map) v
  neighborStates = fmap Goto currentValveNeighbors
  possibleNextStates = neighborStates ++ (if currentValveIsOpen then [] else [Open v])

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
rankDest time (valve, cost) = (valve.rate * (time - (cost + 1)*2))

possiblePaths :: Map.Map Valve [(Valve, Int)] -> Int -> Valve -> Set.Set Valve -> [(Valve,Int)] -> [[(Valve,Int)]]
possiblePaths g time source pset path = result where
  sortedNeighbors = reverse $ sortOn (rankDest time) $ filter (\(n,_) -> not (n `Set.member` pset)) $ g Map.! source
  neighbors = filter (\(_,c) -> time - c >= 0) sortedNeighbors
  result =  if time <= 0 then [tail path] else
            if null neighbors then [path] else
            concat $ fmap (\(v, d) -> possiblePaths g (time - (d+1)) v (Set.insert v pset) ((v,d):path)) neighbors


evalPath :: Int -> VolcanoState -> [(Valve,Int)] -> VolcanoState
evalPath t state [] = until (\s -> s.time >= t) incrementPressure state
evalPath t state (p:ps) = evalPath t ( evalPathStep state p) ps where

evalPathStep :: VolcanoState -> (Valve,Int) -> VolcanoState
evalPathStep state p = next' where
  (valve, cost) = p
  next = openValve valve $ runNTimes (cost+1) incrementPressure state
  next' = next {currentValve = valve} :: VolcanoState


rankedEvaledPaths :: Map.Map Valve [(Valve, Int)] -> Int -> VolcanoState -> Valve -> [Valve] -> [([(Valve,Int)], VolcanoState)]
rankedEvaledPaths g time volcanoState start offlimitNodes = parallelNodeEval $ fmap reverse allPaths  where
  nodeEval = (\p -> (p, evalPath time volcanoState p))
  parallelNodeEval = fmap nodeEval
  ns = Set.fromList offlimitNodes
  allPaths = possiblePaths g time start ns []

pressureSum :: (([(Valve,Int)], VolcanoState),([(Valve,Int)], VolcanoState)) -> Int
pressureSum ((p1, s1),(p2, s2)) = s1.releasedPressure + s2.releasedPressure


boolSequences' :: Int -> [[Bool]]
boolSequences' 0 = [[]]
boolSequences' n = fmap (True:) (boolSequences' (n-1)) ++ fmap (False:) (boolSequences' (n-1))

boolSequences :: Int -> Int -> [[Bool]]
boolSequences maxDiff n = filter (\b -> partSizeDiff b <= maxDiff) $ sortOn partSizeDiff $ boolSequences' n

partSizeDiff :: [Bool] -> Int
partSizeDiff bs = result where
  (t,f) = partition id bs
  result = abs $ length t - length f


doPartition :: [Bool] -> [a] -> ([a],[a])
doPartition bools vals = result where
  z = zip bools vals
  parted = partition fst z
  result = (fmap snd $ fst parted, fmap snd $ snd parted)


allPossiblePartitions :: Int -> [Valve] -> [([Valve],[Valve])]
allPossiblePartitions maxDiff a = result where
  n = length a
  rankedAs = sortOn (\v -> v.rate) a
  bools = boolSequences maxDiff n
  partitionFunctions = fmap doPartition bools
  result = fmap (\f -> f a) partitionFunctions

rp :: ([(Valve,Int)], VolcanoState) -> Int
rp (p, s) = s.releasedPressure

pairedResults :: Int -> Map.Map Valve [(Valve, Int)] -> [([Valve],[Valve])] -> VolcanoState -> Valve -> [(([(Valve,Int)], VolcanoState), ([(Valve,Int)], VolcanoState))]
pairedResults sample g nodePartitions volcanoState start = do
  (humanSet, eleSet) <- nodePartitions
  let getBest k = head $ reverse $ sortOn rp $ take sample k
  let humanPaths = rankedEvaledPaths g 26 volcanoState start (start:humanSet) :: [([(Valve,Int)], VolcanoState)]
  let elePaths = rankedEvaledPaths g 26 volcanoState start (start:eleSet) :: [([(Valve,Int)], VolcanoState)]
  let humanPath = getBest $ humanPaths
  let elePath = getBest $ elePaths
  return (humanPath, elePath)

tv :: String -> Valve
tv s = Valve {name = ValveLabel s, rate = 0}


rankFn :: VolcanoState -> ([Operation], VolcanoState) -> Int
rankFn state (ops, s) = s.releasedPressure - state.releasedPressure


findBestRoute :: (Applicative f) => VolcanoState -> Int -> (VolcanoState -> f ([Operation], VolcanoState)) -> VolcanoState -> f ([Operation], VolcanoState)
findBestRoute initialState timeLimit recur state = result where
  nextOps = getPossibleNextOperations state (state.currentValve)
  nextStates = fmap (\op -> (op, iterateVolcanoStateOnOperation state op)) nextOps
  subRoute (op,s) = (\r -> (op : fst r, snd r)) <$> recur s
  possibleRoutes = traverse subRoute nextStates
  makeTrue :: [([Operation], VolcanoState)] -> [([Operation], VolcanoState)]
  makeTrue l = fmap (\(ops, s) -> (ops, foldl' iterateVolcanoStateOnOperation initialState ops)) l
  trueStates = makeTrue <$> possibleRoutes
  bestRoute = (head . reverse . (sortOn (rankFn state))) <$> trueStates
  result = if state.time >= timeLimit then pure ([], state) else bestRoute

findBestRouteMay :: (Applicative f) => VolcanoState -> Int -> (VolcanoState -> f (Maybe ([Operation], VolcanoState))) -> VolcanoState -> f (Maybe ([Operation], VolcanoState))
findBestRouteMay initialState timeLimit recur state = result where
  nextOps = getPossibleNextOperations state (state.currentValve)
  nextStates = fmap (\op -> (op, iterateVolcanoStateOnOperation state op)) nextOps
  subRoute (op,s) = fmap (\r -> (op : fst r, snd r)) <$> recur s
  possibleRoutes = catMaybes <$> traverse subRoute nextStates
  makeTrue :: [([Operation], VolcanoState)] -> [([Operation], VolcanoState)]
  makeTrue l = fmap (\(ops, s) -> (ops, foldl' iterateVolcanoStateOnOperation initialState ops)) l
  trueStates = makeTrue <$> possibleRoutes
  bestRoute = (headMay . reverse . (sortOn (rankFn state))) <$> trueStates
  result = if state.time >= timeLimit then (pure (Just ([], state))) else bestRoute


rankFnB :: VolcanoState -> ([(Operation,Operation)], VolcanoState) -> Int
rankFnB state (ops, s) = s.releasedPressure - state.releasedPressure

--cutState :: Int -> VolcanoState -> Bool
--cutState timeLimit state = result where
--  overtime = state.time >= timeLimit

foldlUntil :: (Show b) => (a -> b -> a) -> (a -> Bool) -> a -> [b] -> a
foldlUntil f p a [] = a
foldlUntil f p a (b:bs) = result where
  next = f a b
  result = if p a then a else foldlUntil f p next bs

shouldCut :: VolcanoState -> Int -> VolcanoState -> ([(Operation, Operation)], VolcanoState) -> Bool
shouldCut initState timeLimit state (ops, s) = shouldCutResult where
  closedValvesOrderedByHighestRate = sortOn (\v -> -1 * v.rate) $ filter (\v -> v.rate > 0) $ filter (\v -> isValveOpen v state.valveStates == False) $ Map.keys $ asMap state.map
  valvePairs = let parts = ipartition (\i a -> i `mod` 2 == 0) closedValvesOrderedByHighestRate in (zip (fst parts) (snd parts))
  openBestValveEachTurnOpList = concatMap (\(v1, v2) -> [(Goto v1, Goto v2), (Open v1, Open v2)]) valvePairs
  openBestValveEachTurnState = foldlUntil applyBothOps (\s -> s.time >= timeLimit) state openBestValveEachTurnOpList
  remainingTimePassedState = runNTimes (timeLimit - openBestValveEachTurnState.time) incrementPressure openBestValveEachTurnState
  shouldCutResult = s.releasedPressure > remainingTimePassedState.releasedPressure

shouldCut2 :: VolcanoState -> Int -> VolcanoState -> ([(Operation, Operation)], VolcanoState) -> Bool
shouldCut2 initState timeLimit state (ops, s) = shouldCutResult where
  stateAtEquivalentTime = foldlUntil applyBothOps (\s -> s.time >= state.time) initState ops
  shouldCutResult = state.releasedPressure < stateAtEquivalentTime.releasedPressure && state.flowRate < stateAtEquivalentTime.flowRate

--cutBackTrack :: VolcanoState -> Int -> ([(Operation, Operation)], VolcanoState) -> ([(Operation, Operation)], VolcanoState) -> Bool
--cutBackTrack initState timeLimit (curOps, state) (ops, s) = humanBacktracked || eleBackTracked where
--  humanBacktracked = fmap fst curOps
--  eleBackTracked = shouldCut2 initState timeLimit state (ops, s)

cutAll :: VolcanoState -> Int -> ([(Operation, Operation)], VolcanoState) -> ([(Operation, Operation)], VolcanoState) -> Bool
cutAll initState timeLimit (curOps, state) (ops, s) = answer where
  one = shouldCut initState timeLimit state (ops, s)
--  two = shouldCut2 initState timeLimit state (ops, s)
  answer = one -- || two

dontCut :: VolcanoState -> Int -> VolcanoState -> ([(Operation, Operation)], VolcanoState) -> Bool
dontCut initState timeLimit state (ops, s) = False where

shouldCutSingle :: Int -> VolcanoState -> (a, VolcanoState) -> Bool
shouldCutSingle timeLimit state (ops, s) = shouldCut where
  closedValvesOrderedByHighestRate = sortOn (\v -> -1 * v.rate) $ filter (\v -> v.rate > 0) $ filter (\v -> isValveOpen v state.valveStates == False) $ Map.keys $ asMap state.map
  openBestValveEachTurnOpList = concatMap (\v -> [Goto v, Open v]) closedValvesOrderedByHighestRate
  openBestValveEachTurnState = foldl' iterateVolcanoStateOnOperation state openBestValveEachTurnOpList
  remainingTimePassedState = runNTimes (timeLimit - openBestValveEachTurnState.time) incrementPressure openBestValveEachTurnState
  shouldCut = s.releasedPressure > remainingTimePassedState.releasedPressure

isOpenOp :: Operation -> Bool
isOpenOp (Open _) = True
isOpenOp _ = False

findBestRouteEle :: (Applicative f) => VolcanoState -> Int -> (([(Operation, Operation)], VolcanoState) -> f (Maybe ([(Operation, Operation)], VolcanoState))) -> ([(Operation, Operation)], VolcanoState) -> f (Maybe ([(Operation, Operation)], VolcanoState))
findBestRouteEle initialState timeLimit recur (pastOps, state) = result where
  humanOps = getPossibleNextOperations state (state.currentValve)
  eleOpsAll = getPossibleNextOperations state (state.eleValve)
  eleOps = filter (not . (\op -> isOpenOp op && op `elem` humanOps )) eleOpsAll
  allOps = [(h,e) | h <- humanOps, e <- eleOps]
  nextStates = fmap (\ops -> (ops, applyBothOps state ops)) allOps
  subRoute (op,s) = recur ((op:pastOps),s)
  possibleRoutes = catMaybes <$> traverse subRoute nextStates
  makeTrue :: [([(Operation, Operation)], VolcanoState)] -> [([(Operation, Operation)], VolcanoState)]
  makeTrue l = fmap (\(ops, s) -> (ops, foldl' applyBothOps initialState ops)) l
  trueStates = makeTrue <$> possibleRoutes
  bestRoute = (headMay . reverse . (sortOn (rankFnB state))) <$> trueStates
  result = if state.time >= timeLimit then pure $ Just (pastOps, state) else bestRoute

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
--
  let volcanoState = VolcanoState neighbors (toValveState initialValveStates) (Valve (ValveLabel "AA") 0) (Valve (ValveLabel "AA") 0) 0 0 (Goto  (Valve (ValveLabel "AA") 0)) 0

  print "Starting..."
  t1 <- liftIO getCPUTime
  let findBestHuamnRouteMemo = memoize (findBestRoute volcanoState 20)
  let findBestRouteMemo = memoize (findBestRouteEle volcanoState 25)
  let findBestRouteBB = memoizeBB (compare `on` snd) (shouldCutSingle 30) (findBestRouteMay volcanoState 30)
  let findBestRouteBBEle = memoizeBB (compare `on` snd) (cutAll volcanoState 25) (findBestRouteEle volcanoState 25)

  let result = fromJust $ findBestRouteBBEle ([], volcanoState)
  mapM_ print $ fst result
  print $ (snd result).releasedPressure
  t2 <- liftIO getCPUTime
  print $ (fromIntegral (t2 - t1)) / (10 ^ 12)


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
 -- 2122 was too low
 -- 2280 was too low

-- cuts 460748
