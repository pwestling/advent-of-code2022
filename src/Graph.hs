{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Graph where

import Control.Monad.State
import Debug.Trace
import qualified Data.Map as Map
import Data.List

data BFSState a = BFSState {
    queue :: Queue a,
    parents :: Map.Map a a
} deriving (Show)

data Queue a = Queue {
    enqueue :: [a],
    dequeue :: [a]
} deriving (Show)

invert :: Queue a -> Queue a
invert (Queue enq deq) = Queue [] (deq ++ reverse enq)

pop :: Queue a -> (a, Queue a)
pop q = result where
  fullQueue = if length q.dequeue < length q.enqueue then invert q else q
  result = (head fullQueue.dequeue, fullQueue{dequeue = tail fullQueue.dequeue})

push :: a -> Queue a -> Queue a
push x q = q{enqueue = x:q.enqueue}

pushAll :: [a] -> Queue a -> Queue a
pushAll xs q = q{enqueue = xs ++ q.enqueue}

mkQueue :: [a] -> Queue a
mkQueue xs = Queue [] xs

isEmpty :: Queue a -> Bool
isEmpty q = null q.enqueue && null q.dequeue

bfs :: (Show state, Ord state) => (state -> [state]) -> (state -> Bool) -> state -> Maybe [state]
bfs next isGoal start = result where
    initialState = BFSState {
        queue = mkQueue [start],
        parents = Map.singleton start start
    }
    result = fst $ runState (bfsS next isGoal) initialState

bfsFull :: (Show state, Ord state) => (state -> [state]) -> state -> Map.Map state state
bfsFull next start = result where
    initialState = BFSState {
        queue = mkQueue [start],
        parents = Map.singleton start start
    }
    result = fst $ runState (bfsSFull next (const False)) initialState

computePath :: (Ord state) => Map.Map state state -> state -> [state]
computePath parents end = result where
    next = Map.lookup end parents
    result = case next of
        Nothing -> []
        Just parent -> if parent == end then [] else end : computePath parents parent


bfsS :: (Show state, Ord state) => (state -> [state]) -> (state -> Bool) -> State (BFSState state) (Maybe [state])
bfsS mkCandidates isEnd = do
    BFSState queue parents <- get
    let (current, queue') = pop (queue)
    let candidates = mkCandidates (current)
    let validCandidates = filter (`Map.notMember` parents) candidates
    let newQueue = pushAll validCandidates queue'
    let newParents = foldl (\parents candidate -> Map.insert candidate current parents) parents validCandidates
    put $ BFSState newQueue newParents
    if (isEnd current)
        then return $ Just (computePath parents current)
        else if (isEmpty newQueue)
            then return Nothing
            else bfsS mkCandidates isEnd

bfsSFull :: (Show state, Ord state) => (state -> [state]) -> (state -> Bool) -> State (BFSState state) (Map.Map state state)
bfsSFull mkCandidates isEnd = do
    BFSState queue parents <- get
    let (current, queue') = pop queue
    let candidates = mkCandidates (current)
    let validCandidates = filter (`Map.notMember` parents) candidates
    let newQueue = pushAll validCandidates queue'
    let newParents = foldl (\parents candidate -> Map.insert candidate current parents) parents validCandidates
    put $ BFSState newQueue newParents
    if isEnd current
        then return $ parents
        else if (isEmpty newQueue)
            then return $ parents
            else bfsSFull mkCandidates isEnd

--            let dist be a
--
--
--                {\displaystyle |V|\times |V|}
--
--            {\displaystyle |V|\times |V|} array of minimum distances initialized to
--
--
--                {\displaystyle \infty }
--
--            \infty  (infinity)
--            let next be a
--
--
--                {\displaystyle |V|\times |V|}
--
--            {\displaystyle |V|\times |V|} array of vertex indices initialized to null
--
--            procedure FloydWarshallWithPathReconstruction() is
--                for each edge (u, v) do
--                    dist[u][v] ← w(u, v)  // The weight of the edge (u, v)
--                    next[u][v] ← v
--                for each vertex v do
--                    dist[v][v] ← 0
--                    next[v][v] ← v
--                for k from 1 to |V| do // standard Floyd-Warshall implementation
--                    for i from 1 to |V|
--                        for j from 1 to |V|
--                            if dist[i][j] > dist[i][k] + dist[k][j] then
--                                dist[i][j] ← dist[i][k] + dist[k][j]
--                                next[i][j] ← next[i][k]
--
data (Ord a) => FloydWarshallState a = FloydWarshallState {
    distanceMap :: Map.Map (a, a) Int,
    vertexMap :: Map.Map (a,a) a
} deriving (Show)

infix 3 !
(!) :: Ord k => Map.Map k a -> k -> a
(!) = (Map.!)

updateFWState :: (Ord node, Show node) =>  FloydWarshallState node -> (node,node,node) -> FloydWarshallState node
updateFWState s (i,j,k) = result where
  dm = s.distanceMap
  newDists = Map.insert (i,j) ((dm ! (i,k)) + (dm ! (k,j))) dm
  newVMap = Map.insert (i,j) (s.vertexMap ! (i,k)) s.vertexMap
  distIJ = s.distanceMap ! (i,j) :: Int
  distIK = s.distanceMap ! (i,k) :: Int
  distKJ = s.distanceMap ! (k,j) :: Int
  result = if distIJ > distIK + distKJ then (FloydWarshallState newDists newVMap) else s

floydWarshall :: (Ord a, Show a) => Map.Map a [a] -> (FloydWarshallState a)
floydWarshall adjacencyList = result where
    vertices = Map.keys adjacencyList
    iters = [(i,j,k) | k <- vertices, i <- vertices, j <- vertices]
    initialState = FloydWarshallState {
        distanceMap = Map.fromList [((i,j), if i == j then 0 else if j `elem` (adjacencyList ! i) then 1 else 1000000) | i <- vertices, j <- vertices],
        vertexMap = Map.fromList [((i,j), j) | i <- vertices, j <- vertices]
    }
    result = foldl updateFWState initialState iters


computePathFW :: (Ord node) => FloydWarshallState node -> (node, node) -> [node]
computePathFW s (i,j) = result where
    vMap = s.vertexMap
    next = vMap ! (i,j)
    result = if next == j then [j] else j : computePathFW s (i,next)


data BeamState a = BeamState {
    queue :: [a],
    parents :: Map.Map a a
} deriving (Show)

beam :: (Show state, Ord state) => Int -> (state -> state -> Ordering) -> (state -> [state]) -> (state -> Bool) -> state -> Maybe [state]
beam sample comp next isGoal start = result where
    initialState = BeamState {
        queue = [start],
        parents = Map.singleton start start
    }
    result = fst $ runState (beamS sample comp next isGoal) initialState

beamS :: (Show state, Ord state) => Int -> (state -> state -> Ordering) -> (state -> [state]) -> (state -> Bool) -> State (BeamState state) (Maybe [state])
beamS sample comp mkCandidates isEnd = do
    BeamState queue parents <- get
    let (current, queue') = (head queue, tail queue)
    let candidates = mkCandidates (current)
    let validCandidates = filter (`Map.notMember` parents) candidates
    let newQueue = take sample $ sortBy comp $ queue' ++ validCandidates
    let newParents = foldl (\parents candidate -> Map.insert candidate current parents) parents validCandidates
    put $ BeamState newQueue newParents
    if (isEnd current)
        then return $ Just (computePath parents current)
        else if (null newQueue)
            then return Nothing
            else beamS sample comp mkCandidates isEnd