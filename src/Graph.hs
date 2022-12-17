{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Graph where

import Control.Monad.State
import Debug.Trace
import qualified Data.Map as Map

data BFSState a = BFSState {
    queue :: [a],
    parents :: Map.Map a a
} deriving (Show)

pop :: [a] -> (a, [a])
pop [] = error "Empty list"
pop (x:xs) = (x, xs)


bfs :: (Show state, Ord state) => (state -> [state]) -> (state -> Bool) -> state -> Maybe [state]
bfs next isGoal start = result where
    initialState = BFSState {
        queue = [start],
        parents = Map.singleton start start
    }
    result = fst $ runState (bfsS next isGoal) initialState

bfsFull :: (Show state, Ord state) => (state -> [state]) -> state -> Map.Map state state
bfsFull next start = result where
    initialState = BFSState {
        queue = [start],
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
    let (current, queue') = pop queue
    let candidates = mkCandidates (current)
    let validCandidates = filter (`Map.notMember` parents) candidates
    let newQueue = queue' ++ validCandidates
    let newParents = foldl (\parents candidate -> Map.insert candidate current parents) parents validCandidates
    put $ BFSState newQueue newParents
    if isEnd current
        then return $ Just (computePath parents current)
        else if (null newQueue)
            then return Nothing
            else bfsS mkCandidates isEnd

bfsSFull :: (Show state, Ord state) => (state -> [state]) -> (state -> Bool) -> State (BFSState state) (Map.Map state state)
bfsSFull mkCandidates isEnd = do
    BFSState queue parents <- get
    let (current, queue') = pop queue
    let candidates = mkCandidates (current)
    let validCandidates = filter (`Map.notMember` parents) candidates
    let newQueue = queue' ++ validCandidates
    let newParents = foldl (\parents candidate -> Map.insert candidate current parents) parents validCandidates
    put $ BFSState newQueue newParents
    if isEnd current
        then return $ parents
        else if (null newQueue)
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
