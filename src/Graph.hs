module Graph where

import Control.Monad.State
import Debug.Trace
import qualified Data.Map as Map

data (Ord a) => BFSState a = BFSState {
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