module Search where


import Util
import Control.Monad.State
import Data.Functor.Identity
import Data.Hashable
import qualified Data.HashMap.Strict as HMap
import Data.List
import Safe
import Data.Maybe
import Debug.Trace

data SearchState a = SearchState {
  cache :: HMap.HashMap a (Maybe a),
  best :: Maybe a
} deriving (Show)

compareMay :: (a -> a -> Ordering) -> Maybe a -> Maybe a -> Ordering
compareMay comp Nothing Nothing = EQ
compareMay comp Nothing (Just _) = LT
compareMay comp (Just _) Nothing = GT
compareMay comp (Just x) (Just y) = comp x y

solutionSearchInternal :: (Hashable a, Show a) => (a -> a -> Ordering) -> (Maybe a -> a -> [a]) -> (a -> Bool) -> a -> State (SearchState a) (Maybe a)
solutionSearchInternal comp next isEnd state = do
  if isEnd state then do
    (SearchState cache best) <- get
    let leaf = (Just state)
    let newBest = case (best, leaf) of
                  (Nothing, _) -> leaf
                  (_, Nothing) -> best
                  (Just b, Just bsr) -> if comp bsr b == GT then trace ("New Best: " ++ show leaf) leaf else best
    modify (\(SearchState c b) -> SearchState c newBest)
    return leaf
  else do
    (SearchState cache best) <- get
    case HMap.lookup state cache of
      Just cachedState -> return (cachedState)
      Nothing -> do
        (SearchState cache best) <- get
        let nextStates = next best state
        nextResults <- mapM (solutionSearchInternal comp next isEnd) nextStates
        let bestSubResult = maximumByDef Nothing (compareMay comp) (nextResults)
        modify (\(SearchState c b) -> (SearchState (HMap.insert state bestSubResult c) b))
        return $ bestSubResult

solutionSearch :: (Hashable a, Show a) => (a -> a -> Ordering) -> (Maybe a -> a -> [a]) -> (a -> Bool) -> a -> Maybe a
solutionSearch compare next isEnd state = evalState (solutionSearchInternal compare next isEnd state) (SearchState HMap.empty Nothing)
