{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
module Memo where

import Control.Monad.State
import Data.Functor.Identity
import Data.Hashable
import qualified Data.HashMap.Strict as HMap
import Debug.Trace
import Data.Maybe
import Util

type Memo a x = State (HMap.HashMap a x) x

memoize :: (Hashable a) => ((a -> Memo a x) -> a -> Memo a x) -> a -> x
memoize fixFn val = fst $ (runState (go val) HMap.empty) where
    memo a = do
        memo <- get
        case HMap.lookup a memo of
            Just x -> return x
            Nothing -> do
                x <- go a
                modify (HMap.insert a x)
                return x
    go = fixFn memo

data MemoStats = MemoStats {
    numCalls :: Int,
    numCacheHits :: Int,
    numCuts :: Int
} deriving (Show)

type MemoBB a x = State (HMap.HashMap a (Maybe x), Maybe x, MemoStats) (Maybe x)

memoizeBB :: (Hashable a, Show a, Show x) => (x -> x -> Ordering) -> (a -> x -> Bool) -> ((a -> MemoBB a x) -> a -> MemoBB a x) -> a -> Maybe x
memoizeBB comp shouldCut fixFn val = traceShow (trd $ snd result) (fst result) where
    result = runState (go val) (HMap.empty, Nothing, MemoStats 0 0 0)
    memo a = do
        (memMap, best, stats) <- get
        if isJust best && shouldCut a (fromJust best) then do
--          return (trace ("branch cut due to comparing\n" ++ show a ++ "\nto\n" ++ show (fromJust best))  Nothing)
          modify $ (\(m,b,s) -> (m ,b , s {numCuts = s.numCuts + 1}))
          return  Nothing
        else
          case join $ HMap.lookup a memMap of
              Just x -> do
                  modify (\(m,b,s) -> (m ,b , s {numCacheHits = s.numCacheHits + 1}))
                  return (Just x)
              Nothing -> do
                  x <- go a
                  modify $ (\(m,b,s) -> (m ,b , s {numCalls = s.numCalls + 1}))
                  let newBest = case (best, x) of
                        (Nothing, _) -> x
                        (_, Nothing) -> best
                        (Just best, Just x) -> if comp x best == GT then Just x else Just best
                  modify $ (\(m,b,s) -> (HMap.insert a x m, newBest, s))
                  return x
    go = fixFn memo