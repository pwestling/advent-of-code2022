{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Advent19(main) where

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
import Data.Hashable
import qualified Data.Set as Set
import Graph
import Data.Function
import Memo

data Resources = Resources {ore::Int, clay::Int, obsidian::Int, geodes::Int} deriving (Show, Ord)

instance Hashable Resources where
    hashWithSalt salt (Resources ore clay obsidian geodes) = hashWithSalt salt (ore, clay, obsidian, geodes)

instance Eq Resources where
    (Resources ore1 clay1 obsidian1 geodes1) == (Resources ore2 clay2 obsidian2 geodes2) = ore1 == ore2 && clay1 == clay2 && obsidian1 == obsidian2 && geodes1 == geodes2

data Blueprint = Blueprint {
    id :: Int,
    oreBotCost :: Resources,
    clayBotCost :: Resources,
    obsidianBotCost :: Resources,
    geodeBotCost :: Resources
} deriving (Show, Eq, Ord)

data BotState = BotState {
    numOreBots :: Int,
    numClayBots :: Int,
    numObsidianBots :: Int,
    numGeodeBots :: Int,
    blueprint :: Blueprint,
    resources :: Resources,
    time :: Int
} deriving (Eq)


instance Show BotState where
    show (BotState oreBots clayBots obsidianBots geodeBots blueprint resources time) = "BotState {oreBots=" ++ show oreBots ++ ", clayBots=" ++ show clayBots ++ ", obsidianBots=" ++ show obsidianBots ++ ", geodeBots=" ++ show geodeBots ++ ", resources=" ++ show resources ++ ", time=" ++ show time ++ "}"

instance Hashable BotState where
    hashWithSalt salt (BotState ore clay obsidian geode _ resources time) = hashWithSalt salt (ore, clay, obsidian, geode, resources, time)

exampleProblemBP1 = Blueprint {
    id = 1,
    oreBotCost = Resources 4 0 0 0,
    clayBotCost = Resources 2 0 0 0,
    obsidianBotCost = Resources 3 14 0 0,
    geodeBotCost = Resources 2 0 7 0
}

data Operation = OreBot | ClayBot | ObsidianBot | GeodeBot deriving (Show, Eq, Ord)

hasEnoughFor :: Resources -> Resources -> Bool
hasEnoughFor (Resources ore clay obsidian geodes) (Resources ore' clay' obsidian' geodes') = ore >= ore' && clay >= clay' && obsidian >= obsidian' && geodes >= geodes'

spend :: Resources -> Resources -> Resources
spend (Resources ore clay obsidian geodes) (Resources ore' clay' obsidian' geodes') = Resources (ore - ore') (clay - clay') (obsidian - obsidian') (geodes - geodes')

applyBuildOperation :: BotState -> Operation -> BotState
applyBuildOperation state (OreBot) = state { numOreBots = state.numOreBots + 1, resources = state.resources `spend` state.blueprint.oreBotCost }
applyBuildOperation state (ClayBot) = state { numClayBots = state.numClayBots + 1, resources = state.resources `spend` state.blueprint.clayBotCost }
applyBuildOperation state (ObsidianBot) = state { numObsidianBots = state.numObsidianBots + 1, resources = state.resources `spend` state.blueprint.obsidianBotCost }
applyBuildOperation state (GeodeBot) = state { numGeodeBots = state.numGeodeBots + 1, resources = state.resources `spend` state.blueprint.geodeBotCost }


waitForResources :: Int -> BotState -> Resources -> BotState
waitForResources timeLimit state resources = result where
    atTimeLimitOrHasEnough :: BotState -> Bool
    atTimeLimitOrHasEnough s = s.time >= timeLimit || s.resources `hasEnoughFor` resources
    result = until atTimeLimitOrHasEnough incrementTime state

creditGeodes :: Int -> BotState -> BotState
creditGeodes timeLeft state = result where
  numGeodeBots = state.numGeodeBots
  producedGeodes = numGeodeBots * (timeLeft - state.time)
  result = state { resources = state.resources { geodes = state.resources.geodes + producedGeodes }, numGeodeBots = 0 }

applyOperation :: Int -> BotState -> Operation -> BotState
applyOperation timeLimit state op = result where
    resources = case op of
        OreBot -> state.blueprint.oreBotCost
        ClayBot -> state.blueprint.clayBotCost
        ObsidianBot -> state.blueprint.obsidianBotCost
        GeodeBot -> state.blueprint.geodeBotCost
    stateWithMaybeEnough = waitForResources timeLimit state resources
    stateWithCorrectTime = if stateWithMaybeEnough.time < timeLimit then incrementTime stateWithMaybeEnough else stateWithMaybeEnough
    result = if stateWithMaybeEnough.resources `hasEnoughFor` resources then  (applyBuildOperation stateWithCorrectTime op) else stateWithCorrectTime
    result' = creditGeodes timeLimit result

toResourceList :: Blueprint -> [Resources]
toResourceList blueprint = [blueprint.oreBotCost, blueprint.clayBotCost, blueprint.obsidianBotCost, blueprint.geodeBotCost]


incrementTime :: BotState -> BotState
incrementTime state = state { time = state.time + 1, resources = result } where
    result = Resources (state.resources.ore + state.numOreBots) (state.resources.clay + state.numClayBots) (state.resources.obsidian + state.numObsidianBots) (state.resources.geodes + state.numGeodeBots)

enumerateOperations :: Int -> BotState -> [Operation]
enumerateOperations timeLimit state =  result where
    timeLeft = timeLimit - state.time
    mightNeedOreBot = any (\c -> timeLeft * c > state.numOreBots * timeLeft + state.resources.ore ) $ fmap (.ore) $ toResourceList state.blueprint
    mightNeedClayBot = any (\c -> timeLeft * c > state.numClayBots * timeLeft + state.resources.clay ) $ fmap (.clay) $ toResourceList state.blueprint
    mightNeedObsidianBot = any (\c -> timeLeft * c > state.numObsidianBots * timeLeft + state.resources.obsidian ) $ fmap (.obsidian) $ toResourceList state.blueprint
    result = if state.resources `hasEnoughFor` state.blueprint.geodeBotCost then
        [GeodeBot]
      else
        [OreBot | mightNeedOreBot] ++ [ClayBot | mightNeedClayBot] ++ [ObsidianBot | mightNeedObsidianBot] ++ [GeodeBot]

findBestPlan :: (Applicative f) => BotState -> Int -> (BotState -> f (Maybe ([Operation], BotState))) -> BotState -> f (Maybe ([Operation], BotState))
findBestPlan initState timeLimit recur state = finalResult where
    availableOperations = enumerateOperations timeLimit state
    subPlan op = fmap (\(ops, s) -> (op:ops, s)) <$> recur (applyOperation timeLimit state op)
    plans =  (catMaybes <$> traverse subPlan availableOperations)
    geodesOf :: ([Operation], BotState) -> Int
    geodesOf (ops, s) = s.resources.geodes :: Int
    result =  (headMay . reverse . sortOn geodesOf) <$> plans
    finalResult = if state.time >= timeLimit then pure (Just ([], state)) else result


foldlUntil :: (a -> b -> a) -> (a -> Bool) -> a -> [b] -> a
foldlUntil f p a l = result where
  (a', l') = foldl' (\(a, l) b -> if p a then (a, []) else (f a b, l)) (a, l) l
  result = if p a' then a' else foldlUntil f p a' l'

shouldCut :: BotState -> Int -> BotState -> ([Operation], BotState) -> Bool
shouldCut initState timeLimit state (ops, s) = answer where
  makeGeodeBot bs = applyBuildOperation (incrementTime bs) GeodeBot
  upperBound = until (\s -> s.time >= timeLimit) makeGeodeBot state
  answer = s.resources.geodes >= upperBound.resources.geodes

makeEquivOrBetter :: Int -> Int -> BotState -> [Operation] -> BotState
makeEquivOrBetter timelimit targettime bs [] = bs where
makeEquivOrBetter timelimit targettime bs (op:ops) =  result where
  next = applyOperation timelimit bs op
  result = if next.time > targettime then bs else makeEquivOrBetter timelimit targettime next ops


shouldCut2 :: BotState -> Int -> BotState -> ([Operation], BotState) -> Bool
shouldCut2 initState timeLimit state (ops, s) = answer where
  equivalentState = makeEquivOrBetter timeLimit state.time initState ops
  resourcesDominated = equivalentState.resources `hasEnoughFor` state.resources
  botsDominated = equivalentState.numOreBots >= state.numOreBots && equivalentState.numClayBots >= state.numClayBots && equivalentState.numObsidianBots >= state.numObsidianBots && equivalentState.numGeodeBots >= state.numGeodeBots
  answer = resourcesDominated && botsDominated && equivalentState.time <= state.time
  db = if answer then trace ("shouldCut2: " ++ show (show equivalentState ++ "\ndominates\n" ++ show state)) else id

shouldCut3 :: BotState -> Int -> BotState -> ([Operation], BotState) -> Bool
shouldCut3 initState timeLimit state (ops, s) = answer where
  equivalentState = makeEquivOrBetter timeLimit state.time initState ops
  answer = equivalentState.numGeodeBots >= (state.numGeodeBots + 2)
  db = if answer then trace ("shouldCut3: " ++ show (show equivalentState ++ "\ndominates\n" ++ show state)) else id


allCuts :: BotState -> Int -> BotState -> ([Operation], BotState) -> Bool
allCuts initState timeLimit state (ops, s) = answer where
  one = shouldCut initState timeLimit state (ops, s)
  two = shouldCut2 initState timeLimit state (ops, s)
  three = shouldCut3 initState timeLimit state (ops, s)
  answer = one || two || three

dontCut :: BotState -> Int -> BotState -> ([Operation], BotState) -> Bool
dontCut initState timeLimit state (ops, s) = False


parseResource :: Parser (String, Int)
parseResource = do
    value <- parseNumber
    space
    name <- many1 letter
    return (name, value)

parseResources :: String -> Parser Resources
parseResources s = do
  string $ " Each "++s++" robot costs "
  resources <- sepBy1 parseResource (string " and ")
  string "."
  return $ Resources (fromMaybe 0 $ lookup "ore" resources) (fromMaybe 0 $ lookup "clay" resources) (fromMaybe 0 $ lookup "obsidian" resources) (fromMaybe 0 $ lookup "geodes" resources)


parseBlueprint :: Parser Blueprint
parseBlueprint = do
    string "Blueprint "
    id <- parseNumber
    string ":"
    oreBotCost <- parseResources "ore"
    clayBotCost <- parseResources "clay"
    obsidianBotCost <- parseResources "obsidian"
    geodeBotCost <- parseResources "geode"
    return $ Blueprint id oreBotCost clayBotCost obsidianBotCost geodeBotCost

parseBlueprints :: Parser [Blueprint]
parseBlueprints = do
    sepBy1 parseBlueprint newline

display :: ([Operation], BotState) -> IO ()
display (ops, state) = do
    mapM_ print ops
    print state
    print $ state.resources
    putStrLn $ "Geodes: " ++ show state.resources.geodes
    putStrLn $ "Quality Score: " ++ show (state.resources.geodes * state.blueprint.id)
    putStrLn $ "Time reached: " ++ show (state.time)



main :: IO ()
main = do
  t1 <- liftIO getCPUTime
  s <- resource "geode-bot"
  let entries = runParser parseBlueprints () "input" s
  let stuff = case entries of
          Left err -> error $ show err
          Right stuff -> stuff
  let initState = BotState { numOreBots = 1, numClayBots = 0, numObsidianBots = 0, numGeodeBots = 0, blueprint = stuff !! 0, resources = Resources 0 0 0 0, time = 0 }
  let timeLimit = 24
  let findBestSpecific = findBestPlan initState timeLimit
  let findBestMemo = memoizeBB (compare `on` ((.geodes) . (.resources) . snd)) (allCuts initState timeLimit) findBestSpecific
  let initStates = [BotState { numOreBots = 1, numClayBots = 0, numObsidianBots = 0, numGeodeBots = 0, blueprint = b, resources = Resources 0 0 0 0, time = 0 } | b <- stuff]
  let results = catMaybes $ fmap (findBestMemo) initStates
  mapM_ display results
  let qualitySum = sum $ fmap (\(ops, state) -> state.resources.geodes * state.blueprint.id) results :: Int
  print qualitySum

  let higherLimit = 32
  let findBestSpecific2 = findBestPlan initState higherLimit
  let findBestMemo2 = memoizeBB (compare `on` ((.geodes) . (.resources) . snd)) (allCuts initState higherLimit) findBestSpecific2

  let part2States = filter (\s -> s.blueprint.id <= 3) $ initStates
  let results2 = catMaybes $ fmap (findBestMemo2) part2States
  mapM_ display results2
  let qualityProduct = product $ fmap (\(ops, state) -> state.resources.geodes) results2 :: Int
  print qualityProduct


--  display result
--  let ops = [ClayBot, ClayBot, ClayBot, ObsidianBot, ClayBot, ObsidianBot, GeodeBot, GeodeBot, GeodeBot, GeodeBot, GeodeBot]
--  let state = scanl' (applyOperation timeLimit) initState ops
--  mapM_ display $ fmap (\s -> ([], s)) state


--  let result2 = fromJust $ findBestMemo (initState { blueprint = stuff !! 1})
--  display result2


  t2 <- liftIO getCPUTime
  let diff = (fromIntegral (t2 - t1)) / (10^12)
  putStrLn $ "Computation time: " ++ show diff ++ " sec"

  -- 15180 is too low