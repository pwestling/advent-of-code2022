{-# LANGUAGE RecordWildCards #-}

module Advent11(main) where

import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Char
import Data.Functor
import Util
import Data.Char
import Data.Either
import Debug.Trace
import Data.Maybe
import Data.List.Index
import Data.List

data Item = Item Int deriving (Show, Eq, Ord)
data Operand = Const Int | Old deriving (Show, Eq, Ord)
data Op = Mult Operand | Add Operand deriving (Show, Eq, Ord)
data Test = Divisible Int deriving (Show, Eq, Ord)
data Targets = Targets {
  ifTrue :: Int,
  ifFalse :: Int
} deriving (Show, Eq, Ord)

data Monkey = Monkey {
    id :: Int,
    items :: [Item],
    operation :: Op,
    test :: Test,
    target :: Targets,
    inspections :: Int
  } deriving (Show, Eq)



parseMonkey :: Parser Monkey
parseMonkey = do
  string "Monkey "
  id <- read <$> many1 digit
  string ":"
  spaces
  string "Starting items: "
  items <- fmap (Item . read) <$> (many1 digit) `sepBy` (string ", ")
  spaces
  string "Operation: new = old "
  opType <- (string "* " $> Mult) <|> (string "+ " $> Add)
  operand <- (string "old" $> Old) <|> (Const . read <$> many1 digit)
  let operation = opType operand
  spaces
  test <- string "Test: divisible by " >> (Divisible <$> read <$> many1 digit)
  spaces
  string "If true: throw to monkey "
  ifTrue <- read <$> many1 digit
  spaces
  string "If false: throw to monkey "
  ifFalse <- read <$> many1 digit
  let target = Targets {ifFalse=ifFalse, ifTrue=ifTrue}
  let inspections = 0
  return Monkey{..}

parseMonkeys :: Parser [Monkey]
parseMonkeys = parseMonkey `sepBy` spaces

operate :: Int -> Op -> Item -> Item
operate manage (Mult Old) (Item i) = Item ((i * i) `mod` manage)
operate manage (Mult (Const i)) (Item j) = Item ((j * i) `mod` manage)
operate manage (Add Old) (Item i) = Item ((i + i) `mod` manage)
operate manage (Add (Const i)) (Item j) = Item ((j + i) `mod` manage)

testItem :: Test -> Item -> Bool
testItem (Divisible i) (Item j) = j `mod` i == 0


runTurn :: Int -> [Monkey] -> Int -> [Monkey]
runTurn manage monkeys monkeyId = result where
  monkey = monkeys !! monkeyId
  newItems = operate manage (operation monkey) <$> items monkey
  Targets{..} = target monkey
  trueMonkey = monkeys !! ifTrue
  falseMonkey = monkeys !! ifFalse
  trueItems = items trueMonkey ++ filter (testItem (test monkey)) newItems
  falseItems = items falseMonkey ++ filter (not . testItem (test monkey)) newItems
  newTrueMonkey = trueMonkey {items = trueItems}
  newFalseMonkey = falseMonkey {items = falseItems}
  newCurrentMonkey = monkey {items = [], inspections = inspections monkey + length newItems}
  result = setAt monkeyId newCurrentMonkey $ setAt ifTrue newTrueMonkey $ setAt ifFalse newFalseMonkey monkeys

runRound :: Int -> [Monkey] -> [Monkey]
runRound manage monkeys = foldl (runTurn manage) monkeys [0..length monkeys - 1]

runNRounds :: Int -> Int -> [Monkey] -> [Monkey]
runNRounds manage 0 monkeys = monkeys
runNRounds manage n monkeys = (runNRounds manage) (n - 1) (runRound manage monkeys)

monkeyBusiness :: [Monkey] -> Int
monkeyBusiness monkeys = foldl (*) 1 $ take 2 $ reverse $ sort $ fmap inspections monkeys

fromDivisible :: Test -> Int
fromDivisible (Divisible i) = i

managementNumber :: [Monkey] -> Int
managementNumber monkeys = foldl (*) 1 $ fmap (fromDivisible . test) monkeys


main :: IO ()
main = do
  s <- resource "monkey-items"
  let entries = runParser parseMonkeys () "ls" s
  let monkeys = case entries of
          Left err -> error $ show err
          Right monkeys -> monkeys
  let m = managementNumber monkeys
  print m
  mapM_ print $ runNRounds m 10000 monkeys
  print $ monkeyBusiness $ runNRounds m 10000 monkeys


