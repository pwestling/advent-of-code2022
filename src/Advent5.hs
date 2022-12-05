{-# LANGUAGE OverloadedStrings #-}

module Advent5 (main) where


import Data.Either
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Char
import Util
import Text.Parsec.Error (setErrorMessage)
import Data.List
import Data.List.Index

newtype Box = Box Char deriving (Show)

newtype Stack a = Stack [a] deriving (Show)

newStack :: [a] -> Stack a
newStack as = Stack as

push :: a -> Stack a -> Stack a
push a (Stack xs) = Stack (a:xs)

pop :: Stack a -> (a, Stack a)
pop (Stack (x:xs)) = (x, Stack xs)

toList :: Stack a -> [a]
toList (Stack xs) = xs

data Instruction = Instruction
  { from :: Int,
    to :: Int,
    amount :: Int
  }
  deriving (Show)

parseBox :: Parser Box
parseBox = do
  char '['
  x <- letter
  char ']'
  return (Box x)

parseBoxLine :: Parser [Either Box ()]
parseBoxLine = sepBy (fmap Left parseBox <|> fmap Right (string "   " >> return ())) (string " ")

parseBoxPile :: Parser [[Either Box ()]]
parseBoxPile = manyTill (parseBoxLine <* newline) (notFollowedBy parseBoxLine)

parseStackLabels :: Parser [String]
parseStackLabels = manyTill (spaces >> many1 digit) (notFollowedBy (spaces >> many1 digit))

parseInstruction :: Parser Instruction
parseInstruction = do
  string "move "
  amount <- many1 digit
  string " from "
  from <- many1 digit
  string " to "
  to <- many1 digit
  return (Instruction (read from) (read to) (read amount))


parseAll :: Parser ([[Either Box ()]],  [Instruction])
parseAll = do
  boxes <- parseBoxPile
  parseStackLabels
  newline
  newline
  instructions <- sepEndBy parseInstruction newline
  eof
  return (boxes, instructions)


toIndexedStacks :: [[Either Box ()]] -> [Stack Box]
toIndexedStacks boxRows = stacks where
  cols = reverse $ transpose boxRows
  wrappedBoxes = fmap (filter isLeft) cols
  boxes = fmap (fmap (\(Left x) -> x)) wrappedBoxes
  stacks = reverse $ fmap newStack boxes

executeMove :: Instruction -> [Stack Box] -> [Stack Box]
executeMove (Instruction _ _ 0) stacks = stacks
executeMove (Instruction from to amount) stacks = result where
  fromStack = stacks !! (from -1)
  toStack = stacks !! (to -1)
  (popped, newFrom) = pop fromStack
  newTo = push popped toStack
  result = executeMove (Instruction from to (amount -1)) (setAt (from -1) newFrom (setAt (to -1) newTo stacks))

executeMove2 :: Instruction -> [Stack Box] -> [Stack Box]
executeMove2 (Instruction _ _ 0) stacks = stacks
executeMove2 (Instruction from to amount) stacks = result where
  fromS = toList $ stacks !! (from -1)
  toS = toList $ stacks !! (to -1)
  newFrom = drop amount fromS
  newTo = take amount fromS ++ toS
  result = setAt (from -1) (newStack newFrom) (setAt (to -1) (newStack newTo) stacks)

executeAll :: (Instruction -> [Stack Box] -> [Stack Box]) -> [Instruction] -> [Stack Box] -> [Stack Box]
executeAll exec [] stacks = stacks
executeAll exec (i:is) stacks = executeAll exec is (exec i stacks)

main :: IO ()
main = do
  s <- resource "stacked-crates"
  print (parse parseAll "sections" s)
  let (boxes, instructions) = fromRight ([], []) (parse parseAll "sections" s)
  let stacks = toIndexedStacks boxes
  print instructions
  print (executeMove (head instructions) stacks)
  let result = executeAll executeMove instructions stacks
  let resultString = fmap ((\(Box x) -> x) . fst . pop) result
  let result2 = executeAll executeMove2 instructions stacks
  let resultString2 = fmap ((\(Box x) -> x) . fst . pop) result2
  print resultString
  print resultString2
