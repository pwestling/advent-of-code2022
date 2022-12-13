
module Advent9(main) where

import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Char
import Data.Functor
import Util
import Data.List
import Safe
import Data.Char
import Data.Either
import Debug.Trace

up :: Point -> Point
up (Point x y) = Point x (y + 1)

down :: Point -> Point
down (Point x y) = Point x (y - 1)

left :: Point -> Point
left (Point x y) = Point (x - 1) y

right :: Point -> Point
right (Point x y) = Point (x + 1) y


data Direction = U | D | L | R deriving (Show, Eq)
data Instruction = Instruction Direction Int deriving (Show)

example = "R 4\n\
           \U 4\n\
           \L 3\n\
           \D 1\n\
           \R 4\n\
           \D 1\n\
           \L 5\n\
           \R 2\n"

parseInstructions :: Parser [Instruction]
parseInstructions = do
  rows <- many1 (parseInstruction <* (newline <|> (eof >> return 'a')))
  return rows

parseInstruction :: Parser Instruction
parseInstruction = do
  direction <- oneOf "UDLR"
  spaces
  distance <- many1 digit
  let d = case direction of
        'U' -> U
        'D' -> D
        'L' -> L
        'R' -> R
  return $ Instruction d (read distance)


data RopeState = RopeState [Point] deriving (Show)

dirFn :: Direction -> Point -> Point
dirFn U = up
dirFn D = down
dirFn L = left
dirFn R = right

tailFollowHead :: Point -> Point -> Point
tailFollowHead head@(Point x1 y1) tail@(Point x2 y2)
  | x1 == x2 && y1 > y2+1 = up tail
  | x1 == x2 && y1 < y2-1 = down tail
  | y1 == y2 && x1 > x2+1 = right tail
  | y1 == y2 && x1 < x2-1 = left tail
  | x1 > x2 && y1 > y2 && notNextDiag = up $ right tail
  | x1 > x2 && y1 < y2 && notNextDiag = down $ right tail
  | x1 < x2 && y1 > y2 && notNextDiag = up $ left tail
  | x1 < x2 && y1 < y2 && notNextDiag = down $ left tail
  | otherwise = tail where
    notNextDiag = abs (x1 - x2) + abs (y1 - y2) > 2



breakdown :: Instruction -> [Instruction]
breakdown (Instruction dir 1) = [Instruction dir 1]
breakdown (Instruction dir n) = Instruction dir 1 : breakdown (Instruction dir (n-1))

wagTails :: Point -> [Point] -> [Point]
wagTails h [] = [h]
wagTails h (t:ts) = h : wagTails (tailFollowHead h t) ts

move :: RopeState -> Instruction -> RopeState
move s (Instruction direction 0) = s
move (RopeState (k : ks)) (Instruction direction distance) = result where
  newHead = dirFn direction k
  rope = wagTails newHead ks
  result = move (RopeState rope) (Instruction direction (distance - 1))


getTail :: RopeState -> Point
getTail (RopeState [tail]) = tail
getTail (RopeState (k:ks)) = getTail (RopeState ks)

main :: IO ()
main = do
  s <- resource "rope-instructions"
  let entries = runParser parseInstructions () "ls" s
  print entries
  let instructions = case entries of
        Left err -> error $ show err
        Right instructions -> concatMap breakdown instructions
  print instructions
  let allStates = scanl move (RopeState (replicate 10 (Point 0 0))) instructions
  mapM_ print allStates
  print $ length $ nub $ fmap getTail allStates
