{-# LANGUAGE RecordWildCards #-}

module Advent10(main) where

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

data Instruction = Noop | Add Int deriving (Show, Eq)

parseNoop :: Parser Instruction
parseNoop = string "noop" >> return Noop

parseAdd :: Parser Instruction
parseAdd = do
  string "addx"
  spaces
  neg <- optionMaybe (char '-')
  value <- many1 digit
  let v = case neg of
        Nothing -> read value
        Just _ -> -1 * read value
  return $ Add v

parseInstructions :: Parser [Instruction]
parseInstructions = sepBy (parseNoop <|> parseAdd) newline <* end

data Machine = Machine {
  register :: Int,
  cycle :: Int
} deriving (Show, Eq)


executeMachine :: Machine -> [Instruction] -> [Machine]
executeMachine m [] = [m]
executeMachine (Machine r c) (Noop : is) = (Machine r (c+1)) : executeMachine (Machine r (c+1)) is
executeMachine (Machine r c) (Add v : is) = (Machine r (c+1)) : Machine r (c+2) : executeMachine (Machine (r+v) (c+2)) is

strength :: Machine -> Int
strength Machine{..} = register * cycle

isTargetCycle :: Machine -> Bool
isTargetCycle Machine{..} = cycle `elem` [20,60,100,140,180,220]

screenXY :: Machine -> Point
screenXY (Machine _ cycle) = Point (cycle `mod` 40) (cycle `div` 40)

spritePoints :: Machine -> [Point]
spritePoints (Machine r c) = [Point (r) y, Point (r+1) y, Point (r+2) y]
  where (Point _ y) = screenXY (Machine r c)

litPoint :: Machine -> Maybe Point
litPoint m = if (screenXY m) `elem` (spritePoints m) then Just (screenXY m) else Nothing

printer :: [Point] -> Int -> Int -> IO ()
printer _ _ 7 = return ()
printer ps x y = do
  if (Point x y) `elem` ps then putStr "#" else putStr "."
  if x == 40 then putStrLn "" >> printer ps 1 (y+1) else printer ps (x+1) y

main :: IO ()
main = do
  s <- resource "cycle-cmds"
  let entries = runParser parseInstructions () "ls" s
  print entries
  let instructions = case entries of
        Left err -> error $ show err
        Right instructions -> instructions
  let baseMachine = Machine 1 0
  let machines = executeMachine baseMachine instructions
  let targets = filter isTargetCycle machines
  mapM_ print $ machines
  print $ sum $ map strength targets
  let litPoints = mapMaybe litPoint machines
  printer litPoints 1 0

