{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Advent21 where

import System.CPUTime (getCPUTime)
import Control.Monad.IO.Class (liftIO)
import Util
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Char

import qualified Data.Map as Map

data Var = Var String deriving (Show, Eq, Ord)
data Op = Add | Mul | Div | Sub | Eq deriving (Show, Eq, Ord)
data Exp = Const Int | Binary Op Var Var | Unknown deriving (Show, Eq, Ord)

data Assignment = Assignment {
    name :: Var,
    expression :: Exp
} deriving (Show, Eq, Ord)

parseExp :: Parser Exp
parseExp = (Const <$> parseNumber) <|> parseBinary where
    parseBinary = do
        a <- Var <$> many1 letter
        spaces
        op <- oneOf "+-*/"
        spaces
        b <- Var <$> many1 letter
        let op' = case op of
                '+' -> Add
                '-' -> Sub
                '*' -> Mul
                '/' -> Div
        return $ Binary op' a b

parseAssignment :: Parser Assignment
parseAssignment = do
    name <- Var <$> many1 letter
    if name == (Var "humn") then do
      string ": "
      _ <- parseExp
      return $ Assignment name Unknown
    else if name == (Var "root") then do
      string ": "
      (Binary _ a1 a2) <- parseExp
      return Assignment { name = name, expression = (Binary Eq a1 a2) }
    else do
      string ": "
      expression <- parseExp
      return Assignment { name = name, expression = expression }


toTree :: [Assignment] -> Map.Map Var Assignment
toTree = Map.fromList . map (\a -> (a.name, a))


getValForVar :: Map.Map Var Assignment -> Var -> Maybe Int
getValForVar tree var = case Map.lookup var tree of
    Nothing -> error $ "No value for " ++ show var
    Just a -> case a.expression of
        Const i -> Just i
        Binary op a b -> let
            a' = getValForVar tree a
            b' = getValForVar tree b
            in case (a', b', op) of
                (Just a', Just b', Add) -> Just $ a' + b'
                (Just a', Just b', Sub) -> Just $ a' - b'
                (Just a', Just b', Mul) -> Just $ a' * b'
                (Just a', Just b', Div) -> Just $ a' `div` b'
                (Nothing, _, _) -> Nothing
                (_, Nothing, _) -> Nothing
        Unknown -> Nothing


solveFor :: Map.Map Var Assignment -> Var -> Int -> Int
solveFor tree var target = case Map.lookup var tree of
    Nothing -> error $ "No value for " ++ show var
    Just a -> case a.expression of
        Const i -> error $ "Can't solve for " ++ show var ++ " because it's already a constant"
        Binary op a b -> let
            a' = getValForVar tree a
            b' = getValForVar tree b
            in case (a', b', op) of
                (Just a', Nothing, Add) -> solveFor tree b (target - a')
                (Nothing, Just b', Add) -> solveFor tree a (target - b')
                (Just a', Nothing, Sub) -> solveFor tree b (a' - target)
                (Nothing, Just b', Sub) -> solveFor tree a (target + b')
                (Just a', Nothing, Mul) -> solveFor tree b (target `div` a')
                (Nothing, Just b', Mul) -> solveFor tree a (target `div` b')
                (Just a', Nothing, Div) -> solveFor tree b (a' `div` target)
                (Nothing, Just b', Div) -> solveFor tree a (target * b')
        Unknown -> target

solveRoot :: Map.Map Var Assignment -> Int
solveRoot tree = case Map.lookup (Var "root") tree of
    Nothing -> error "No root"
    Just a -> case a.expression of
        Binary Eq a b -> let
            a' = getValForVar tree a
            b' = getValForVar tree b
            in case (a', b') of
                (Just a', Just b') -> a'
                (Nothing, Just b') -> solveFor tree a b'
                (Just a', Nothing) -> solveFor tree b a'
                (Nothing, Nothing) -> error "Can't solve for root"


main :: IO ()
main = do
  t1 <- liftIO getCPUTime
  s <- resource "monkey-yell"
  let entries = runParser (parseAssignment `sepBy` newline) () "input" s
  let stuff = case entries of
          Left err -> error $ show err
          Right stuff -> stuff
  let tree = toTree stuff
  print stuff
  print $ length stuff
  let result = solveRoot tree
  print result


  t2 <- liftIO getCPUTime
  let diff = (fromIntegral (t2 - t1)) / (10^12)
  putStrLn $ "Computation time: " ++ show diff ++ " sec"