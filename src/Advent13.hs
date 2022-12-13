{-# LANGUAGE RecordWildCards #-}

module Advent13(main) where

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

data List = List [ListItem] deriving (Show, Eq)
data ListItem = N Int | L List deriving (Show, Eq)

parseList :: Parser List
parseList = List <$> (char '[' *> (parseListItem `sepBy` (string "," >> spaces)) <* char ']')

parseListItem :: Parser ListItem
parseListItem = (N <$> read <$> many1 digit) <|> (L <$> parseList)

parseListPair :: Parser (List, List)
parseListPair = (,) <$> parseList <*> (newline *> parseList)

parseListPairList :: Parser [(List, List)]
parseListPairList = parseListPair `sepBy` spaces

compareList :: List -> List -> Ordering
compareList (List []) (List []) = EQ
compareList (List []) (List _) = LT
compareList (List _) (List []) = GT
compareList (List (x:xs)) (List (y:ys)) = compareListItem x y <> compareList (List xs) (List ys)

compareListItem :: ListItem -> ListItem -> Ordering
compareListItem (N x) (N y) = compare x y
compareListItem (N x) (L y) = compareListItem (L $ List [N x]) (L y)
compareListItem (L x) (N y) = compareListItem (L x) (L $ List [N y])
compareListItem (L x) (L y) = compareList x y

instance Ord List where
  compare = compareList

decoderPair :: [List]
decoderPair = [x,y] where
  x = grabRight $ runParser parseList () "" "[[2]]"
  y = grabRight $ runParser parseList () "" "[[6]]"

main :: IO ()
main = do
  s <- resource "signal-order"
  let entries = runParser parseListPairList () "ls" s
  let listPairs = case entries of
          Left err -> error $ show err
          Right listPairs -> listPairs
  print entries
  let results = indexed $ fmap (uncurry compareList) listPairs
  let correctOrderResults = filter (\(i, r) -> r == LT) results
  print $ (sum $ fmap fst correctOrderResults) + length correctOrderResults

  let allLists = concatMap (\(a,b) -> [a,b]) listPairs ++ decoderPair
  let sortedLists = sort allLists
  let firstDecoder = fromJust $ findIndex (== head decoderPair) sortedLists
  let secondDecoder = fromJust $ findIndex (== last decoderPair) sortedLists
  print $ (firstDecoder+1) * (secondDecoder+1)

