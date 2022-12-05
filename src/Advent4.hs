module Advent4(main) where

import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.ParserCombinators.Parsec.Char
import Data.Either
import Text.Parsec.String
import Util


data Range = Range Int Int

parseRange :: Parser Range
parseRange = do
    bottom <- many1 digit
    char '-'
    top <- many1 digit
    return (Range (read bottom) (read top))

parseRangePair :: Parser (Range, Range)
parseRangePair = do
    r1 <- parseRange
    char ','
    r2 <- parseRange
    return (r1, r2)

fullyContained :: (Range, Range) -> Bool
fullyContained (Range x1 x2, Range y1 y2) = (x1 <= y1 && y2 <= x2) || (y1 <= x1 && x2 <= y2)

overlaps :: (Range, Range) -> Bool
overlaps (Range x1 x2, Range y1 y2) = (x1 <= y1 && y1 <= x2) || (y1 <= x1 && x1 <= y2) || (x1 <= y2 && y2 <= x2) || (y1 <= x2 && x2 <= y2)

main :: IO ()
main = do
    s <- resource "cleaning-sections"
    let sections = fromRight [] (runParser (sepEndBy parseRangePair newline) () "sections" s)
    let areContained = length $ filter id $ fmap fullyContained sections
    print areContained
    let areOverlaps = length $ filter id $ fmap overlaps sections
    print areOverlaps

