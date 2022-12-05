module Advent1 where
  
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.ParserCombinators.Parsec.Char
import Data.Either
import Data.List
import Util

eol :: Parsec String () String
eol = try (string "\n\r")
  <|> string "\r\n"
  <|> string "\n"
  <|> string "\r"
  <|> (eof >> return "")
  <?> "end of line"

parseSingleBackpack :: Parsec String () [Integer]
parseSingleBackpack = fmap read (many1 digit) `sepEndBy` eol

parseAllBackpacks :: Parsec String () [[Integer]]
parseAllBackpacks = parseSingleBackpack `sepBy` (newline)

main = do
    s <- resource "nums"
    let backpacks = fromRight [[]] (runParser (parseAllBackpacks <* spaces) () "nums" s)
    let sums = fmap sum backpacks
    let maxCal = maximum sums
    print maxCal
    let top3 = take 3 $ reverse $ sort sums
    let top3sum = sum top3
    print top3sum


