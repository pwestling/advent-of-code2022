module Advent2(main) where

import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.ParserCombinators.Parsec.Char
import Data.Either
import Text.Parsec.String
import Util


data RPS = Rock | Paper | Scissors deriving Show

data RPSGame = RPSGame RPS RPS deriving Show
data RPSWinner = Opponent | Me | Draw deriving Show

data RPSGame2 = RPSGame2 RPS RPSWinner deriving Show

parseRPS :: Parser RPS
parseRPS = do
    val <- oneOf "ABCXYZ"
    let result = case val of 'A' -> Rock
                             'B' -> Paper
                             'C' -> Scissors
                             'X' -> Rock
                             'Y' -> Paper
                             'Z' -> Scissors
                             _ -> error "Invalid RPS"

    return result

parseResult :: Parser RPSWinner
parseResult = do
    val <- oneOf "XYZ"
    let result = case val of 'X' -> Opponent
                             'Y' -> Draw
                             'Z' -> Me
                             _ -> error "Invalid RPS"

    return result

parseGame :: Parser RPSGame
parseGame = do
    opponent <- parseRPS
    spaces
    me <- parseRPS
    return (RPSGame opponent me)

parseGame2 :: Parser RPSGame2
parseGame2 = do
    opponent <- parseRPS
    spaces
    me <- parseResult
    return (RPSGame2 opponent me)


determineWinner :: RPSGame -> RPSWinner
determineWinner (RPSGame opponent me) =
    case opponent of   Rock ->   (case me of Rock -> Draw
                                             Paper -> Me
                                             Scissors -> Opponent)
                       Paper ->  (case me of Rock -> Opponent
                                             Paper -> Draw
                                             Scissors -> Me)
                       Scissors ->  (case me of Rock -> Me
                                                Paper -> Opponent
                                                Scissors -> Draw)

winnerPoints :: RPSWinner -> Int
winnerPoints Me = 6
winnerPoints Opponent = 0
winnerPoints Draw = 3

choicePoints :: RPS -> Int
choicePoints Rock = 1
choicePoints Paper = 2
choicePoints Scissors = 3

scoreGame :: RPSGame -> Int
scoreGame game@(RPSGame _ me) = winnerPoints (determineWinner game) + choicePoints me

computeAction :: RPSGame2 -> RPS
computeAction (RPSGame2 opponent winner) =
    case winner of   Me ->   (case opponent of Rock -> Paper
                                               Paper -> Scissors
                                               Scissors -> Rock)
                     Opponent ->  (case opponent of Rock -> Scissors
                                                    Paper -> Rock
                                                    Scissors -> Paper)
                     Draw ->  (case opponent of Rock -> Rock
                                                Paper -> Paper
                                                Scissors -> Scissors)

scoreGame2 :: RPSGame2 -> Int
scoreGame2 game@(RPSGame2 _ winner) = winnerPoints winner + choicePoints (computeAction game)


main :: IO ()
main = do
    s <- resource "rps-game"
    let rpsGames = fromRight [] (runParser (sepEndBy parseGame newline) () "nums" s)
    let scores = fmap scoreGame rpsGames
    print (sum scores)
    let rpsGames2 = fromRight [] (runParser (sepEndBy parseGame2 newline) () "nums" s)

    let scores2 = fmap scoreGame2 rpsGames2
    print (sum scores2)

