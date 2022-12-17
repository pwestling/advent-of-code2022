module Advent15(main) where

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
import Data.List.Index
import Data.List
import qualified Data.Map as Map
import qualified Data.List.Split as Split

data Sensor = Sensor {
   location :: Point,
   nearestBeacon :: Point
} deriving (Show, Eq)

parseSensor :: Parser Sensor
parseSensor = do
  string "Sensor at x="
  x <- parseNumber
  string ", y="
  y <- parseNumber
  string ": closest beacon is at x="
  bx <- parseNumber
  string ", y="
  by <- parseNumber
  return $ Sensor (Point x y) (Point bx by)

parseSensors :: Parser [Sensor]
parseSensors = parseSensor `sepBy` newline

getRadius :: Sensor -> Int
getRadius (Sensor s b) = distance s b

inRangeOf :: Sensor -> Point -> Bool
inRangeOf sensor point = distance (location sensor) point <= getRadius sensor

leftMostEdge :: Sensor -> Point
leftMostEdge sensor = Point (x - radius) y
  where
    Point x y = location sensor
    radius = getRadius sensor

rightMostEdge :: Sensor -> Point
rightMostEdge sensor = Point (x + radius) y
  where
    Point x y = location sensor
    radius = getRadius sensor

xBounds :: [Sensor] -> (Int, Int)
xBounds sensors = (minimum xs, maximum xs)
  where
    leftMost sensor = getRadius sensor + getX (location sensor)
    rightMost sensor = getX (location sensor) - getRadius sensor
    xs = map leftMost sensors ++ map rightMost sensors


closeEnoughSensor :: Int -> Sensor -> Bool
closeEnoughSensor rowY s@(Sensor location beacon) = result where
  sensorRadius = getRadius s
  sensorYMax = getY location + sensorRadius
  sensorYMin = getY location - sensorRadius
  result = sensorYMax >= rowY && sensorYMin <= rowY



circumfrencePoints :: Sensor -> [Point]
circumfrencePoints sensor = points
  where
    radius = getRadius sensor + 1
    Point cx cy = location sensor
    upLeftPoints = map (\i -> Point ((cx - radius) + i) (cy - i)) [0..radius]
    downRightPoints = map (\i -> Point (cx + i) (cy + radius - i)) [0..radius]
    upRightPoints = map (\i -> Point (cx + radius - i) (cy - i)) [0..radius]
    downLeftPoints = map (\i -> Point ((cx - radius) + i) (cy + i)) [0..radius]
    points = upLeftPoints ++ upRightPoints ++ downLeftPoints ++ downRightPoints


drawGrid :: Int -> Int -> [Sensor] -> String
drawGrid lowbound highbound sensors = unlines $ map drawRow [lowbound..highbound] where
  allcircumfrencePoints = concatMap circumfrencePoints sensors
  drawRow y = map (\x -> drawPoint (Point x y)) [lowbound..highbound]
  drawPoint p = glyph
    where
      isInRangeOfSensor = any (\s -> inRangeOf s p) sensors
      isCircumfrencePoint = any (== p) allcircumfrencePoints
      isSensor = any (\s -> location s == p) sensors
      isBeacon = any (\s -> nearestBeacon s == p) sensors
      glyph = if isSensor then 'S'
              else if isBeacon then 'B'
              else if (isCircumfrencePoint && not isInRangeOfSensor) then '+'
              else if isCircumfrencePoint then 'o'
              else if isInRangeOfSensor then '#'
              else '.'


main :: IO ()
main = do
  s <- resource "sensor-beacon"
  let entries = runParser parseSensors () "input" s
  let stuff = case entries of
          Left err -> error $ show err
          Right stuff -> stuff
--  print entries
  let bounds =  xBounds stuff
  let rowPoints = [Point x 2000000 | x <- [fst bounds..snd bounds]]
--  print rowPoints
  let ruledOut = filter (\point -> all (\(Sensor location beacon) -> point /= beacon) stuff) $ filter (\point -> any (\sensor -> inRangeOf sensor point) stuff) rowPoints
--  print $ length rowPoints
--  print $ length ruledOut
  let upper = 4000000

--  print $ length gridPoints
  print $ length stuff
  let gridPoints = concatMap circumfrencePoints stuff
  let sensors = stuff
--  putStrLn $ drawGrid (0) 20 sensors

  print $ length sensors
  let notInRange point = not $ any (\sensor -> inRangeOf sensor point) sensors

  print $ length gridPoints
  let validPoints = filter notInRange $ filter (\(Point x y) -> x <= upper && y <= upper && x >=0 && y >= 0) gridPoints

  print $ length validPoints
  print $ ((getX $ head validPoints) * 4000000 + (getY $ head validPoints))


