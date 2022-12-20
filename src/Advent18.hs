{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Advent18(main) where

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
import Data.List
import qualified Data.Map as Map
import qualified Data.List.Split as Split
import System.CPUTime
import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Data.HashMap.Strict as HMap
import qualified Data.Text as Text
import Data.Hashable
import qualified Data.Set as Set
import Graph


data Point3D = Point3D {x::Int, y::Int, z::Int} deriving (Show, Eq, Ord)

instance Hashable Point3D where
    hashWithSalt salt (Point3D x y z) = hashWithSalt salt (x, y, z)

parsePoint3D :: Parser Point3D
parsePoint3D = do
    x <- parseNumber
    char ','
    y <- parseNumber
    char ','
    z <- parseNumber
    return $ Point3D x y z

parsePoint3Ds :: Parser [Point3D]
parsePoint3Ds = parsePoint3D `sepBy` newline

exposedSides :: Set.Set Point3D -> Point3D -> Int
exposedSides pointset p = length $ airPoints pointset p

airPoints :: Set.Set Point3D -> Point3D -> [Point3D]
airPoints pointset p = exposed where
      isFace dx dy dz = (length $ filter (== 0) [dx, dy, dz]) == 2
      neighbors = [Point3D (p.x + dx) (p.y + dy) (p.z + dz) | dx <- [-1..1], dy <- [-1..1], dz <- [-1..1], isFace dx dy dz]
      exposed = filter (\p -> not $ Set.member p pointset) neighbors

data Bounds3D = Bounds3D {minX::Int, maxX::Int, minY::Int, maxY::Int, minZ::Int, maxZ::Int} deriving (Show, Eq, Ord)

getBounds :: [Point3D] -> Bounds3D
getBounds points = Bounds3D minX maxX minY maxY minZ maxZ where
    minX = minimum $ map (.x) points
    maxX = maximum $ map (.x) points
    minY = minimum $ map (.y) points
    maxY = maximum $ map (.y) points
    minZ = minimum $ map (.z) points
    maxZ = maximum $ map (.z) points

rayTraceIsInside :: Bounds3D -> Set.Set Point3D -> Point3D -> Bool
rayTraceIsInside bounds pointset p = isInside where
    rayUp = takeWhile (\p -> p.y <= bounds.maxY ) $ iterate (\p -> p {y = p.y+1}) p
    rayDown = takeWhile (\p -> p.y >= bounds.minY ) $ iterate (\p -> p {y = p.y-1}) p
    rayLeft = takeWhile (\p -> p.x >= bounds.minX ) $ iterate (\p -> p {x = p.x-1}) p
    rayRight = takeWhile (\p -> p.x <= bounds.maxX ) $ iterate (\p -> p {x = p.x+1}) p
    rayFront = takeWhile (\p -> p.z >= bounds.minZ ) $ iterate (\p -> p {z = p.z-1}) p
    rayBack = takeWhile (\p -> p.z <= bounds.maxZ ) $ iterate (\p -> p {z = p.z+1}) p
    rayStrikes ray = any (\p -> Set.member p pointset) ray
    isInside = all id [rayStrikes rayUp, rayStrikes rayDown, rayStrikes rayLeft, rayStrikes rayRight, rayStrikes rayFront, rayStrikes rayBack]

inBounds :: Bounds3D -> Int -> Point3D -> Bool
inBounds bounds n p = all id [p.x >= bounds.minX - n, p.x <= bounds.maxX + n, p.y >= bounds.minY - n, p.y <= bounds.maxY + n, p.z >= bounds.minZ - n, p.z <= bounds.maxZ + n]

bfsIsInside :: Bounds3D -> Set.Set Point3D -> Point3D -> Bool
bfsIsInside bounds pointset p = isNothing bfsResult where
  bfsResult = bfs (\p -> filter (inBounds bounds 1) $ airPoints pointset p) (not. (inBounds bounds 0)) p

tr :: Show a => String -> a -> a
tr s a = trace (s ++ show a) a

getExternalExposedPoints :: Set.Set Point3D -> [Point3D]
getExternalExposedPoints pointset = trueAir where
    bounds = getBounds $ Set.toList pointset
    isPointInside p = bfsIsInside bounds pointset p
    air = Set.fromList $ concatMap (airPoints pointset) $ Set.toList pointset
    internal = tr "Internal pointset: " $ Set.filter (isPointInside) $ air
    internalPoints =  Set.union internal pointset
    trueAir = concatMap (airPoints internalPoints) $ Set.toList pointset

main :: IO ()
main = do
  t1 <- liftIO getCPUTime
  s <- resource "lava-drop"
  let entries = runParser parsePoint3Ds () "input" s
  let stuff = case entries of
          Left err -> error $ show err
          Right stuff -> stuff
  let pointSet = Set.fromList stuff
  let exposed = sum $ fmap (exposedSides pointSet) stuff
  putStrLn $ "Exposed faces: " ++ show exposed

  let externalExposed = getExternalExposedPoints pointSet
  let externalExposedCount = length externalExposed
  putStrLn $ "External exposed faces: " ++ show externalExposedCount

  t2 <- liftIO getCPUTime
  let diff = (fromIntegral (t2 - t1)) / (10^12)
  putStrLn $ "Computation time: " ++ show diff ++ " sec"

-- 2585 is too low