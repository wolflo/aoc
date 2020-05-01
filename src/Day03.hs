module Day03 where

import           Prelude
import           Linear.V2
import           Data.List (intersect, minimumBy)
import           Data.List.Split (splitOn)
import qualified Data.Set as Set
import qualified Data.Map as Map

import           Lib

inputPath = "src/inputs/day03.txt"

----- Part 1
p1 :: IO ()
p1 = do
  contents <- readFile inputPath
  print . minCrossDist $ parseInput contents

parseInput :: String -> [[Disp]]
parseInput = map (map parseDisp . splitOn ",") . lines

type Pos = V2 Int           -- a position in our 2d space
type Disp = (Int, V2 Int)   -- a displacement. (Magnitude, unit vector)
type Path = [Pos]

origin :: Pos
origin = V2 0 0

minCrossDist :: [[Disp]] -> Int
minCrossDist = minDist origin . Set.toList . meet . map (Set.fromList) . paths

minDist :: Pos -> [Pos] -> Int
minDist x = minimum . map (mannDist x)

-- |x2 - x1| + |y2 - y1|
mannDist :: Pos -> Pos -> Int
mannDist (V2 x1 y1) (V2 x2 y2) = abs (x2 - x1) + abs (y2 - y1)

totalPath :: Pos -> [Disp] -> Path
totalPath pos []           = [pos]
totalPath pos (disp:disps) = step ++ (totalPath endPos disps)
  where
    (step, [endPos]) = splitAt (length fullStep - 1) fullStep
    fullStep         = toPath pos disp

toPath :: Pos -> Disp -> Path
toPath pos (0, _)       = [pos]
toPath pos (magn, unitV) = pos : toPath pos2 (magn - 1, unitV)
  where pos2 = pos + unitV

parseDisp :: String -> Disp
parseDisp (dir:magn) = (readInt magn, unitVect dir)

unitVect :: Char -> V2 Int
unitVect dir = case dir of
  'R' -> V2 1 0
  'L' -> V2 (-1) 0
  'U' -> V2 0 1
  'D' -> V2 0 (-1)
  _   -> error "cant decode displacement vector"

----- Part 2
p2 :: IO ()
p2 = do
  contents <- readFile inputPath
  print . minTime $ parseInput contents

minTime :: [[Disp]] -> Int
minTime disps =  minimum . map timeToPos $ Set.toList meets
  where
    wires         = paths disps
    meets         = meet $ map Set.fromList wires
    times         = map stepMap wires
    timeToPos pos = sumSteps pos times

sumSteps :: Ord k => k -> [Map.Map k Int] -> Int
sumSteps k = sum . map (\m -> m Map.! k)

meet :: Ord a => [Set.Set a] -> Set.Set a
meet (s:[]) = s
meet (s:ss) = Set.intersection s $ meet ss

paths :: [[Disp]] -> [Path]
paths = map (tail . totalPath origin)

stepMap :: Ord a => [a] -> Map.Map a Int
stepMap = Map.fromList . reverse . flip zip [1..]
