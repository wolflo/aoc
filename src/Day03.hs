module Day03 where

import Prelude
import Linear.V2
import Data.List (intersect)
import Data.List.Split (splitOn)

-- import Diagrams.TwoD.Vector
import Lib

----- Part 1
p1 :: IO Int
p1 = do
  contents <- readFile "src/inputs/day03.txt"
  return $ crossDist contents

type Pos = V2 Int           -- a position in our 2d space
type Disp = (Int, V2 Int)   -- a displacement. (Magnitude, unit vector)
type Path = [Pos]

origin :: Pos
origin = V2 2 1

crossDist :: String -> Int
crossDist contents = minimum . map (mannDist origin) . tail $ intersect path1 path2
  where
    disps = toDisps $ words contents
    path1 = totalPath origin $ head disps
    path2 = totalPath origin $ last disps

-- |x2 - x1| + |y2 - y1|
mannDist :: Pos -> Pos -> Int
mannDist (V2 x1 y1) (V2 x2 y2) = abs (x2 - x1) + abs (y2 - y1)

totalPath :: Pos -> [Disp] -> Path
totalPath pos []           = []
totalPath pos (disp:disps) = step ++ totalPath endStep disps
  where
    step    = toPath pos disp
    endStep = last step

toPath :: Pos -> Disp -> Path
toPath pos (0, _)       = [pos]
toPath pos (magn, dirV) = pos : toPath pos2 (magn - 1, dirV)
  where pos2 = pos + dirV

toDisps :: [String] -> [[Disp]]
toDisps = map (map fromString . splitOn ",")

fromString :: String -> Disp
fromString (dir:magn) = (readInt magn, unitVect dir)

unitVect :: Char -> V2 Int
unitVect 'R' = V2 1 0
unitVect 'L' = V2 (-1) 0
unitVect 'U' = V2 0 1
unitVect 'D' = V2 0 (-1)
unitVect _   = error "cant decode displacement vector"
