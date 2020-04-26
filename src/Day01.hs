module Day01 where

import Prelude
import System.IO
import Control.Monad

import Lib

----- Part 1
p1 :: IO Int
p1 = do
  contents <- readFile "src/inputs/day01.txt"
  return . sumFuel $ map readInt . words $ contents


fuel :: Int -> Int
fuel mass = floor (fromIntegral mass / 3) - 2

sumFuel :: [Int] -> Int
sumFuel = sum . map fuel


----- Part 2
p2 :: IO Int
p2 = do
  contents <- readFile "src/inputs/day01.txt"
  return . sumRecFuel $ map readInt . words $ contents

recFuel :: Int -> Int
recFuel mass = case compare f 0 of
  GT -> f + recFuel f
  _  -> 0
  where f = fuel mass

sumRecFuel :: [Int] -> Int
sumRecFuel = sum . map recFuel
