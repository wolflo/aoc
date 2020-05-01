module Day04 where

import           Prelude
import           Data.List.Split (splitOn)
import           Text.Read (readMaybe)
import           Data.Char (ord)

import Lib (readInt)

inputPath = "src/inputs/day04.txt"
-- inputPath = "src/inputs/foo.txt"
-- inputPath = "src/inputs/bar.txt"

----- Part 1
p1 :: IO Int
p1 = do
  contents <- readFile inputPath
  return . length . strain $ parseRange contents

parseRange :: String -> [Int]
parseRange = range . map readInt . splitOn "-" . init
  where range [l,u] = [l..u]

strain :: [Int] -> [Int]
strain = filter checkAll
  where
    checkAll xs = digits (show xs) && monotonic (show xs) && hasDub (show xs)

consec :: [a] -> [(a,a)]
consec xs = zip xs $ drop 1 xs

digits :: String -> Bool
digits = (==) 6 . length

monotonic :: String -> Bool
monotonic = all inc . consec . map ord
  where inc (a,b) = not $ a > b

hasDub :: String -> Bool
hasDub = any match . consec
  where match (a,b) = a == b
