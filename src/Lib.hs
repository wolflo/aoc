module Lib
    ( readInt
    , readListInt
    ) where

readInt :: String -> Int
readInt = read

readListInt :: String -> [Int]
readListInt = read

matrify :: [(a, b)] -> ([a], [b])
matrify xs = (map fst xs, map snd xs)
