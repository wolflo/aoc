module Lib
    ( readInt
    , readListInt
    ) where

import Prelude

readInt :: String -> Int
readInt = read

readListInt :: String -> [Int]
readListInt = read

matrify :: [(a, b)] -> ([a], [b])
matrify xs = (map fst xs, map snd xs)
