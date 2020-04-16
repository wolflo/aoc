module Day02 where

import Data.List
import System.IO
import Control.Monad

import Lib

----- Part 1
p1 :: IO [Int]
p1 = do
  contents <- readFile "src/inputs/day02.txt"
  return . run . initialState $ readListInt $ "[" ++ contents ++ "]"

type PC = Int
type Prog = [Int]

data OP = Halt | Add | Mul deriving (Eq)
fromInt :: Int -> OP
fromInt x = case x of
  1 -> Add
  2 -> Mul
  _ -> Halt

data State = State {
  pc :: PC,
  prog :: Prog
  } deriving (Show)

initialState :: Prog -> State
initialState = State 0 . writePos 2 2 . writePos 1 12

run :: State -> Prog
run s = if done then prog s else run $ step s
  where done = pc s >= length (prog s)

step :: State -> State
step s = case op of
  Halt -> endState s
  Add  -> nextState (+) s
  Mul  -> nextState (*) s
  where op = fromInt $ readPos (pc s) (prog s)

nextState :: (Int -> Int -> Int) -> State -> State
nextState f (State pc prog) = State (incPC pc) (exec f opArgs prog)
  where opArgs = map3 (`readPos` prog) (pc + 1, pc + 2, pc + 3)

endState :: State -> State
endState (State pc' prog') = State (length prog') prog'

exec :: (Int -> Int -> Int) -> (Int, Int, Int) -> Prog -> Prog
exec f (pa1, pa2, po) prog = writePos po outval prog
  where arg1   = readPos pa1 prog
        arg2   = readPos pa2 prog
        outval = f arg1 arg2

incPC :: PC -> PC
incPC = (+4)

readPos :: Int -> [Int] -> Int
readPos = flip (!!)

writePos :: Int -> a -> [a] -> [a]
writePos pos val = modNth pos (const val)

modNth :: Int -> (a -> a) -> [a] -> [a]
modNth n f xs = ls ++ f mid : rs
  where (ls, mid:rs) = splitAt n xs

map3 :: (a -> b) -> (a, a, a) -> (b, b, b)
map3 f (x, y, z) = (f x, f y, f z)


----- Part 2
p2 :: IO [(Int, Int)]
p2 = do
  contents <- readFile "src/inputs/day02.txt"
  return . solveP2 . prog . initialState $ readListInt $ "[" ++ contents ++ "]"

type Index      = Int
type Range      = [Int]
type Constraint = (Index, Int)
type Sub        = (Index, Int)
type DOF        = (Index, Range)

constraintsP2 :: [Constraint]
constraintsP2 = [(0, 19690720)]
degOfFreedomP2 :: [DOF]
degOfFreedomP2 = [(1,[0..99]), (2,[0..99])]

solveP2 :: Prog -> [Sub]
solveP2 = solve constraintsP2 degOfFreedomP2

combineInputs :: Int -> Int -> Int
combineInputs = (+) . (*) 100

runProg :: Prog -> Prog
runProg = run . State 0

-- first solution
-- solve prog = head [(x1, x2) | x1 <- [0..99], x2 <- [0..99], satisfies c1 (runProg (plug [(1, x1), (2, x2)] prog))]

solve :: [Constraint] -> [DOF] -> Prog -> [Sub]
solve cs dofs prog = head [ss | ss <- drawSubs dofs, solves ss]
  where solves ss = satisfiesAll cs (runProg (plug ss prog))

drawSubs :: [DOF] -> [[Sub]]
drawSubs dofs = sequence [draw d | d <- dofs]
  where draw (idx, range) = [(idx, val) | val <- range]

plug :: [Sub] -> Prog -> Prog
plug [] = id
plug ((si,sv):ss) = plug ss . writePos si sv

satisfiesAll :: [Constraint] -> Prog -> Bool
satisfiesAll [] _ = True
satisfiesAll (c:cs) prog = satisfies c prog && satisfiesAll cs prog

satisfies :: Constraint -> [Int] -> Bool
satisfies (cidx, cval) xs = cval == xs !! cidx
