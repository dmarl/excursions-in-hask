import Control.Monad
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe

word1 = [('b', 0), ('a', 1), ('b', 2) , ('b', 2)] :: [(Char, Int)]
word2 = [('u', 1), ('v', 1), ('v', -1), ('u', -1)] :: [(Char, Int)]
word3 = [('b', 0), ('b', 1), ('b', 2) , ('b', 2)] :: [(Char, Int)]
word4 = [('b', 1), ('b', 1), ('b', 1) , ('b', 1), ('b', 1)] :: [(Char, Int)]

cubeVs :: [[Double]]
cubeVs = replicateM 3 [0, 1]

cubeEs = concatMap adj cubeVs where
  adj v = map (v:) [[as ++ 1:cs] | v@(as, b:cs) <-
    init $ zip (inits v) (tails v), b == 0]

canon ::[(Char, Int)] -> [(Char, Int)]
canon [] = []
canon (as@(a0, a1):bs) | a1 == 0 = canon bs
                        | otherwise  = as : canon bs

filtr :: [(Char, Int)] -> [(Char, Int)] -> [(Char, Int)]
filtr pre [] = pre
filtr [] ((a0, b0):(a1, b1):cs) | a0 == a1 && b0 + b1 == 0  = filtr [] cs
                                 | a0 == a1 && b0 + b1 /= 0  = filtr [(a0, b0+b1)] cs
                                 | otherwise                 = filtr [(a0, b0)] ((a1, b1):cs)
filtr pre ((a0, b0):(a1, b1):cs) | a0 == a1 && b0 + b1 == 0 = filtr (init pre) (last(pre) : cs)
                                  | a0 == a1 && b0 + b1 /= 0 = filtr pre ((a0, b0 + b1):cs)
                                  | otherwise                = filtr (pre ++ [(a0, b0)]) ((a1, b1):cs)
filtr pre [(a0, b0)] | fst (head pre) == a0 = filtr (init pre) (head pre: [(a0, b0)])
                     | otherwise            = pre ++ [(a0, b0)]

reduce = (filtr []) . canon