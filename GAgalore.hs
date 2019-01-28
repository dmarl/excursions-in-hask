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

