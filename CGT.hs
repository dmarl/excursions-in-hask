import Data.Char

firstWord = [('u', 1), ('v', -1), ('v', 1), ('u', -1)] :: [(Char, Int)]
secondWord = [('a', 1), ('a', 5), ('a', -2), ('a', -2)] :: [(Char, Int)]

canon ::[(Char, Int)] -> [(Char, Int)]
canon [] = []
canon (as@(a0, a1):bs) | a1 == 0 = canon bs
                        | otherwise  = as : canon bs

filtr :: [(Char, Int)] -> [(Char, Int)] -> [(Char, Int)]
filtr pre [] = pre
filtr [] [(a, b)] = [(a, b)]
filtr [] ((a0, b0):(a1, b1):cs) | a0 == a1 && b0 + b1 == 0  = filtr [] cs
                                 | a0 == a1 && b0 + b1 /= 0  = filtr [(a0, b0+b1)] cs
                                 | otherwise                 = filtr [(a0, b0)] ((a1, b1):cs)
filtr pre ((a0, b0):(a1, b1):cs) | a0 == a1 && b0 + b1 == 0 = filtr (init pre) (last(pre) : cs)
                                  | a0 == a1 && b0 + b1 /= 0 = filtr pre ((a0, b0 + b1):cs)
                                  | otherwise                = filtr (pre ++ [(a0, b0)]) ((a1, b1):cs)
filtr pre [(a0, b0)] | fst (head pre) == a0 = filtr (init pre) (head pre: [(a0, b0)])
                     | otherwise            = pre ++ [(a0, b0)]

reduce = (filtr []) . canon

howlong :: [(Char, Int)] -> Int
howlong [] = 0
howlong ((a, b):rest) = b + howlong rest

thislong :: [(Char, Int)] -> Int
thislong = howlong . reduce

oflength :: Int -> [[(Char, Int)]]
oflength 0 = [[]]
oflength 1 = [[('a', 1)], [('b', 1)], [('a', (-1))], [('b', (-1))]]
oflength k = (map (('a', 1):) (oflength (k-1))) ++ 
             (map (('b', 1):) (oflength (k-1))) ++ 
             (map (('a', (-1)):) (oflength (k-1))) ++ 
             (map (('b', (-1)):) (oflength (k-1)))

redlength :: Int -> [[(Char, Int)]]
redlength n | n == 0    = [[]]
            | otherwise = rmvdups $ filter (\x -> (lenof x == n)) (map reduce (oflength n))

lenof :: [(Char, Int)] -> Int
lenof [] = 0
lenof ((a, b):rest) = (abs b) + lenof rest

rmvdups :: Eq a => [a] -> [a]
rmvdups [] = []
rmvdups (a:bs) | a `elem` bs = rmvdups bs
               | otherwise   = a : rmvdups bs