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

