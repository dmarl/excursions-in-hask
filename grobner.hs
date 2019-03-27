import Data.Char

data Monom = M (Float, [Int])
    deriving (Show, Eq)

monomax :: Monom -> Monom -> Monom
monomax (M (a, as)) (M (b, bs)) |   as < bs = M (b, bs)
                                | otherwise = M (a, as)
leadterm :: [Monom] -> Monom
leadterm [] = M (0, [])
leadterm ((lead:rest)) = monomax lead (leadterm (rest))

monodiv :: Monom -> Monom -> Bool
monodiv (M(a, as)) (M(b, bs)) = foldr (&&) True $ zipWith (<=) as bs

doesdiv :: Monom -> [Monom] -> Bool
doesdiv mono monos = foldr (||) False [mano `monodiv` mono | mano <- monos]

{-whichmono :: Monom -> [Monom] -> Monom
whichmono mono (mono':rest) | mono' `monodiv` mono = mono'
                            | otherwise = whichmono mono rest   -}                             

insert :: [a] -> [a] -> [a]
insert sublist list = take ((length list) - length (sublist)) list ++ sublist

monolist :: [[Monom]] -> [Monom]
monolist polys = [leadterm p | p <- polys]

reducepoly :: [Monom] -> [Monom]
reducepoly [] = []
reducepoly (M (c, mono): rest) |    c == 0 = reducepoly rest
                               | otherwise = (M (c, mono)) : reducepoly rest

addmono :: Monom -> Monom -> [Monom]
addmono m1@(M(a, as)) m2@(M(b, bs)) | as == bs  = [M (a+b, as)]
                                    | otherwise = [m1, m2]
                                        
monomult :: Monom -> Monom -> Monom
monomult (M(a, as)) (M(b, bs)) = M(a*b, zipWith (+) as bs)

divmono :: Monom -> Monom -> Monom
divmono (M(a, as)) (M(b, bs)) = M(a/b, zipWith (-) as bs)

polyadd :: [Monom] -> [Monom] -> [Monom]
polyadd p1 p2 = polyred $ polyord (p1 ++ p2)

polymin :: [Monom] -> [Monom] -> [Monom]
polymin p1 p2 = polyadd p1 (polyscale (-1) p2) 

unwrap :: Monom -> [Int]
unwrap (M(a, as)) = as

polymult :: [Monom] -> [Monom] -> [Monom]
polymult _ [] = []
polymult [] _ = []
polymult p1 p2 = polyred $ monomult <$> p1 <*> p2

polyord :: [Monom] -> [Monom]
polyord [] = []
polyord (mono:mials) = polyord (filter (\mon -> (unwrap mon) <= (unwrap mono)) mials) ++ [mono]
                    ++ polyord (filter (\mon -> (unwrap mon) > (unwrap mono)) mials)

monoscale :: Float -> Monom -> Monom
monoscale lambda (M(a, as)) = M(a*lambda, as)

polyscale :: Float -> [Monom]  -> [Monom]
polyscale lambda p = map (monoscale lambda) p

polyred :: [Monom] -> [Monom]
polyred [] = []
polyred [M(coeff, mono)] | coeff == 0 = []
                         | otherwise  = [M(coeff, mono)]
polyred ((M(a,as)):(M(b,bs)):rest) | a == 0              = polyred((M(b,bs)):rest)
                                   | as == bs            = polyred ((M(a+b, as)):rest)
                                   | otherwise           = (M(a, as)):polyred((M(b,bs)):rest)

whichmono :: [Monom] -> [[Monom]] -> [[Monom]] -> ([Monom], [[Monom]])
whichmono s (p:olys) (c:oeffs) | (monodiv p' s') = (reDuce $ polymin s (polymult p [divmono s' p']), [reDuce $ polyadd c [divmono s' p']] ++ oeffs)
                                | otherwise       = whichmono s olys oeffs
                                    where p' = leadterm p
                                          s' = leadterm s

reDuce = polyred . polyord

grobDiv :: [Monom] -> [Monom] -> [[Monom]] -> [[Monom]] -> ([[Monom]], [Monom])
grobDiv r [] polys coeffs = (coeffs, r)
grobDiv r s polys coeffs | doesdiv s' leads = grobDiv r s'' polys coeffs'
                             | otherwise = grobDiv (reDuce $ polyadd r [s']) (reDuce $ polyadd s (polyscale (-1) [s'])) polys coeffs
                                 where s' = leadterm s
                                       leads = [leadterm poly | poly <- polys]
                                       s'' = reDuce $ fst (whichmono s polys coeffs)
                                       coeffs' = insert (snd (whichmono s polys coeffs)) coeffs

grobDivision r s polys coeffs = grobDiv (reDuce r) (reDuce s) (map reDuce polys) (map reDuce coeffs)

lCm :: Monom -> Monom -> Monom
lCm (M(a, as)) (M(b, bs)) = M(1, cs)
    where
        cs = [max a b | (a, b) <- zip as bs]

sPoly :: [Monom] -> [Monom] -> [Monom]
sPoly f g = polymin (polymult [divmono monom f'] f) (polymult [divmono monom g'] g)
    where
        monom = lCm f' g'
        f' = leadterm f
        g' = leadterm g

polycomp :: [Monom] -> [Monom] -> Bool
polycomp [] [] = True
polycomp [] _ = True
polycomp _ [] = False
polycomp f g | f' < g' = True
             | f' > g' = False
             | otherwise = polycomp f'' g''
                 where
                     f' = unwrap $ leadterm f
                     g' = unwrap $ leadterm g
                     f'' = polymin f [leadterm f]
                     g'' = polymin g [leadterm g]


ordList :: [[Monom]] -> [[Monom]]
ordList [] = []
ordList (f:fs) = (filter (\p -> polycomp p f) fs) ++ [f] ++ (filter (polycomp f) fs)


buchBerg :: [[Monom]] -> [[Monom]]
buchBerg polys | polys /= polys'                = buchBerg polys' 
               | (filter (/= []) rmnders) == [] = map reDuce polys
               | otherwise                      = buchBerg (map reDuce (polys ++ (filter (/= []) rmnders)))
                   where
                       rmnders = [snd $ grobDivision [] (sPoly f g) polys [[] | poly <- polys] | f <- polys', g <- polys', polycomp f g]
                       polys' = ordList $ map reDuce polys

monoList :: [[Monom]] -> [Monom] -> [Monom]
monoList polys poly = [leadterm poly' | poly' <- polys, poly' /= poly]

minGrob :: [[Monom]] -> [[Monom]]
minGrob basis = [poly | poly <- basis, doesdiv (leadterm poly) (monoList basis poly) == False] 

p1 = [M (1, [1, 0, 0]), M (1, [0, 1, 0]), M (1,  [0, 0, 1])] :: [Monom]
p2 = [M (1, [1, 1, 0]), M (1, [0, 1, 1]), M (1,  [1, 0, 1])] :: [Monom]
p3 = [M (1, [2, 0, 0]), M (1, [0, 2, 0]), M (1,  [0, 0, 2])] :: [Monom]
p4 = [M (1, [1, 0, 0]), M (0, [0, 1, 0]), M (1,  [0, 0, 1]), M (1, [1, 0, 0])] :: [Monom]
f = [M (1, [4, 0]), M (1, [0, 4])] :: [Monom]
f1 = [M (1, [2, 0]), M (1, [0, 1])] :: [Monom]
f2 = [M (1, [2, 1]), M (1, [0, 0])] :: [Monom]
g1 = [M (1, [0,0,1,0]), M(-1, [1,0,0,0]), M(-1, [0,1,0,0])] :: [Monom]
g2 = [M (1, [0,0,0, 1]), M(-1, [1,1,0,0])] :: [Monom]
xplusy = [M (1, [1, 0, 0]), M (1, [0, 1, 0])] :: [Monom]
xminy = [M (1, [1, 0, 0]), M (-1, [0, 1, 0])] :: [Monom]

ps = [p1, p2, p3]


m1 = M (1, [1, 0, 0]) :: Monom
m2 = M (1, [2, 1, 0]) :: Monom
m3 = M (1, [1, 1, 2]) :: Monom

interp :: Int -> Int -> [Int] -> [Int]
interp n k perm = (take (k-1) perm) ++ [n] ++ (drop (k-1) perm)

perms :: Int -> [[Int]]
perms 1 = [[1]]
perms n = [interp n k perm | k <- [1..n], perm <- (perms (n-1))]
    
minor :: Num a => [[a]] -> Int -> [[a]]
minor [[x]] _ = []
minor m k = [(take (k-1) row) ++ (drop k row) | row <- tail m]

{-gendiv :: ([Float], [[Int]]) -> ([Float], [[Int]]) -> [([Float], [[Int]])] -> (Float, ([Float], [[Int]]))
gendiv [] [] polys = (0, (0, []))
gendiv [coeffs1, monos1] [coeffs2, monos2] [polys] | 

defn: monom
monomax :: monom -> monom -> monom
leadmon :: [monom] -> monom
monodiv :: monom -> monom -> Bool
doesdiv :: monom -> [monom] -> Bool
whichmono :: monom -> [monom] -> monom
reducepoly :: [monom] -> [monom]
addmono :: monom -> monom -> monom
multmono :: monom -> monom -> monom
addpoly :: [monom] -> [monom] -> [monom]
multpoly :: [monom] -> [monom] -> [monom] -}