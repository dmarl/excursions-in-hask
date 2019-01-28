{-# LANGUAGE OverloadedLists #-}
import Control.Monad
import Data.List
import Data.Map (Map)
import qualified Data.Map as M

{-
here we establish some //geometrical algebra// preliminaries with the intention of ultimately finding the resultant angle and axis of a composition of rotations in 3D
-}

type MultiVector = Map String Double 

--multiply two multivectors in any number of dimensions
gMult :: MultiVector -> MultiVector -> MultiVector
gMult x y = M.filter (/= 0) $ M.fromListWith (+) $ basisMult <$> M.assocs x <*> M.assocs y

basisMult :: (String, Double) -> (String, Double) -> (String, Double)
basisMult (s, m) (t, n) = gnome [] (s++t) (m*n)
    where
        gnome pre (a:b:rest) c
            | a < b  = gnome (pre ++ [a]) (b:rest) c
            | a == b = back pre rest c
            | a > b  = back pre (b:a:rest) (-c)
                where
                    back [] rest c = gnome [] rest c
                    back pre rest c = gnome (init pre) (last pre:rest) c
        gnome pre rest c = (pre ++ rest, c)

--canonises multivectors
canon = gMult iD

--returns orthogonal basis of Gn given orthonormal basis of Rn
canonical :: String -> [String]
canonical = subsequences

iD :: MultiVector
iD = [("", 1)]
v1 :: MultiVector
v1 = [("uvwuuv", 1)] 

v2 :: MultiVector
v2 = [("ac", 1), ("ab", 1), ("ba", 1)]

v3 :: MultiVector
v3 = [("e", 3), ("f", 4)]

v4 :: MultiVector
v4 = [("x", 1), ("y", 2), ("z", 3)]

v5 :: MultiVector
v5 = [("xyz", 1)]

v6 :: MultiVector
v6 = [("zyx", 1)]

--assumes reduced form
name :: MultiVector -> String
name x | null x = "zero multivector"
       | all ((==k ) . length) ss = show k ++ "-vector"
       | otherwise = "whole different box of frogs mate"
       where
           s:ss = M.keys x
           k = length s


--expresses an axis and angle (degs) as a rotor wrto orthonormal vectors x, y, z
toRotr :: [Double] -> Double -> MultiVector
toRotr axis theta = canon [("", c), ("yz", nx*s), ("zx", ny*s), ("xy", nz*s)]
    where
        c = cos (theta' / 2)
        s = sin (theta' / 2)
        [nx, ny, nz] = normalise axis
        theta' = pi * theta / 180

normalise :: [Double] -> [Double]
normalise vec = map (/ sqrt(sum(map (^2) vec))) vec

--extracts axis and angle of rotation from a rotor
fromRotr :: MultiVector -> ([Double], Double)
fromRotr m = (normalise [M.findWithDefault 0 "yz" m', -(M.findWithDefault 0 "xz" m'), M.findWithDefault 0 "xy" m'], acos(M.findWithDefault 0 "" m') * 360 / pi)
    where m' = canon m

--since rotations form a group in Rn we have that fromRotr $ gMult rotr1 rotr2 gives the axis/ angle of the composition rotr2 . rotr1

--entirely algebraic composition; case theta = 0 is degenerate
compose :: [Double] -> Double -> [Double] -> Double -> ([Double], Double)
compose axis1 theta axis2 phi = ([st*sp*(v'*w-v*w') + u*st*cp + u'*sp*ct,
                                  st*sp*(u*w'-u'*w) + v*st*cp + v'*sp*ct,
                                  st*sp*(u'*v-u*v') + w*st*cp + w'*sp*ct], 360*acos(ct*cp-st*sp*(u*u'+v*v'+w*w'))/pi)
                                      where
                                          theta' = pi*theta/180
                                          phi' = pi*phi/180
                                          ct = cos(theta'/2)
                                          cp = cos(phi'/2)
                                          st = sin(theta'/2)
                                          sp = sin(phi'/2)
                                          [u, v, w] = normalise axis1
                                          [u', v', w'] = normalise axis2

rotr1 = toRotr [1, 0, 0] 90
rotr2 = toRotr [0, 1, 0] 90
rotr3 = toRotr [0, 0, 1] 90

xs :: [Double]
xs = [1, 0, 0]
ys :: [Double]
ys = [0, 1, 0]
zs :: [Double]
zs = [0, 0, 1]

rotcomp r r' = fromRotr (gMult r r')