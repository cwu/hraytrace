module Algebra where

data Vector = V !Double !Double !Double deriving (Eq, Show)
type Point = Vector

instance Num Vector where
  (V x y z) + (V a b c) = V (x+a) (y+b) (z+c)
  (V x y z) - (V a b c) = V (x-a) (y-b) (z-c)
  (V x y z) * (V a b c) = V (y * c - z * b) (z * a - x * c) (x * b - y * a)
  negate        = scale (-1)
  abs _         = error "abs Vector not impelemented"
  signum _      = error "signum Vector not implemented"
  fromInteger _ = error "fromInteger Vector not impelmented"

dot :: Vector -> Vector -> Double
dot (V x y z) (V a b c) = x * a + y * b + z * c

sqrMag :: Vector -> Double
sqrMag v = v `dot` v

mag :: Vector -> Double
mag = sqrt . sqrMag

projMag :: Vector -> Vector -> Double
projMag v u = (v `dot` u) / mag u

scale :: Double -> Vector -> Vector
scale s (V x y z) = V (x*s) (y*s) (z*s)

norm :: Vector -> Vector
norm (V 0 0 0) = V 0 0 0
norm v = scale (1 / mag v) v

proj :: Vector -> Vector -> Vector
proj v u = scale ((v `dot` u) / sqrMag u) u


-- Ray start direction
data Ray = Ray Point Vector deriving (Eq, Show)


-- Intersection intersectPoint normal distance
data Intersection = Intersection Point Vector Double deriving (Eq, Show)
