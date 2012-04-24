{-# LANGUAGE ForeignFunctionInterface #-}

module Algebra where

data Vector = V !Double !Double !Double deriving (Eq, Show)
type Point = Vector

instance Num Vector where
  (V x y z) + (V a b c) = V (x+a) (y+b) (z+c)
  (V x y z) - (V a b c) = V (x-a) (y-b) (z-c)
  (*)                   = cross
  abs _                 = error "abs Vector not impelemented"
  signum                = error "signum not implemented"
  fromInteger x         = V (fromInteger x) (fromInteger x) (fromInteger x)

instance Ord Vector where
  compare v1 v2 = compare (mag v1) (mag v2)

dot :: Vector -> Vector -> Double
dot (V x y z) (V a b c) = x * a + y * b + z * c

cross :: Vector -> Vector -> Vector
cross (V x y z) (V a b c) =  V (y * c - z * b) (z * a - x * c) (x * b - y * a)

sqrMag :: Vector -> Double
sqrMag v = v `dot` v

mag :: Vector -> Double
mag = sqrt . sqrMag

scale :: Double -> Vector -> Vector
scale s (V x y z) = V (x*s) (y*s) (z*s)

plusScalar :: Double -> Vector -> Vector
plusScalar s (V x y z) = V (x+s) (y+s) (z+s)

norm :: Vector -> Vector
norm (V 0 0 0) = V 0 0 0
norm v         = scale (1 / mag v) v

proj :: Vector -> Vector -> Vector
proj v u = scale ((v `dot` u) / sqrMag u) u

-- Ray start direction
data Ray = Ray Point Vector deriving (Eq, Show)

-- Intersection intersectPoint normal distance
data Intersection = Intersection Point Vector Double deriving (Eq, Show)

instance Ord Intersection where
  compare (Intersection _ _ t1) (Intersection _ _ t2) = compare t1 t2

-- Color r g b
data Color = Color Double Double Double deriving (Eq, Show)

scaleColor :: Double -> Color -> Color
scaleColor s (Color r g b) = Color (r*s) (g*s) (b*s)

clampColor (Color r g b) = Color (clamp r) (clamp g) (clamp b)
  where
    clamp = min 1 . max 0

type ColorRGB = (Int, Int, Int)
toRGB :: Color -> ColorRGB
toRGB (Color r g b) = (floor' (r * 255), floor' (g * 255), floor' (b * 255))

floor' :: Double -> Int
floor' x = (fromIntegral . (truncate :: Double -> Int)) $ c_floor x

foreign import ccall unsafe "math.h floor"
  c_floor :: Double -> Double

instance Num Color where
  (Color r1 g1 b1) + (Color r2 g2 b2) = Color (r1+r2) (g1+g2) (b1+b2)
  (Color r1 g1 b1) - (Color r2 g2 b2) = Color (r1-r2) (g1-g2) (b1-b2)
  (Color r1 g1 b1) * (Color r2 g2 b2) = Color (r1*r2) (g1*g2) (b1*b2)
  abs (Color r g b)                   = Color (abs r) (abs g) (abs b)
  signum _                            = error "signum Color not implemented"
  fromInteger x                       = Color (fromIntegral x) (fromIntegral x) (fromIntegral x)
