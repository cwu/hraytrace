module Vector where

data Vector = V Double Double Double deriving (Eq, Show)

instance Num Vector where
	(V x y z) + (V a b c) = V (x+a) (y+b) (z+c)
	(V x y z) - (V a b c) = V (x-a) (y-b) (z-c)
	(V x y z) * (V a b c) = V x' y' z'
			where
				x' = y * c - z * b
				y' = z * a - x * c
				z' = x * b - y * a
	negate (V x y z) = V (-x) (-y) (-z)
	abs v = error "abs Vector not impelemented" 
	signum v = error "signum Vector not implemented" 
	fromInteger i =  error "fromInteger Vector not impelmented"

dot :: Vector -> Vector -> Double
dot (V x y z) (V a b c) = x * a + y * b + z * c

sqrMag :: Vector -> Double
sqrMag v = v `dot` v

mag :: Vector -> Double
mag = sqrt . sqrMag

projMag :: Vector -> Vector -> Double
projMag v u = (v `dot` u) / mag u

scale :: Vector -> Double -> Vector
scale (V x y z) s = V (x*s) (y*s) (z*s)

norm :: Vector -> Vector
norm (V 0 0 0) = V 0 0 0
norm v = scale v (1 / mag v)

proj :: Vector -> Vector -> Vector
proj v u = scale u ((v `dot` u) / sqrMag u)

i = V 1 0 0
j = V 0 1 0
k = V 0 0 1
