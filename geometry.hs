module Geometry where

import Vector

-- Ray definition
type Point = Vector
type Direction = Vector
type Time = Double
type Ray = (Point, Direction)

ray_mk :: Point -> Vector -> Ray
ray_mk p0 v = (p0, norm v)

ray_pos :: Ray -> Time -> Point
ray_pos (p0, d) t = p0 + scale d t

class (Eq a, Show a) => Shape a where
	intersect :: a -> Ray -> Maybe Double

-- Sphere center radius
data Sphere = Sphere Vector Double deriving (Eq, Show)
instance Shape Sphere where
	intersect (Sphere c r) (p0, d) 
		| null roots 				= Nothing
		| (head roots) >= 0			= Just (head roots)
		| (null . tail) roots		= Nothing
		| (head . tail) roots >= 0	= Just ((head . tail) roots)
		| otherwise					= Nothing
		where
			 roots = roots2 (sqrMag d) ((scale v 2) `dot` d) (sqrMag v - r*r)
			 v = p0 - c

roots2 :: (Ord a, Floating a) => a -> a -> a -> [a]
roots2 a b c 
	| discrim <  0 	= []
	| discrim == 0 	= [- b / (2 * a)]
	| otherwise		= [(-b - sqrt discrim) / (2 * a), (-b - sqrt discrim) / (2 * a)]
	where discrim = b*b - 4 * a * c

