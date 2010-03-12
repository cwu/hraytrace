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
ray_pos (p0, d) t = p0 + scale t d

class (Eq a, Show a) => Shape a where
	intersect :: a -> Ray -> Maybe Time
	normal :: a -> Point -> Vector
	reflect :: a -> Ray -> Maybe Ray

	reflect s r@(_,d) = case intersect s r of 
							Nothing -> Nothing
							Just t 	-> let 
											p' = ray_pos r t
											n  = normal s p'
											d' = d - scale 2 (proj d n)
										in Just (ray_mk p' d')

-- Sphere center radius
data Sphere = Sphere Vector Double deriving (Eq, Show)
instance Shape Sphere where
	intersect (Sphere c r) (p0, d) 
		| null positiveRoots		= Nothing
		| otherwise					= Just (foldl1 min positiveRoots)
		where 
			v = p0 - c
			roots = roots2 (sqrMag d) ((scale 2 v) `dot` d) (sqrMag v - r*r) 
			positiveRoots = filter (>=0) roots 
	normal (Sphere c _) p = p - c
	

roots2 :: (Ord a, Floating a) => a -> a -> a -> [a]
roots2 a b c 
	| discrim <  0 	= []
	| discrim == 0 	= [- b / (2 * a)]
	| otherwise		= [(-b - sqrt discrim) / (2 * a), (-b - sqrt discrim) / (2 * a)]
	where discrim = b*b - 4 * a * c

