module Geometry where

import Vector

-- Ray definition
type Point = Vector
type Direction = Vector
type Time = Double
type Ray = (Point, Direction)

mkray :: Point -> Point -> Ray
mkray from to = (from, norm (to - from) )


ray_pos :: Ray -> Time -> Point
ray_pos (p0, d) t = p0 + scale t d

data Shape = Sphere Vector Double | Plane Vector Double deriving (Eq, Show)
				
mkplane :: Point -> Point -> Point -> Maybe Shape
mkplane p0 p1 p2
	| cross == 0 	= Nothing
	| otherwise		= Just $ Plane n d
	where
		(v,w) = (p1 - p0, p2 - p0)
		cross  = v * w
		n = norm cross
		d = -p0 `dot` n

epsilon :: Double
epsilon = 0.0001

type Intersection = (Shape, Time)
intersect :: Ray -> Shape -> [Intersection]
intersect (p0, d) s@(Sphere c r)
	| null positiveRoots		= []
	| otherwise					= map (\x -> (s, x)) positiveRoots
	where 
		v = p0 - c
		roots = roots2 (sqrMag d) ((scale 2 v) `dot` d) (sqrMag v - r*r) 
		positiveRoots = filter (>epsilon) roots 

intersect (p0, dir) s@(Plane n d)
	| num /= 0 && den /= 0 && num / den > epsilon 	= [(s, num / den)]
	| otherwise 									= [] 
	where (num,den) = (-d - p0 `dot` n, dir `dot` n)

closest :: [(Shape, Time)] -> (Shape, Time)
closest xs = foldl1 (\(s1,t1) (s2,t2) -> if t1 < t2 then (s1,t1) else (s2,t2)) xs

normal :: Shape -> Point -> Vector
normal (Sphere c _) p = p - c

{-reflect :: Shape -> Ray -> Maybe Ray
reflect s r@(_,d) = case x@(intersect s r) of 
						[] 		  -> Nothing
						otherwise -> let 
										t = snd 
										p' = ray_pos r t
										n  = normal s p'
										d' = d - scale 2 (proj d n)
									in Just (mkray p' d')
-}


roots2 :: (Ord a, Floating a) => a -> a -> a -> [a]
roots2 a b c 
	| discrim <  0 	= []
	| discrim == 0 	= [- b / (2 * a)]
	| otherwise		= [(-b - sqrt discrim) / (2 * a), (-b - sqrt discrim) / (2 * a)]
	where discrim = b*b - 4 * a * c

