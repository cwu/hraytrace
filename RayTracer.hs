module RayTracer where

import Vector
import Geometry

type Camera = (Point, Vector, Vector)
type PixelRay = (Integer, Integer, Ray)
type Screen = (Integer, Integer, Double, Double)

mkcamera :: Point -> Vector -> Vector -> Camera
mkcamera p dir up = (p, norm dir, norm up)

mkpixelrays :: Camera -> Screen -> [PixelRay]
mkpixelrays (pos, dir, up) (width, height, dist, aov) = let
		centre = pos + scale dist dir
		d = - dir `dot` centre
		down = scale 0.3 $ norm $ up `proj` dir - up
		right = scale 0.3 $ norm $ down * dir 

		v = - (scale (fromInteger (width-1) / 2) right) - (scale (fromInteger (height-1) / 2) down)
		topLeft = centre + v

		getPixelPos :: Integer -> Integer -> Point
		getPixelPos r c = topLeft + scale (fromInteger r) down + scale (fromInteger c) right
	in [(r, c, mkray pos (getPixelPos r c))	| r <- [0..height-1], c <- [0..width-1]]


type Color = (Double, Double, Double)
red, green, blue, white, black, nearlywhite :: Color
red = (1,0,0)
green = (0,1,0)
blue = (0,0,1)
white = (1,1,1)
black = (0,0,0)
nearlywhite = (0.8, 0.8, 0.8)

background_color = black

scale_col :: Double -> Color -> Color
scale_col k (r,g,b) = (r*k, g*k, b*k)

add_col :: Color -> Color -> Color
add_col (r1,g1,b1) (r2,g2,b2) = (r1+r2, g1+g2, b1+b2)

clamp :: Color -> Color
clamp (r,g,b) = ( clampdouble r, clampdouble g, clampdouble b)
                where clampdouble = max 0.0 . min 1.0

combine_col :: Color -> Color -> Color
combine_col (r1,g1,b1) (r2,g2,b2) = (r1*r2, g1*g2, b1*b2)

type ColorRGB = (Int, Int, Int)
toRGB :: Color -> ColorRGB
toRGB (r,g,b) = (floor (r * 255), floor (g * 255), floor (b * 255))

type Object = (Shape, Color)
type LightSource = (Point,Color)
type World = ([Object], [LightSource])

trace :: World -> Ray -> Color
trace ([],_) _ = background_color
trace (_,[]) _ = background_color
trace world@(objects,lights) ray  = let
		intersections :: [(Intersection, Color)]
		intersections = concat $ map (\(s,c) -> zip (intersect ray s) (repeat c) ) objects
	in if null intersections then background_color
		else let
				closestObj :: [(Intersection, Color)] -> (Intersection, Color)
				closestObj = foldl1 minObjDist 
					where minObjDist x@((_,t1),_) y@((_,t2),_) = if t1 < t2 then x else y

				((s, t), c) = closestObj intersections
				p = ray_pos ray t
			in lighting world (s,c) p

lighting :: World -> Object -> Point ->  Color
lighting (objs, lights) (Sphere centre radius,c) p = clamp $ scale_col brightness c
	where
		normal = scale (1/radius) (p - centre)
		brightness = sum $ map (\ray -> (normal `dot` ray) / (mag ray)) tolights
		tolights = map (\(lightPos, _) -> lightPos - p) lights
		

