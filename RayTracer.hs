module RayTracer where

import Vector
import Geometry

type Camera = (Point, Vector, Vector)
type PixelRay = (Integer, Integer, Ray, Ray, Ray)
type Screen = (Integer, Integer, Double, Double)

mkcamera :: Point -> Vector -> Vector -> Camera
mkcamera p dir up = (p, norm dir, norm up)

mkpixelrays :: Camera -> Screen -> [PixelRay]
mkpixelrays (pos, dir, up) (width, height, dist, pixelSize) = let
    centre = pos + scale dist dir
    d = - dir `dot` centre
    down = scale pixelSize $ norm $ up `proj` dir - up
    right = scale pixelSize $ norm $ down * dir

    v = - (scale (fromInteger (width-1) / 2) right) - (scale (fromInteger (height-1) / 2) down)
    topLeft = centre + v

    pixelPos :: Integer -> Integer -> Point
    pixelPos r c = topLeft + scale (fromInteger r) down + scale (fromInteger c) right
  in [(r, c,
    mkray pos (pixelPos r c), mkray pos (pixelPos (r+1) c), mkray pos (pixelPos r (c+1)))
      | r <- [0..height-1], c <- [0..width-1]]


type Color = (Double, Double, Double)
red, green, blue, white, black, nearlywhite :: Color
red = (1,0,0)
green = (0,1,0)
blue = (0,0,1)
white = (1,1,1)
black = (0,0,0)
nearlywhite = (0.8, 0.8, 0.8)

backgroundColor = black

scaleCol :: Double -> Color -> Color
scaleCol k (r,g,b) = (r*k, g*k, b*k)

addCol :: Color -> Color -> Color
addCol (r1,g1,b1) (r2,g2,b2) = (r1+r2, g1+g2, b1+b2)

clamp :: Color -> Color
clamp (r,g,b) = ( clampdouble r, clampdouble g, clampdouble b)
                where clampdouble = max 0.0 . min 1.0

combineCol :: Color -> Color -> Color
combineCol (r1,g1,b1) (r2,g2,b2) = (r1*r2, g1*g2, b1*b2)

type ColorRGB = (Int, Int, Int)
toRGB :: Color -> ColorRGB
toRGB (r,g,b) = (floor (r * 255), floor (g * 255), floor (b * 255))

data AliasType = NoAlias | SuperSample deriving (Eq, Show)
type Object = (Shape, Color)
type LightSource = (Point,Color)
type World = ([Object], [LightSource])

trace :: World -> Ray -> Color
trace ([],_) _ = backgroundColor
trace (_,[]) _ = backgroundColor
trace world@(objects,lights) ray  = let
    intersections :: [(Intersection, Color)]
    intersections = concatMap (\(s,c) -> zip (ray `intersect` s) (repeat c) ) objects
  in if null intersections then backgroundColor
    else let
        closestObj :: [(Intersection, Color)] -> (Intersection, Color)
        closestObj = foldl1 minObjDist
          where minObjDist x@((_,t1),_) y@((_,t2),_) = if t1 < t2 then x else y

        ((s, t), c) = closestObj intersections
        p = rayPos ray t
      in lighting world (s,c) p

lighting :: World -> Object -> Point ->  Color
lighting (objs, lights) (Sphere centre radius,c) p = clamp $ scaleCol brightness c
  where
    normal = scale (1/radius) (p - centre)
    brightness = sum $ map (\ray -> (normal `dot` ray) / mag ray) tolights
    tolights = map (\(lightPos, _) -> lightPos - p) lights


render :: World -> PixelRay -> Color
render world (_, _, (p,topLeft), (_,bottomLeft), (_,topRight)) = let
    ndiv = 3
    div = 1 / ndiv
    down = scale div $ bottomLeft - topLeft
    right = scale div $ topRight - topLeft

    raylets = [(p, topLeft + scale r down + scale c right) | r <- [1..ndiv], c <-[1..ndiv]]
  in scaleCol (1/(ndiv * ndiv)) $ foldl (\acc -> addCol acc . trace world) (0,0,0) raylets

