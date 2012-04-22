{-# LANGUAGE ForeignFunctionInterface #-}
module Geometry where
import Algebra

import Debug.Trace

epsilon = 1e-4

data Shape = Sphere Point Double -- Sphere center radius
           | Cube Point Double   -- Cube position size
           | Plane Point Vector  -- Plane point normal
           deriving (Eq, Show)

intersect :: Ray -> Shape -> Maybe Intersection
intersect (Ray o d) (Sphere c r)
  | null positiveRoots = Nothing
  | otherwise          = Just (Intersection p (norm (p-c)) t)
  where
    v             = o - c
    roots         = roots2 (sqrMag d) (scale 2 v `dot` d) (sqrMag v - r*r)
    positiveRoots = filter (>epsilon) roots
    t             = minimum roots
    p             = o + scale t d

intersect (Ray o@(V ox oy oz) d@(V dx dy dz)) (Cube c@(V cx cy cz) s)
  | null mins || null maxes                                   = Nothing
  | tmax < epsilon                                            = Nothing
  | tmin > tmax                                               = Nothing
  | not $ isPointInsideBox offsetP c (V (cx+s) (cy+s) (cz+s)) = Nothing
  | otherwise                                                 = Just (Intersection p nmin tmin)
  where
    inf             = 1/0
    dist a b speed  = (b - a)/speed
    (tx1, ty1, tz1) = (dist ox cx dx, dist oy cy dy, dist oz cz dz)
    (tx2, ty2, tz2) = (dist ox (cx+s) dx, dist oy (cy+s) dy, dist oz (cz+s) dz)
    (xmin, xmax)    = if tx1 < tx2 then ((tx1, V (-1) 0 0), (tx2, V 1 0 0)) else ((tx2, V 1 0 0), (tx1, V (-1) 0 0))
    (ymin, ymax)    = if ty1 < ty2 then ((ty1, V 0 (-1) 0), (ty2, V 0 1 0)) else ((ty2, V 0 1 0), (ty1, V 0 (-1) 0))
    (zmin, zmax)    = if tz1 < tz2 then ((tz1, V 0 0 (-1)), (tz2, V 0 0 1)) else ((tz2, V 0 0 1), (tz1, V 0 0 (-1)))
    mins            = filter (\(t, _) -> t < inf && t > (-inf)) [xmin, ymin, zmin]
    maxes           = filter (\(t, _) -> t < inf && t > (-inf)) [xmax, ymax, zmax]
    (tmin, nmin)    = maximum mins
    (tmax, nmax)    = minimum maxes
    p               = o + scale tmin d
    offsetP         = p + scale epsilon d

intersect (Ray o d) (Plane p0 n)
  | directToPlane `dot` d < epsilon = Nothing
  | otherwise                       = Just (Intersection p intersectNorm dist)
  where
    toPlaneAlongRay = scale (abs (mag directToPlane / d `dot` n)) d
    directToPlane   = proj (p0 - o) n
    p               = o + toPlaneAlongRay
    dist            = mag toPlaneAlongRay
    intersectNorm   = norm (-directToPlane)

isPointInsideBox :: Point -> Point -> Point -> Bool
isPointInsideBox (V px py pz) (V minx miny minz) (V maxx maxy maxz) =
  px >= minx && px <= maxx && py >= miny && py <= maxy && pz >= minz && pz <= maxz

roots2 :: Double -> Double -> Double -> [Double]
roots2 a b c
  | discrim < 0  = []
  | discrim == 0 = [- b / (2 * a)]
  | otherwise    = [(-b - c_sqrt discrim) / (2 * a), (-b - c_sqrt discrim) / (2 * a)]
  where
    discrim = b*b - 4*a*c

foreign import ccall unsafe "math.h sqrt"
    c_sqrt :: Double -> Double
