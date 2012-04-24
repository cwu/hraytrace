{-# LANGUAGE ForeignFunctionInterface #-}

module Geometry where

import Data.Maybe (isJust)
import Algebra

import Debug.Trace

epsilon = 1e-4

data Face  = Face [Point] Vector -- Face points normal
           deriving (Eq, Show)
data Shape = Sphere Point Double -- Sphere center radius
           | Cube Point Double   -- Cube position size
           | Plane Point Vector  -- Plane point normal
           | Polygon [Face]      -- Polygon faces
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
    (i, j, k)       = (V 1 0 0, V 0 1 0, V 0 0 1)
    dist a b speed  = (b - a)/speed
    (tx1, ty1, tz1) = (dist ox cx dx, dist oy cy dy, dist oz cz dz)
    (tx2, ty2, tz2) = (dist ox (cx+s) dx, dist oy (cy+s) dy, dist oz (cz+s) dz)
    (xmin, xmax)    = if tx1 < tx2 then ((tx1, -i), (tx2, i)) else ((tx2, i), (tx1, -i))
    (ymin, ymax)    = if ty1 < ty2 then ((ty1, -j), (ty2, j)) else ((ty2, j), (ty1, -j))
    (zmin, zmax)    = if tz1 < tz2 then ((tz1, -k), (tz2, k)) else ((tz2, k), (tz1, -k))
    mins            = filter (not . isInf . fst) [xmin, ymin, zmin]
    maxes           = filter (not . isInf . fst) [xmax, ymax, zmax]
    isInf x         = abs x == 1/0
    (tmin, nmin)    = maximum mins
    (tmax, nmax)    = minimum maxes
    p               = o + scale tmin d
    offsetP         = p + scale epsilon d
    isPointInsideBox (V px py pz) (V minx miny minz) (V maxx maxy maxz) =
      px >= minx && px <= maxx && py >= miny && py <= maxy && pz >= minz && pz <= maxz

intersect (Ray o d) (Plane p0 n)
  | directToPlane `dot` d < epsilon = Nothing
  | otherwise                       = Just (Intersection p intersectNorm dist)
  where
    toPlaneAlongRay = scale (abs (mag directToPlane / d `dot` n)) d
    directToPlane   = proj (p0 - o) n
    p               = o + toPlaneAlongRay
    dist            = mag toPlaneAlongRay
    intersectNorm   = norm (-directToPlane)

intersect ray (Polygon faces)
  | null justIntersections = Nothing
  | otherwise              = minimum justIntersections
  where
    justIntersections = filter isJust $ map (intersectFace ray) faces

intersectFace :: Ray -> Face -> Maybe Intersection
intersectFace (Ray o d) face@(Face points@(p0 : _ ) n)
  | directToPlane `dot` d < epsilon = Nothing
  | not $ isPointInsideFace p face  = Nothing
  | otherwise                       = Just (Intersection p intersectNorm dist)
  where
    toPlaneAlongRay = scale (abs (mag directToPlane / d `dot` n)) d
    directToPlane   = proj (p0 - o) n
    p               = o + toPlaneAlongRay
    dist            = mag toPlaneAlongRay
    intersectNorm   = norm (-directToPlane)

-- | isPointInsideFace checks if a point is inside of a face. A point along
-- the edge of the face is considered NOT inside. this produces an edge case
-- where the rotatingVectors will be opposite directions and the cross product
-- will be 0.
--
-- assumes the list of points that make up the face are in a plane
isPointInsideFace :: Point -> Face -> Bool
isPointInsideFace p (Face points@(p0 : ps) n) = consistentSigns directionSigns
  where
    consistentSigns []           = True
    consistentSigns [x]          = True
    consistentSigns (x:y:xs)     = signum x == signum y && consistentSigns (y:xs)
    makeRotatingVectors (p1, p2) = (p1 - p, p2 - p)
    rotatingVectors              = map makeRotatingVectors $ zip points (ps ++ [p0])
    crosses                      = map (uncurry cross) rotatingVectors
    directionSigns               = map (dot n) crosses

roots2 :: Double -> Double -> Double -> [Double]
roots2 a b c
  | discrim < 0  = []
  | discrim == 0 = [- b / (2 * a)]
  | otherwise    = [(-b - c_sqrt discrim) / (2 * a), (-b - c_sqrt discrim) / (2 * a)]
  where
    discrim = b*b - 4*a*c

foreign import ccall unsafe "math.h sqrt"
    c_sqrt :: Double -> Double
