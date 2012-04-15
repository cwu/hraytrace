module Geometry where
import Algebra

epsilon = 1e-4

data Shape = Sphere Point Double -- Sphere center radius
              deriving (Eq, Show)

intersect :: Ray -> Shape -> Maybe Intersection
intersect (Ray o d) (Sphere c r)
  | null positiveRoots = Nothing
  | otherwise          = Just (Intersection p (norm (-d)) t)
  where
    v             = o - c
    roots         = roots2 (sqrMag d) (scale 2 v `dot` d) (sqrMag v - r*r)
    positiveRoots = filter (>epsilon) roots
    t             = minimum roots
    p             = o + scale t d
  

roots2 :: (Ord a, Floating a) => a -> a -> a -> [a]
roots2 a b c
  | discrim < 0 = []
  | discrim == 0 = [- b / (2 * a)]
  | otherwise   = [(-b - sqrt discrim) / (2 * a), (-b - sqrt discrim) / (2 * a)]
  where
    discrim = b*b - 4*a*c
