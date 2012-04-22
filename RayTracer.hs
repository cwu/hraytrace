module RayTracer where

import Data.Maybe (isNothing, isJust)
import Geometry
import Algebra
import Object
import Light

-- Scene objects
data Scene = Scene [Object] deriving (Eq, Show)

-- World scene lights ambient
data World = World Scene [Light] Color deriving (Eq, Show)

-- Screen width height fov resolution
data Screen = Screen Double Double Double Double deriving (Eq, Show)

-- Camera position viewVector upVector
data Camera = Camera Point Vector Vector deriving (Eq, Show)

calculateRay :: Screen -> Camera -> Double -> Double -> Ray
calculateRay (Screen w h fov _) (Camera cp view up) x y =
  Ray cp (norm dir)
  where
    side     = view * up
    distance = (h / 2) / tan (fov / 2)
    dirSide  = scale (x - w / 2) side
    dirUp    = scale (y - h / 2) up
    dirView  = scale distance view
    dir      = dirSide + dirUp + dirView

getRays :: Screen -> Camera -> [[Ray]]
getRays screen@(Screen w h _ resolution) camera =
  map (map getRay . dividePixel) pixels
  where
    getRay             = uncurry (calculateRay screen camera)
    sqrtNumSamples     = ceiling $ 1 / resolution
    resolution'        = 1 / fromIntegral sqrtNumSamples
    lower              = - sqrtNumSamples `div` 2
    upper              = sqrtNumSamples `div` 2
    dividePixel (x, y) = [(x + fromIntegral x' * resolution',
                           y + fromIntegral y' * resolution') |
                          y' <- [lower..upper], x' <- [lower..upper]]
    pixels             = [(x,h-y-1) | y <- [0..h-1], x <- [0..w-1]]

render :: World -> Screen -> Camera -> [Color]
render world screen camera@(Camera eye _ _) =
  map (clampColor . avg . map rayTracer) $ getRays screen camera
  where
    rayTracer = rayTrace world eye
    avg xs = scaleColor (1 / fromIntegral (length xs)) (sum xs)

rayTrace :: World -> Point -> Ray -> Color
rayTrace (World (Scene objects) lights ambient) eye ray
  | isNothing closest = ambient
  | otherwise         = ambient * kd + diffuse + specular
  where
    closest                      = closestIntersection objects ray
    Just (intersection, object)  = closest
    (diffuse, specular)          = foldl1 sumTuple2 diffuseAndSpecularColors
    diffuseAndSpecularColors     = map (calculateColor intersection object eye objects) lights
    calculateColor'              = calculateColor intersection object eye objects
    (Object _ (Material kd _ _)) = object
    sumTuple2 (f1, s1) (f2, s2)  = (f1 + f2, s1 + s2)

calculateColor :: Intersection -> Object -> Point -> [Object] -> Light ->
                  (Color, Color)
calculateColor (Intersection p n t) (Object _ (Material kd ks shininess))
               eye objects (Light lightPos ld ls)
  | isJust occ                     = (Color 0 0 0 , Color 0 0 0)
  | n `dot` l > 0 && r `dot` v > 0 = (diffuse     , specular)
  | n `dot` l > 0                  = (diffuse     , Color 0 0 0)
  | r `dot` v > 0                  = (Color 0 0 0 , specular)
  | otherwise                      = (Color 0 0 0 , Color 0 0 0)
  where
    occ      = closestIntersection objects (Ray p l)
    l        = norm $ lightPos - p
    r        = (-l) + scale (2*(l `dot` n)) n
    v        = norm $ eye - p
    diffuse  = ld * scaleColor (l `dot` n) kd
    specular = ls * scaleColor ((r `dot` v)**shininess) ks

closestIntersection :: [Object] -> Ray -> Maybe (Intersection, Object)
closestIntersection objects ray
  | null objects       = Nothing
  | null intersections = Nothing
  | otherwise          = Just (intersection, object)
  where
    intersections                            = filter (isJust . fst) $ map intersectWithObject objects
    intersectWithObject obj@(Object shape _) = (ray `intersect` shape, obj)
    minObject obj1 obj2                      = if fst obj2 < fst obj1 then obj2 else obj1
    (Just intersection, object)              = foldl1 minObject intersections
