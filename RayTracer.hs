module RayTracer where

import Debug.Trace
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
    sqrtNumSamples :: Int
    sqrtNumSamples     = ceiling $ 1 / resolution
    resolution'        = 1 / (fromIntegral sqrtNumSamples)
    lower              = - sqrtNumSamples `div` 2
    upper              = sqrtNumSamples `div` 2
    dividePixel (x, y) = [(x + fromIntegral x' * resolution', y + fromIntegral y' * resolution)  | y' <- [lower..upper], x' <- [lower..upper]]
    pixels             = [(x,h-y-1) | y <- [0..h-1], x <- [0..w-1]]

render :: World -> Screen -> Camera -> [Color]
render world screen camera@(Camera eye _ _) =
  map (avg . map rayTracer) $ getRays screen camera
  where
    rayTracer = rayTrace world eye
    avg xs = scaleColor (1 / (fromIntegral (length xs))) (foldl1 (+) xs)

rayTrace :: World -> Point -> Ray -> Color
rayTrace world eye ray@(Ray o d)
  | not $ isJust closest = Color 0.2 0.2 0.2
  | otherwise          = color
  where
    (World (Scene objects) lights ambient) = world
    closest                                = closesetIntersection objects ray
    Just (intersection, object)            = closest
    (diffuse, specular)                    = foldl1 (\(c1,c2) (c3,c4) -> (c1+c3, c2+c4)) $ map (calculateColor intersection object eye world) lights
    (Object _ (Material kd _ _))           = object
    color                                  = ambient * kd + diffuse + specular

calculateColor :: Intersection -> Object -> Point -> World -> Light -> (Color, Color)
calculateColor intersection object eye world light
  | isJust occ                     = ((Color 0 0 0), (Color 0 0 0))
  | n `dot` l > 0 && r `dot` v > 0 = (diffuse, specular)
  | n `dot` l > 0                  = (diffuse, (Color 0 0 0))
  | r `dot` v > 0                  = ((Color 0 0 0), specular)
  | otherwise                      = ((Color 0 0 0), (Color 0 0 0))
  where
    (World (Scene objects) _ _) = world
    (Object shape material)     = object
    (Material kd ks shininess)  = material
    (Light lightPos ld ls)      = light
    (Intersection p n t)        = intersection
    occ                         = closesetIntersection objects (Ray p (norm (lightPos-p)))
    l                           = norm $ lightPos - p
    r                           = (-l) + scale (2*(l `dot` n)) n
    v                           = norm $ eye - p
    diffuse                     = ld * (scaleColor (l `dot` n) kd)
    specular                    = ls * (scaleColor ((r `dot` v)**shininess) ks)

closesetIntersection :: [Object] -> Ray -> Maybe (Intersection, Object)
closesetIntersection objects ray
  | null objects       = Nothing
  | null intersections = Nothing
  | otherwise          = Just (intersection, object)
  where
    intersections                            = filter (isJust . fst) $ map intersectWithObject objects
    intersectWithObject obj@(Object shape _) = (intersect ray shape, obj)
    minObject obj1 obj2                      = if fst obj2 < fst obj1 then obj2 else obj1
    (Just intersection, object)              = foldl1 minObject intersections

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _       = True
