module RayTracer where

import Geometry
import Algebra
import Object
import Light

-- Scene objects
data Scene = Scene [Object] deriving (Eq, Show)

-- World scene lights
data World = World Scene [Light] deriving (Eq, Show)

-- Screen width height fov
data Screen = Screen Double Double Double deriving (Eq, Show)

-- Camera position viewVector upVector
data Camera = Camera Point Vector Vector deriving (Eq, Show)

calculateRay :: Screen -> Camera -> Double -> Double -> Ray
calculateRay (Screen w h fov) (Camera cp view up) x y =
  Ray cp (norm dir)
  where
    side     = view * up
    distance = (h / 2) / tan (fov / 2)
    dirSide  = scale (x - w / 2) side
    dirUp    = scale (y - h / 2) up
    dirView  = scale distance view
    dir      = dirSide + dirUp + dirView

getRays :: Screen -> Camera -> [Ray]
getRays screen@(Screen w h _) camera = 
  map (uncurry (calculateRay screen camera)) [(x,y) | x <- [0..w-1], y <- [0..h-1]]

render :: World -> Screen -> Camera -> [Color]
render world screen camera@(Camera eye _ _) =
  map (rayTrace world eye) $ getRays screen camera

rayTrace :: World -> Point -> Ray -> Color
rayTrace world eye ray@(Ray o d) 
  | null objects       = Color 0 0 0
  | null intersections = Color 0.5 0.5 0.5
  | otherwise          = Color 1 1 1
  where
    (World (Scene objects) lights)           = world
    intersections                            = filter (isJust . fst) $ map intersectWithObject objects
    intersectWithObject obj@(Object shape _) = (intersect ray shape, obj)
    (Just intersection, object)                   = foldl1 (\min_ obj -> if fst obj < fst min_ then obj else min_) intersections
    (diffuse, specular)                      = foldl1 (\(c1,c2) (c3,c4) -> (c1+c3, c2+c4)) $ map (calculateColor intersection object eye) lights
    

calculateColor :: Intersection -> Object -> Point -> Light -> (Color, Color)
calculateColor intersection object eye light =
  (diffuse, specular)
  where
    (Object shape material) = object 
    (Material kd ks shininess) = material
    (Light lightPos la ld ls) = light
    (Intersection p n t) = intersection
    l = lightPos - p
    r = (-l) + scale (2*(l `dot` n)) n
    v = eye - p
    diffuse = ld * (scaleColor (l `dot` n) kd)
    specular = ls * (scaleColor ((r `dot` v)**shininess) ks)
    


isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _       = True
