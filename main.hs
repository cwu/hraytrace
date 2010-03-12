import Geometry
import Vector

x = V 1 1 1

r = [ray_mk (V 0 0 0) (V 1 0 0), ray_mk (V 0 0 0) (V (-1) 0 0)]
s = [Sphere (V 10 2 0) 1, Sphere (V 10 1 0) 1, Sphere (V (-10) 1 0) 1, Sphere (V 10 2 0) 1, Sphere (V 10 (-2) 0) 1, Sphere (V (-10) 2 0) 1, Sphere (V 0 0 0) 1, Sphere (V 1 0 0) 1, Sphere (V (-1) 0 0) 1, Sphere (V 2 0 0) 1, Sphere (V (-2) 0 0) 1]

answers = [[Nothing, Nothing], [Just 10, Nothing], [Nothing, Just 10], [Nothing, Nothing], [Nothing, Nothing], [Nothing, Nothing], [Nothing, Nothing], [Just 0, Nothing], [Nothing, Just 0], [Just 1, Nothing], [Nothing, Just 1]]

results = map (\y -> map (intersect y) r) s

res = filter (\(x,_,_) -> not x) $ zip3 (map (\(a,b) -> a == b) $ zip answers results) s (zip results answers)

main = if (not . null) res then putStrLn "Failure" else putStrLn "Pass"
