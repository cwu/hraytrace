import Geometry
import Vector
import RayTracer

x = V 1 1 1

{-
r = [ray_mk (V 0 0 0) (V 1 0 0), ray_mk (V 0 0 0) (V (-1) 0 0)]
s = [Sphere (V 10 2 0) 1, Sphere (V 10 1 0) 1, Sphere (V (-10) 1 0) 1, Sphere (V 10 2 0) 1, Sphere (V 10 (-2) 0) 1, Sphere (V (-10) 2 0) 1, Sphere (V 0 0 0) 1, Sphere (V 1 0 0) 1, Sphere (V (-1) 0 0) 1, Sphere (V 2 0 0) 1, Sphere (V (-2) 0 0) 1]

answers = [[Nothing, Nothing], [Just 10, Nothing], [Nothing, Just 10], [Nothing, Nothing], [Nothing, Nothing], [Nothing, Nothing], [Nothing, Nothing], [Just 0, Nothing], [Nothing, Just 0], [Just 1, Nothing], [Nothing, Just 1]]

results :: [
results = map (\y -> map (intersect y) r) s

res = filter (\(x,_,_) -> not x) $ zip3 (map (\(a,b) -> a == b) $ zip answers results) s (zip results answers)

main = if (not . null) res then putStrLn "Failure" else putStrLn "Pass"
-}
objs = [(Sphere (V 100 0 0) 80, green),(Sphere (V (10) 1 0) 6, blue)]
lights = [(V 100 100 100, (0.1, 0.1, 0.1))]
world = (objs, lights)
camera = (o, i, j)
screen = (100, 100, 6.0)
pixelrays = mkpixelrays camera screen

fst3 (x,_,_) = x
snd3 (_,x,_) = x
thr3 (_,_,x) = x

display = map (trace world . thr3) pixelrays

count :: Eq a => [a] -> a -> Int
count xs x = length (filter (==x) xs)

is_in :: Eq a => a -> [(a,b)] -> Bool
is_in _ [] = False
is_in y (x:xs) = if fst x == y then True else is_in y xs

findCounts acc [] = acc
findCounts acc (x:xs) = if x `is_in` acc then findCounts acc xs else findCounts ((x, 1 + count xs x) : acc) xs

fc :: Eq a => [a] -> [(a,Int)]
fc = findCounts []

make_ppm :: Integer -> Integer -> [Color] -> String
make_ppm w h disp = "P3\n" ++ show w ++ " " ++ show h ++ "\n255\n" ++ stringify (map toRGB disp)
	where 
		stringify [] = ""
		stringify ((r,g,b):xs) = " " ++ show r ++ " " ++ show g ++ " " ++ show b ++ stringify xs

traceToFile :: Screen -> [Color] -> IO ()
traceToFile (w,h,_) display = writeFile "display.ppm" (make_ppm w h display)
ppm = traceToFile screen display
		
