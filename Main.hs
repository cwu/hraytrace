import Vector
import Geometry
import RayTracer

objs = [(Sphere (V 100 0 0) 80, green),(Sphere (V 10 1 0) 6, blue)]
lights = [(V 0 10 0, (0.1, 0.1, 0.1))]
world = (objs, lights)
camera = (o, i, k)
screen = (200, 200, 20.0, 0.3)
pixelrays = mkpixelrays camera screen

fst3 (x,_,_) = x
snd3 (_,x,_) = x
thr3 (_,_,x) = x

display = map (render world) pixelrays

count :: Eq a => [a] -> a -> Int
count xs x = length (filter (==x) xs)

isIn :: Eq a => a -> [(a,b)] -> Bool
isIn _ [] = False
isIn y (x:xs) = (fst x == y) || isIn y xs

findCounts acc [] = acc
findCounts acc (x:xs) = if x `isIn` acc then findCounts acc xs else findCounts ((x, 1 + count xs x) : acc) xs

fc :: Eq a => [a] -> [(a,Int)]
fc = findCounts []

makePpm :: Integer -> Integer -> [Color] -> String
makePpm w h disp = "P3\n" ++ show w ++ " " ++ show h ++ "\n255" ++ stringify (map toRGB disp)
	where
		stringify [] = "\n"
		stringify ((r,g,b):xs) = "\n" ++ show r ++ " " ++ show g ++ " " ++ show b ++ stringify xs

traceToFile :: Screen -> [Color] -> IO ()
traceToFile (w,h,_,_) display = writeFile "display.ppm" (makePpm w h display)
ppm = traceToFile screen display

main = ppm
