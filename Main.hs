import RayTracer
import Geometry
import Light
import Algebra
import Object

i = V 1 0 0
j = V 0 1 0
k = V 0 0 1
o = V 0 0 0

eye = o
hit = Ray eye (-k)
miss = Ray eye i

screen = Screen 100 100 45
camera = Camera eye (-k) j


sphere = Sphere (V 0 0 (-5)) 1
material = Material (Color 1 1 1) (Color 1 1 1) 20
scene = Scene [(Object sphere material)]
light = Light (V 3 0 0) (Color 1 1 1) (Color 1 1 1) (Color 1 1 1)
world = World scene [light]

-- debug stuff
rt = rayTrace world eye

ray = calculateRay screen camera 0 250


makePpm :: Screen -> [Color] -> String
makePpm (Screen w h _) disp = "P3\n" ++ show (floor w) ++ " " ++ show (floor h) ++ "\n255" ++ stringify (map toRGB disp)
  where
    stringify [] = "\n"
    stringify ((r,g,b):xs) = "\n" ++ show r ++ " " ++ show g ++ " " ++ show b ++ stringify xs

traceToFile :: Screen -> [Color] -> IO ()
traceToFile screen display = writeFile "display.ppm" (makePpm screen display)

colors = render world screen camera

ppm = traceToFile screen colors

main = ppm
