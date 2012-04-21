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
hit = Ray eye (norm (V (-1.7) 0.3 (-3.4) - eye))
miss = Ray eye (norm (V 0 1.2 (-4) - eye))

screen = Screen 400 200 45 1
camera = Camera eye (-k) j


sphere1 = Sphere (V 0 0 (-4)) 1
sphere2 = Sphere (V 2.5 0 (-4)) 0.5
material1 = Material (Color 1.0 0.0 0.0) (Color 1 1 1) 20
material2 = Material (Color 0.0 1.0 0.0) (Color 1 1 1) 20
material3 = Material (Color 0.0 0.0 1.0) (Color 1 1 1) 20
cube = Cube (V (-2) 0.3 (-4)) 0.8
scene = Scene [Object sphere1 material1, Object sphere2 material2, Object cube material3]
light = Light (V 1300 200 200) (Color 0.8 0.8 0.8) (Color 0.8 0.8 0.8)
world = World scene [light] (Color 0.1 0.1 0.1)

-- debug stuff
rt = rayTrace world eye

ray = calculateRay screen camera 0 250

makePpm :: Screen -> [Color] -> String
makePpm (Screen w h _ _) disp = "P3\n" ++ show (floor w) ++ " " ++ show (floor h) ++ "\n255" ++ stringify (map toRGB disp)
  where
    stringify [] = "\n"
    stringify ((r,g,b):xs) = "\n" ++ show r ++ " " ++ show g ++ " " ++ show b ++ stringify xs

traceToFile :: Screen -> [Color] -> IO ()
traceToFile screen display = writeFile "display.ppm" (makePpm screen display)

colors = render world screen camera

ppm = traceToFile screen colors

main = ppm
