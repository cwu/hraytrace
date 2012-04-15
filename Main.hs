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


sphere = Sphere (V 0 0 (-5)) 1
material = Material (Color 1 1 1) (Color 1 1 1) 20
scene = Scene [(Object sphere material)]
light = Light (V 3 0 0) (Color 1 1 1) (Color 1 1 1) (Color 1 1 1)
world = World scene [light]

rt = rayTrace world eye
