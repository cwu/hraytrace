SRC = Main.hs Geometry.hs Algebra.hs RayTracer.hs Light.hs Object.hs

all: main

optimized: $(SRC)
	ghc -O2 --make -o main Main.hs

main: $(SRC)
	ghc -rtsopts -auto-all -prof -O2 --make -o main Main.hs

clean:
	rm -f main *.o *.hi
