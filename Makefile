SRC = Main.hs Geometry.hs Algebra.hs RayTracer.hs Light.hs Object.hs

all: main

main: $(SRC)
	ghc -O2 --make -o main Main.hs

clean:
	rm main *.o *.hi
