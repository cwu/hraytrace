SRC = Geometry.hs RayTracer.hs Main.hs Vector.hs

all: main

main: $(SRC)
	ghc -prof -auto-all --make -o main Main.hs

test: main
	@./main
	eog display.ppm

clean:
	rm -f *.o *.hi *.prof *.exe *.exe.*
