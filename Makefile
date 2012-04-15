OPTIMIZE_FLAGS = -fexcess-precision -O2 -optc-O3 -optc-ffast-math -funfolding-use-threshold=16
SRC = Geometry.hs RayTracer.hs Main.hs Vector.hs

all: main

main: $(SRC)
	ghc $(OPTIMIZE_FLAGS) -prof -rtsopts -auto-all --make -o $@ Main.hs

preview: main
	@./main
	@eog display.ppm

test: UnitTest.hs
	ghc -auto-all --make -o $@ $<

time: main
	time ./main

clean:
	rm -f main *.o *.hi *.prof *.exe *.exe.*
