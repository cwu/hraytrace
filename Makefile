HC=ghc

all: prof

prof: main.hs
	ghc -prof -auto-all --make -o main main.hs

clean:
	rm -f *.o *.hi *.prof *.exe *.exe.*
