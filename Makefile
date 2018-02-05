all:
	ghc --make xvales02.hs -o dka-2-mka

clean:
	rm dka-2-mka *.o *.hi
