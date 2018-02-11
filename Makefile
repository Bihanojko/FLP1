# dka-2-mka
# Funkcionalni a logicke programovani
# Nikola Valesova, xvales02

all:
	ghc --make xvales02.hs -o dka-2-mka

run: all
	./dka-2-mka ${ARGS}

clean:
	rm dka-2-mka *.o *.hi
