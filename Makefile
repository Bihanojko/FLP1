# dka-2-mka
# Funkcionalni a logicke programovani
# Nikola Valesova, xvales02

all:
	ghc --make ${ARGS} xvales02.hs -o dka-2-mka

clean:
	rm dka-2-mka *.o *.hi
