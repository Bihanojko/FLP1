# dka-2-mka
# Funkcionalni a logicke programovani
# Nikola Valesova, xvales02

all:
	ghc --make ${ARGS} xvales02.hs -o dka-2-mka

test: all
	python3 dka-2-mka-test.py

clean:
	rm dka-2-mka *.o *.hi
