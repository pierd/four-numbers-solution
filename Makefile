4numbers: 4numbers.hs
	ghc -o 4numbers -O2 4numbers.hs

.PHONY: clean
clean:
	rm -f 4numbers 4numbers.hi 4numbers.o
