all: ezq

ezq: *.hs
	ghc --make -o ezq Ezq.hs

test:
	cd tests; make

clean:
	rm -f *.hi *.o ezq
