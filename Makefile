all: ezq

ezq: *.hs
	ghc --make -o ezq ezq.hs

test:
	cd tests; make

clean:
	rm -f *.hi *.o ezq dist
