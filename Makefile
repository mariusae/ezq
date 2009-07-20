all: ezq

ezq: *.hs
	ghc --make -o ezq Ezq.hs

clean:
	rm -f *.hi *.o ezq
