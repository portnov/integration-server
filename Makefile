GHC=ghc $(GHCFLAGS) --make

all: parser-test

parser-test: parser-test.hs THIS/*.hs THIS/*/*.hs
	$(GHC) $<

clean:
	rm -f parser-test
	find . -name \*.o -delete
	find . -name \*.hi -delete
