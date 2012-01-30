GHC=ghc $(GHCFLAGS) --make

all: parser-test this

parser-test: parser-test.hs THIS/*.hs THIS/*/*.hs
	$(GHC) $<

this: this.hs THIS/*.hs THIS/*/*.hs
	$(GHC) $<

clean:
	rm -f parser-test
	find . -name \*.o -delete
	find . -name \*.hi -delete
