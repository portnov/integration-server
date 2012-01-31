GHC=ghc $(GHCFLAGS) --make

all: parser-test this this-install-db

parser-test: parser-test.hs THIS/*.hs THIS/*/*.hs
	$(GHC) $<

this-install-db: this-install-db.hs THIS/*.hs THIS/*/*.hs
	$(GHC) $<

this: this.hs THIS/*.hs THIS/*/*.hs
	$(GHC) $<

clean:
	rm -f parser-test this this-install-db
	find . -name \*.o -delete
	find . -name \*.hi -delete
