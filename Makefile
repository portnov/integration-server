GHC=ghc $(GHCFLAGS) --make

all: this this-install-db

this-install-db: this-install-db.hs THIS/*.hs THIS/*/*.hs
	$(GHC) $<

this: this.hs THIS/*.hs THIS/*/*.hs
	$(GHC) $<

install-db: this-install-db
	./this-install-db

clean:
	rm -f parser-test this this-install-db
	find . -name \*.o -delete
	find . -name \*.hi -delete
