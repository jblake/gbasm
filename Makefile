build:
	runhaskell Setup.hs configure --prefix=/usr --docdir=/usr/share/doc/gbasm --disable-executable-profiling -O --enable-executable-stripping --enable-tests
	runhaskell Setup.hs build

test:
	runhaskell Setup.hs test

install:
	runhaskell Setup.hs copy --destdir="$(DESTDIR)"

clean:
	rm -rf dist
