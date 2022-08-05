.PHONY: all build check clean document test install

all: install

build: document
	$(R) CMD build .

check: build
	$(R) CMD check signatr*tar.gz

clean:
	-rm -f signatr*tar.gz
	-rm -fr signatr.Rcheck
	-rm -rf src/*.o src/*.so
	-rm -rf tests/testthat/test_db/*

document:
	$(R) -e 'devtools::document()'

test:
	$(R) -e 'devtools::test()'

install: clean
	$(R) CMD INSTALL .
