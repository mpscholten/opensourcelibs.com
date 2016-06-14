SRCS=$(wildcard src/*.hs)
GHC_FLAGS=-XOverloadedStrings -XDeriveGeneric -O0 -Wall -isrc/ -hidirdist/ -odirdist/

.PHONY: all clean

all: dist/main
	dist/main

dist/main: $(SRCS)
	mkdir -p dist
	ghc --make src/Main.hs -o dist/main ${GHC_FLAGS}

clean:
	rm -f dist/*
