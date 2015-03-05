install:
	cabal install

all: install hlint test

hlint:
	hlint src/*.hs

test: test-dictbuilder test-pronounce

test-dictbuilder:
	cd data && dictbuilder
	diff data/phonetic-dict.txt.bak data/phonetic-dict.txt

test-pronounce:
	pronounce samples/mad-tea-party.txt  |diff - samples/mad-tea-party.pron
	pronounce samples/universal-declaration-of-human-rights.txt  |diff - samples/universal-declaration-of-human-rights.pron
