
build:
	cabal build

init:
	cabal sandbox init
	cabal install --dependencies-only
