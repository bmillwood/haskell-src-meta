default: dist
	cabal build
configure dist: $(wildcard Setup.hs *.cabal)
	cabal configure
clean sdist:
	cabal $@

.PHONY: clean default sdist configure
