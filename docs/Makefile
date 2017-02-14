.PHONY: clean

IMGS=$(patsubst %.svg,_build/%.pdf,$(wildcard *.svg))

talk.pdf: talk.md $(IMGS) Makefile
	pandoc -t beamer -V institute="Galois Inc." $< -o $@

_build/%.pdf: %.svg
	mkdir -p _build
	inkscape -A $(shell pwd)/$@ $(shell pwd)/$<
# silly use of pwd required to support OS X
# https://bugs.launchpad.net/inkscape/+bug/181639


clean:
	rm -rf _build
