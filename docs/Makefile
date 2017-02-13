
IMGS=$(patsubst %.svg,pdf/%.pdf,$(wildcard *.svg))

talk.pdf: talk.md $(IMGS) Makefile
	pandoc -t beamer -V theme:Singapore -V institute="Galois Inc." $< -o $@

pdf/%.pdf: %.svg
	mkdir -p pdf
	inkscape -A $(shell pwd)/$@ $(shell pwd)/$<
# silly use of pwd required to support OS X
# https://bugs.launchpad.net/inkscape/+bug/181639

.PHONY: clean
	rm -rf pdf
