
IMGS=$(patsubst %.svg,pdf/%.pdf,$(wildcard *.svg))

talk.pdf: talk.md $(IMGS) Makefile
	pandoc -t beamer -V theme:Singapore -V institute="Galois Inc." $< -o $@

pdf/%.pdf: %.svg
	mkdir -p pdf
	inkscape -A $@ $<

.PHONY: clean
	rm -rf pdf
