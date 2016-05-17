.PHONY: all install clean distclean

TOP=..

DIST=inplace

install:
	mkdir -p $(DIST)
	mkdir -p $(DIST)/bin
	mkdir -p $(DIST)/include
	mkdir -p $(DIST)/lib
	cp -r $(TOP)/galua/include $(DIST)/include/galua
	./build galua
	cp libgalua.a $(DIST)/lib
	cp galua $(DIST)/bin
	./build galua-dbg
	cp libgalua-dbg.a $(DIST)/lib
	cp galua-dbg $(DIST)/bin
	gzip -f libgalua.a
	gzip -f libgalua-dbg.a

all:
	./build-lib galua
	./build-lib galua-dbg


clean:
	rm -f galua libgalua.a.gz galua-dbg libgalua-dbg.a.gz newstate.o

distclean: clean
	rm -rf $(DIST)
