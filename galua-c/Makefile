.PHONY: all install clean distclean prepare

TOP=..

DIST=inplace
CC=gcc
LUALIBS=liblualibs.a

UNAME:=$(shell uname -s)

ifeq ($(UNAME), Darwin)
USE_FLAGS=-DLUA_USE_MACOSX
else
USE_FLAGS=-DLUA_USE_LINUX
endif


LUALIBS_OBJS=\
     lualibs/objs/lauxlib.o \
     lualibs/objs/lbaselib.o \
     lualibs/objs/lbitlib.o \
     lualibs/objs/lcorolib.o \
     lualibs/objs/ldblib.o \
     lualibs/objs/liolib.o \
     lualibs/objs/lmathlib.o \
     lualibs/objs/loadlib.o \
     lualibs/objs/loslib.o \
     lualibs/objs/lstrlib.o \
     lualibs/objs/ltablib.o \
     lualibs/objs/lutf8lib.o \
     lualibs/objs/linit.o \
     lualibs/objs/lcompat.o

install: prepare ${LUALIBS}
	./build galua
	cp libgalua.a $(DIST)/lib
	cp galua $(DIST)/bin
	./build galua-dbg
	cp libgalua-dbg.a $(DIST)/lib
	cp galua-dbg $(DIST)/bin

prepare:
	mkdir -p $(DIST)
	mkdir -p $(DIST)/bin
	mkdir -p $(DIST)/include/galua
	mkdir -p $(DIST)/lib
	cp $(TOP)/galua/include/* $(DIST)/include/galua
	cp $(TOP)/galua-c/lualibs/include/* $(DIST)/include/galua

all:
	./build-lib galua
	./build-lib galua-dbg


clean:
	rm -f galua galua-dbg newstate.o ${LUALIBS}
	rm -fr inplace lualibs/objs

distclean: clean
	rm -rf $(DIST)

lualibs/objs/%.o: lualibs/src/%.c
	mkdir -p lualibs/objs
	${CC} -fPIC -g -c $(USE_FLAGS) -Iinplace/include/galua -o $@ $<

${LUALIBS}: ${LUALIBS_OBJS}
	ar -rcs $@ $^
