.PHONY: all clean

CC=gcc
TARGET=liblualibs.a

OBJS=objs/lauxlib.o \
     objs/lbaselib.o \
     objs/lbitlib.o \
     objs/lcorolib.o \
     objs/ldblib.o \
     objs/liolib.o \
     objs/lmathlib.o \
     objs/loadlib.o \
     objs/loslib.o \
     objs/lstrlib.o \
     objs/ltablib.o \
     objs/lutf8lib.o \
     objs/linit.o

all: ${TARGET}

clean:
	rm -fr objs

objs/%.o: src/%.c
	mkdir -p objs
	${CC} -c -I../galua-c/inplace/include -o $@ $<

${TARGET}: ${OBJS}
	ar -rcs $@ $^
