INCLUDE_DIR=../../galua-c/inplace/include/galua
LIB_DR=../../galua-c/inplace/lib

CC=gcc
LD=ld




UNAME:=$(shell uname)

ifeq ($(UNAME), Linux)
SHARED_OPTS=-fPIC -shared
LIBS=-lgalua -lgmp -lm -lrt -ldl -lpthread -lutil
MAIN_FLAGS=-Wl,-dynamic-list=list
endif

ifeq ($(UNAME), Darwin)
SHARED_OPTS=-fPIC -bundle -undefined dynamic_lookup
LIBS=-lgalua -liconv
MAIN_FLAGS=
endif


CFLAGS=-rdynamic -std=c99 -g -Wall -Wextra -I$(INCLUDE_DIR)


.PHONY: all, clean

all: main shared_lib.so

main: main.c
	$(CC) $(CFLAGS) -o $@ $^ -L$(LIB_DR) $(LIBS)

shared_lib.so: shared_lib.c
	$(CC) $(CFLAGS) $(SHARED_OPTS) -o $@ $^

clean:
	rm -fr main shared_lib.so core core.* shared_lib.so.dSYM main.dSYM
