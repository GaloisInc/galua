TOP=../..
LUAINC=$(TOP)/galua-c/inplace/include
LUA=$(TOP)/galua-c/inplace/bin/galua
CC=gcc

WARN= -pedantic -Wall -Wextra
#WARN == -ansi

# MODE=-O2
MODE=-g

INCS=-I$(LUAINC) -I.

MYNAME=example_c_module
TEST=test.lua




UNAME:=$(shell uname)

ifeq ($(UNAME), Linux)
SHARED_OPTS=-shared
endif

ifeq ($(UNAME), Darwin)
SHARED_OPTS=-bundle -undefined dynamic_lookup
endif

CFLAGS=-std=c99 -fPIC $(INCS) $(WARN) $(MODE)


.PHONY: all, clean

all:	$(MYNAME).so
	$(LUA) $(TEST)

$(MYNAME).so: $(MYNAME).c
	$(CC) $(CFLAGS) $(SHARED_OPTS) -o $@ $^

clean:
	rm -f $(MYNAME).so core core.*

