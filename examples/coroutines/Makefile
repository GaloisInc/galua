all: simple_coroutines galua_simple_coroutines

simple_coroutines: simple_coroutines.c
	clang `pkg-config --cflags --libs lua-5.3` $^ -o $@

galua_simple_coroutines: simple_coroutines.c ../../galua-c/libgalua.a
	clang $< -o $@ \
	    -lgalua \
	    -liconv \
	    -L../../galua-c \
	    -I../../galua-c/inplace/include/galua
