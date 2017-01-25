yieldk: simple_coroutines_yieldk.c
	clang $(shell pkg-config --cflags --libs lua-5.3) $^ -o $@

galua_yieldk: simple_coroutines_yieldk.c ../../galua-c/libgalua.a
	clang -I../../galua-c/inplace/include/galua \
	      -L../../galua-c \
	      -lgalua -liconv $< -o $@
