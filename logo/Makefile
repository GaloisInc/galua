
.PHONY: all clean

SIZES=16 32 62 128 256 512 1024
NAME_PREF=galua

all:
	for i in $(SIZES); do \
	  inkscape -w $$i -h $$i \
                   drawing.svg --export-png=$(NAME_PREF)_$${i}x$${i}.png; \
	  done

clean:
	for i in $(SIZES); do rm -f $(NAME_PREF)_$${i}x$${i}.png; done


