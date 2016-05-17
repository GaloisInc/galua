#include <stdlib.h>

#ifndef NEWSTATE
#error "NEWSTATE not specified"
#endif
#ifndef WRITESTRING
#error "WRITESTRING not specified"
#endif

extern void * NEWSTATE(void (*f)(void), void * ud);

void *lua_newstate( void (*f)(void), void * ud) {
        return NEWSTATE(f,ud);
}

extern size_t WRITESTRING(char *s, size_t l);

size_t galua_writestring( char * s, size_t l ) {
        return WRITESTRING(s, l);
}
