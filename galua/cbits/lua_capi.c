// `sigsetjmp` is specified by POSIX.1-2001
// `stpcpy` is specified by POSIX.1-2008
#define _POSIX_C_SOURCE 200809L

#include <setjmp.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <errno.h>

#include "lua.h"
#include "Galua_stub.h"
#include "Galua/CApi_stub.h"
#include "Rts.h"

struct lua_State
{
        // A stable pointer to an `MVar` for communication with the interpreter.
        // The `MVar` is of type `CCallState`.
        void * const token;

        // This is used to unwind the C stack, if we encounter an error.
        sigjmp_buf *onerror;
};

#define API_ENTRY(method,L,...) do { if (method##_hs(L, ##__VA_ARGS__)) raise_error(L); } while(0)

__attribute__((noreturn)) static void raise_error(lua_State *L);

LUA_API
int lua_error (lua_State *L) {
        API_ENTRY(lua_error,L);
        return 0;
}

LUA_API
void lua_createtable (lua_State *L, int narr, int nrec) {
        API_ENTRY(lua_createtable, L, narr, nrec);
}

LUA_API
const lua_Number * lua_version (lua_State *L) {
        static const lua_Number version = LUA_VERSION_NUM;
        return &version;
}

LUA_API
lua_CFunction lua_atpanic (lua_State *L, lua_CFunction panicf) {
        return NULL;
}

/*************************************************************************
 * Argument pushing
 *************************************************************************/

LUA_API
void lua_pushlightuserdata (lua_State *L, void *p) {
        API_ENTRY(lua_pushlightuserdata, L, p);
}

LUA_API
void lua_pushnumber (lua_State *L, lua_Number n) {
        API_ENTRY(lua_pushnumber, L, n);
}

LUA_API
void lua_pushboolean (lua_State *L, int b) {
        API_ENTRY(lua_pushboolean, L, b);
}

LUA_API
void lua_pushinteger (lua_State *L, lua_Integer n) {
        API_ENTRY(lua_pushinteger, L,n);
}

LUA_API
void lua_pushnil (lua_State *L) {
        API_ENTRY(lua_pushnil, L);
}

LUA_API
const char *lua_pushlstring (lua_State *L, const char *s, size_t len) {
        char *out;
        API_ENTRY(lua_pushlstring, L,(char*)s,len,&out);
        return out;
}


LUA_API
const char *lua_pushstring (lua_State *L, const char *s) {
        char *out;
        API_ENTRY(lua_pushstring, L,(char*)s,&out);
        return out;
}

static size_t utf8_length(long x) {
        if (x < 0) return 3; // replacement character
        if (x < 0x80) return 1;
        if (x < 0x800) return 2;
        if (x < 0x10000) return 3;
        if (x < 0x110000) return 4;
        return 3; // replacement character
}

static int encode_utf8(char *dst, long x) {
        const char *replacement = "\xef\xbf\xbd";
        const size_t replacement_n = strlen(replacement);
        if (x < 0) {
          strcpy(dst,replacement);
          return replacement_n;
        } else if (x < 0x80) {
          dst[0] = x;
          return 1;
        } else if (x < 0x800) {
          dst[0] = 0xc0 | (x>>6);
          dst[1] = 0x80 | (x>>0 & 0x3f);
          return 2;
        } else if (x < 0x10000) {
          dst[0] = 0xe0 | (x>>12);
          dst[1] = 0x80 | (x>>6 & 0x3f);
          dst[2] = 0x80 | (x>>0 & 0x3f);
          return 3;
        } else if (x < 0x110000) {
          dst[0] = 0xf0 | (x>>18);
          dst[1] = 0x80 | (x>>12 & 0x3f);
          dst[2] = 0x80 | (x>>6  & 0x3f);
          dst[3] = 0x80 | (x>>0  & 0x3f);
          return 4;
        } else {
          strcpy(dst,replacement);
          return replacement_n;
        }
}

LUA_API
const char *lua_pushfstring (lua_State *L, const char *fmt, ...) {
        va_list(args);
        va_start(args,fmt);
        const char * ret = lua_pushvfstring(L, fmt, args);
        va_end(args);
        return ret;
}

LUA_API
const char *lua_pushvfstring (lua_State *L, const char *fmt, va_list args) {

        int n = 0;

        while (*fmt) {
          n++;
          if (*fmt == '%') {
            fmt++;
            switch(*fmt++) {
                    case '%': lua_pushliteral(L, "%"); break;
                    case 's':
                         { char *str = va_arg(args, char*);
                           lua_pushstring(L, str);
                           break;
                         }
                    case 'f': lua_pushnumber(L, va_arg(args, lua_Number)); break;
                    case 'I': lua_pushinteger(L, va_arg(args, lua_Integer)); break;
                    case 'p':
                         { void *p = va_arg(args, void*);
                           int len = snprintf(NULL, 0, "%p", p);
                           char buffer[len+1];
                           sprintf(buffer, "%p", p);
                           lua_pushlstring(L, buffer, len);
                           break;
                         }
                    case 'd': lua_pushinteger(L, va_arg(args, int)); break;
                    case 'c':
                         { char c = va_arg(args,int);
                           lua_pushlstring(L, &c, 1);
                           break;
                         }
                    case 'U':
                         { char buffer[4];
                           int len = encode_utf8(buffer, va_arg(args,long));
                           lua_pushlstring(L, buffer, len);
                           break;
                         }
                    default:
                         {
                                 char buffer[20];
                                 sprintf(buffer, "invalid option '%%%c'", fmt[-1]);
                                 lua_pushstring(L, buffer);
                                 lua_error(L);
                                 abort();
                         }
            }
          } else {
            char *next = strchr(fmt, '%');
            if (next == NULL) {
                lua_pushstring(L, fmt);
                break;
            } else {
                lua_pushlstring(L, fmt, next - fmt);
                fmt = next;
            }
          }
        }

        va_end(args);

        if (n == 0) {
                lua_pushliteral(L, "");
        } else {
                lua_concat(L, n);
        }

        return lua_tostring(L, -1);
}


LUA_API
void lua_pushvalue (lua_State *L, int index) {
        API_ENTRY(lua_pushvalue, L,index);
}

LUA_API
void lua_pushcclosure (lua_State *L, lua_CFunction fn, int n) {
        API_ENTRY(lua_pushcclosure, L, (void (*)(void)) fn, n);
}

/*************************************************************************
 * Argument checking
 *************************************************************************/


LUA_API
int lua_type (lua_State *L, int index) {
        int out;
        API_ENTRY(lua_type, L, index, &out);
        return out;
}

LUA_API
const char *lua_typename (lua_State *L, int tp) {
  switch(tp) {
          case LUA_TNIL: return "no value";
          case LUA_TNUMBER: return "number";
          case LUA_TBOOLEAN: return "boolean";
          case LUA_TSTRING: return "string";
          case LUA_TTABLE: return "table";
          case LUA_TFUNCTION: return "function";
          case LUA_TUSERDATA: return "userdata";
          case LUA_TTHREAD: return "thread";
          case LUA_TLIGHTUSERDATA: return "userdata";
          default: return "unknown";
  }
}

LUA_API
void lua_settop (lua_State *L, int index) {
        API_ENTRY(lua_settop, L, index);
}

LUA_API
int lua_gettop (lua_State *L) {
        int out;
        API_ENTRY(lua_gettop, L, &out);
        return out;
}

LUA_API
void lua_settable (lua_State *L, int index) {
        API_ENTRY(lua_settable, L, index);
}

LUA_API
void lua_copy (lua_State *L, int fromidx, int toidx) {
        API_ENTRY(lua_copy, L, fromidx, toidx);
}

LUA_API
lua_Number lua_tonumberx (lua_State *L, int index, int *isnum) {
        lua_Number out;
        API_ENTRY(lua_tonumberx, L, index, isnum, &out);
        return out;
}

LUA_API
int lua_toboolean (lua_State *L, int index) {
        int out;
        API_ENTRY(lua_toboolean, L, index, &out);
        return out;
}

LUA_API
lua_Integer lua_tointegerx (lua_State *L, int index, int *isnum) {
        lua_Integer out;
        API_ENTRY(lua_tointegerx, L, index, isnum, &out);
        return out;
}

LUA_API
lua_CFunction lua_tocfunction (lua_State *L, int index) {
        lua_CFunction out;
        API_ENTRY(lua_tocfunction, L, index, &out);
        return out;
}

LUA_API
const char *lua_tolstring (lua_State *L, int index, size_t *len) {
        char *out;
        API_ENTRY(lua_tolstring, L, index, len, &out);
        return out;
}

LUA_API
void *lua_touserdata (lua_State *L, int index) {
        void *out;
        API_ENTRY(lua_touserdata, L, index, &out);
        return out;
}

LUA_API
int lua_getfield (lua_State *L, int index, const char *k) {
        int out;
        API_ENTRY(lua_getfield, L, index, (char*)k, &out);
        return out;
}

LUA_API
void lua_setfield (lua_State *L, int index, const char *k) {
        API_ENTRY(lua_setfield, L, index, (char*)k);
}

LUA_API
int lua_rawequal (lua_State *L, int index1, int index2) {
        int out;
        API_ENTRY(lua_rawequal, L, index1, index2, &out);
        return out;
}

LUA_API int lua_compare (lua_State *L, int idx1, int idx2, int op) {
        int out;
        API_ENTRY(lua_compare, L, idx1, idx2, op, &out);
        return out;
}

LUA_API void lua_arith (lua_State *L, int op) {
        API_ENTRY(lua_arith, L, op);
}

LUA_API
int lua_rawgeti (lua_State *L, int idx, lua_Integer n) {
        int out;
        API_ENTRY(lua_rawgeti, L, idx, (long)n, &out);
        return out;
}

LUA_API
int lua_rawgetp (lua_State *L, int idx, const void *p) {
        int out;
        API_ENTRY(lua_rawgetp, L, idx, (void*)p, &out);
        return out;
}

LUA_API int (lua_geti) (lua_State *L, int idx, lua_Integer n) {
        int out;
        API_ENTRY(lua_geti, L, idx, (long)n, &out);
        return out;
}

LUA_API
size_t lua_rawlen (lua_State *L, int idx) {
        size_t out;
        API_ENTRY(lua_rawlen, L, idx, &out);
        return out;
}

LUA_API
int lua_rawget (lua_State *L, int index) {
        int out;
        API_ENTRY(lua_rawget, L, index, &out);
        return out;
}

LUA_API
int lua_gettable (lua_State *L, int index) {
        int out;
        API_ENTRY(lua_gettable, L, index, &out);
        return out;
}

LUA_API
void lua_rawset (lua_State *L, int index) {
        API_ENTRY(lua_rawset, L, index);
}

LUA_API
void lua_rawseti (lua_State *L, int idx, lua_Integer n) {
        API_ENTRY(lua_rawseti, L, idx, n);
}

LUA_API
void lua_rawsetp (lua_State *L, int idx, const void *p) {
        API_ENTRY(lua_rawsetp, L, idx, (void*)p);
}

LUA_API
void  lua_seti(lua_State *L, int idx, lua_Integer n) {
        API_ENTRY(lua_seti, L, idx, n);
}

LUA_API
void lua_rotate (lua_State *L, int idx, int n) {
        API_ENTRY(lua_rotate, L, idx, n);
}

LUA_API
int lua_getmetatable (lua_State *L, int index) {
        int out;
        API_ENTRY(lua_getmetatable, L, index, &out);
        return out;
}

LUA_API
void *lua_newuserdata (lua_State *L, size_t size) {
        void *out;
        API_ENTRY(lua_newuserdata, L, size, &out);
        return out;

}

LUA_API
int lua_getuservalue (lua_State *L, int index) {
        int out;
        API_ENTRY(lua_getuservalue, L, index, &out);
        return out;
}

LUA_API
void lua_setuservalue (lua_State *L, int index) {
        API_ENTRY(lua_setuservalue, L, index);
}

LUA_API
void lua_setmetatable (lua_State *L, int index) {
        API_ENTRY(lua_setmetatable, L, index);
}

LUA_API
int lua_isstring (lua_State *L, int index) {
        int out;
        API_ENTRY(lua_isstring, L, index, &out);
        return out;
}

LUA_API
int lua_iscfunction (lua_State *L, int index) {
        int out;
        API_ENTRY(lua_iscfunction, L, index, &out);
        return out;
}

LUA_API
int lua_isinteger (lua_State *L, int index) {
        int out;
        API_ENTRY(lua_isinteger, L, index, &out);
        return out;
}

LUA_API
size_t lua_stringtonumber (lua_State *L, const char *s) {
        size_t out;
        API_ENTRY(lua_stringtonumber, L, (char*)s, &out);
        return out;

}

LUA_API
int lua_isuserdata (lua_State *L, int index) {
        int out;
        API_ENTRY(lua_isuserdata, L, index, &out);
        return out;
}

LUA_API
int lua_isnumber (lua_State *L, int index) {
        int out;
        API_ENTRY(lua_isnumber, L, index, &out);
        return out;
}

LUA_API
void lua_callk (lua_State *L, int nargs, int nresults, lua_KContext ctx, lua_KFunction k) {
        API_ENTRY(lua_callk, L, nargs, nresults, ctx, (HsFunPtr)k);
}

LUA_API
int lua_pcallk (lua_State *L, int nargs, int nresults, int msgh, lua_KContext ctx, lua_KFunction k) {
        int out;
        API_ENTRY(lua_pcallk, L, nargs, nresults, msgh, &out);
        return out;
}

LUA_API
int lua_absindex (lua_State *L, int idx) {
        int out;
        API_ENTRY(lua_absindex, L, idx, &out);
        return out;
}

LUA_API
void lua_len (lua_State *L, int idx) {
        API_ENTRY(lua_len, L, idx);
}

LUA_API
int lua_load (lua_State *L, lua_Reader reader, void *data, const char *chunkname, const char *mode) {

        char *all_data = NULL;
        size_t all_data_n = 0;

        for(;;) {
                size_t next_chunk_n = 0;
                const char *next_chunk = reader(L, data, &next_chunk_n);

                if (next_chunk == NULL && next_chunk_n == 0) {
                        break;
                }

                all_data = realloc(all_data, all_data_n + next_chunk_n);
                if (!all_data) abort();

                memcpy(all_data + all_data_n, next_chunk, next_chunk_n);
                all_data_n += next_chunk_n;
        }

        int out;
        int res = lua_load_hs(L, all_data, all_data_n, (char*)chunkname, (char*)mode, &out);
        free(all_data);
        if (res) raise_error(L);
        return out;
}

LUA_API
void lua_setglobal (lua_State *L, const char *name) {
        API_ENTRY(lua_setglobal, L, (char*)name);
}

LUA_API
int lua_getglobal (lua_State *L, const char *name) {
        int out;
        API_ENTRY(lua_getglobal, L, (char*)name, &out);
        return out;
}


LUA_API
int lua_checkstack (lua_State *L, int n) {
        return 1;
}

void lua_concat (lua_State *L, int n) {
        API_ENTRY(lua_concat, L, n);
}

static void *l_alloc (void *ud, void *ptr, size_t osize, size_t nsize) {
  (void)ud; (void)osize;  /* not used */
  if (nsize == 0) {
    free(ptr);
    return NULL;
  }
  else
    return realloc(ptr, nsize);
}

LUA_API
lua_Alloc lua_getallocf (lua_State *L, void **ud) {
        *ud = NULL;
        return l_alloc;
}

LUA_API
lua_Hook lua_gethook (lua_State *L) {
        return NULL; // XXX: hook not supported
}


LUA_API
void lua_sethook (lua_State *L, lua_Hook func, int mask, int count) {
}

LUA_API
int lua_getstack (lua_State *L, int level, lua_Debug *ar) {
        int out;
        API_ENTRY(lua_getstack, L, level, ar, &out);
        return out;
}

LUA_API
int lua_getinfo (lua_State *L, const char *what, lua_Debug *ar) {
        int out;
        API_ENTRY(lua_getinfo, L, (HsPtr)what, (HsPtr)ar, &out);
        return out;
}

LUA_API
const char *lua_getlocal (lua_State *L, const lua_Debug *ar, int n) {
        const char *out;
        API_ENTRY(lua_getlocal, L, (HsPtr)ar, n, &out);
        return out;
}

LUA_API
const char *lua_setlocal (lua_State *L, const lua_Debug *ar, int n) {
        const char *out;
        API_ENTRY(lua_setlocal, L, (HsPtr)ar, n, &out);
        return out;
}

LUA_API
const char *lua_getupvalue (lua_State *L, int funcindex, int n) {
        const char *out;
        API_ENTRY(lua_getupvalue, L, funcindex, n, &out);
        return out;
}

LUA_API
const char *lua_setupvalue (lua_State *L, int funcindex, int n) {
        const char *out;
        API_ENTRY(lua_setupvalue, L, funcindex, n, &out);
        return out;
}

LUA_API
void lua_upvaluejoin (lua_State *L, int funcindex1, int n1, int funcindex2, int n2) {
        API_ENTRY(lua_upvaluejoin, L, funcindex1, n1, funcindex2, n2);
}

LUA_API
int lua_gc (lua_State *L, int what, int data) {
        int out;
        API_ENTRY(lua_gc, L, what, data, &out);
        return out;
}

LUA_API
void *lua_upvalueid (lua_State *L, int funcindex, int n) {
        void *out;
        API_ENTRY(lua_upvalueid, L, funcindex, n, &out);
        return out;
}

LUA_API
void lua_xmove (lua_State *from, lua_State *to, int n) {
        API_ENTRY(lua_xmove, from, to, n);
}

LUA_API
const void *lua_topointer (lua_State *L, int idx) {
        void *out;
        API_ENTRY(lua_topointer, L, idx, &out);
        return out;
}

LUA_API
int lua_gethookcount (lua_State *L) {
        return 0; // XXX: hook not supported
}

LUA_API
int lua_gethookmask (lua_State *L) {
        return 0; // XXX: hook not supported
}

LUA_API
int lua_next (lua_State *L, int index) {
        int out;
        API_ENTRY(lua_next, L, index, &out);
        return out;
}

LUA_API int lua_dump (lua_State *L, lua_Writer writer, void *data, int strip) {
        int out;
        API_ENTRY(lua_dump, L, (HsFunPtr)writer, data, strip, &out);
        return out;
}

LUA_API
void lua_setallocf (lua_State *L, lua_Alloc f, void *ud) {
        API_ENTRY(lua_setallocf, L, (HsFunPtr)f, ud);
}

int    galua_argc = 0;    // Means: "use defaults for hs_init"
int    galua_argu = 0;
char **galua_argv = NULL;

LUA_API
lua_State *lua_newstate_nondbg (lua_Alloc f, void *ud) {
  lua_State *res;
  RtsConfig config = defaultRtsConfig;

  config.rts_opts = "--install-signal-handlers=no";

  if (galua_argc == 0) {
    hs_init_ghc(NULL, NULL, config);
  } else {
    hs_init_ghc(&galua_argc, &galua_argv, config);
  }

  res = galua_newstate(&galua_argu);
  memset(lua_getextraspace(res), 0, LUA_EXTRASPACE);
  return res;
}

size_t galua_writestring_nondbg(char *s, size_t l) {
  return fwrite(s, sizeof(char), l, stdout);
}

LUA_API void lua_close (lua_State *L) {
}


__attribute__((noreturn))
static void raise_error(lua_State *L) {
  if (L->onerror == NULL) {
          fprintf(stderr, "Galua: Unhandled error\n");
          abort();
  }
  siglongjmp(*L->onerror,1);
}

struct LX {
        void *extra;
        lua_State st;
};

extern
void galua_free_luaState(lua_State *L) {

        struct LX lx;
        ptrdiff_t offset = (void*)(&lx.st) - (void*)&lx;
        void *ptr = (void*)L - offset;
        free(ptr);
}

/* Called from Haskell */
extern
lua_State *galua_allocate_luaState(void *token) {
        lua_State initL = {
             .token = token,
             };

        struct LX * lx = malloc(sizeof(struct LX));
        if (lx != NULL) {
                lua_State *L = &lx->st;
                memcpy(L, &initL, sizeof(initL));
                return L;
        }
        return NULL;
}

/* This is the entrypoint from Lua calling into C.
 * Instead of calling C functions directly, we pass
 * them as the first argument to this function.
 * This allows us to integrate Lua error propagation
 * behavior into the C call.
 */
extern
int galua_capi_entry(int (*cfun)(void *), lua_State *L) {
        lua_State old_state;
        memcpy(&old_state, L, sizeof(lua_State));

        sigjmp_buf new_buf;
        L->onerror = &new_buf;

        int jumpresult = sigsetjmp(new_buf,0/* ignore signals */);
        int result;

        if (jumpresult == 0) {
                /* normal control flow */
                result = cfun(L);

                /* Lua C functions return value is number of returned
                 * values on the Lua stack. Negative numbers are invalid
                 * and reserved for Galua error signaling.
                 */
                if (result < 0) {
                        result = -2;
                }
        } else {
                /* exceptional control flow */
                result = -1;
        }

        memcpy(L, &old_state, sizeof(lua_State));

        return result;
}

extern
int galua_capi_entryk(lua_KFunction k, lua_KContext ctx, int status, lua_State *L) {
        lua_State old_state;
        memcpy(&old_state, L, sizeof(lua_State));

        sigjmp_buf new_buf;
        L->onerror = &new_buf;

        int jumpresult = sigsetjmp(new_buf,0/* ignore signals */);
        int result;

        if (jumpresult == 0) {
                /* normal control flow */
                result = k(L, status, ctx);

                /* Lua C functions return value is number of returned
                 * values on the Lua stack. Negative numbers are invalid
                 * and reserved for Galua error signaling.
                 */
                if (result < 0) {
                        result = -2;
                }
        } else {
                /* exceptional control flow */
                result = -1;
        }

        memcpy(L, &old_state, sizeof(lua_State));

        return result;
}


/* Coroutines **************************************************/

LUA_API
lua_State *lua_tothread(lua_State *L, int index) {
  lua_State* res;
  API_ENTRY(lua_tothread, L, index, &res);
  return res;
}

LUA_API
int lua_pushthread(lua_State *L) {
  int res;
  API_ENTRY(lua_pushthread, L, &res);
  return res;
}

LUA_API
int lua_isyieldable(lua_State *L) {
  int res;
  API_ENTRY(lua_isyieldable, L, &res);
  return res;
}

LUA_API
lua_State *lua_newthread(lua_State *L) {
  lua_State *res;
  API_ENTRY(lua_newthread, L, &res);
  memcpy(lua_getextraspace(res), lua_getextraspace(L), sizeof(LUA_EXTRASPACE));
  return res;
}

LUA_API
int lua_resume (lua_State *L, lua_State *from, int nargs) {
  int res;
  API_ENTRY(lua_resume, L, from, nargs, &res);
  return res;
}

LUA_API
int lua_status(lua_State *L) {
  int res;
  API_ENTRY(lua_status, L, &res);
  return res;
}

LUA_API
int lua_yieldk (lua_State *L,
                int nresults,
                lua_KContext ctx,
                lua_KFunction k) {
  API_ENTRY(lua_yieldk, L, nresults, ctx, (HsFunPtr)k);
  abort();
}

/* End of Coroutines ********************************************/




/************************************************************************
 * LUA AUX LIB
 */


static inline void lua_insert_ (lua_State *L, int index) { lua_insert(L, index); }
#undef lua_insert
LUA_API void lua_insert (lua_State *L, int index) { lua_insert_(L, index); }

static inline void lua_remove_ (lua_State *L, int index) { lua_remove(L, index); }
#undef lua_remove
LUA_API void lua_remove (lua_State *L, int index) { lua_remove_(L, index); }

static inline void lua_replace_ (lua_State *L, int index) { lua_replace(L, index); }
#undef lua_replace
LUA_API void lua_replace (lua_State *L, int index) { lua_replace_(L, index); }

static inline void lua_pushunsigned_ (lua_State *L, lua_Unsigned n) { lua_pushunsigned(L,n); }
#undef lua_pushunsigned
LUA_API
void lua_pushunsigned (lua_State *L, lua_Unsigned n) { lua_pushunsigned_(L,n); }
