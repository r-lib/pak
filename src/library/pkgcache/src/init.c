
#include "pkgcache.h"

#include <R_ext/Rdynload.h>

#ifdef GCOV_COMPILE

void __gcov_dump(void);
SEXP pkgcache__gcov_flush(void) {
  REprintf("Flushing coverage info\n");
  (void) __gcov_dump();
  return R_NilValue;
}

#else

SEXP pkgcache__gcov_flush(void) {
  return R_NilValue;
}

#endif

#define REG(name, args) { #name, (DL_FUNC) name, args }

static const R_CallMethodDef callMethods[]  = {
  REG(pkgcache_read_raw,              1),
  REG(pkgcache_parse_description_raw, 1),
  REG(pkgcache_parse_description,     1),
  REG(pkgcache_parse_descriptions,    2),
  REG(pkgcache_parse_packages_raw,    1),
  REG(pkgcache_graphics_api_version,  0),

  REG(pkgcache__gcov_flush,           0),
  { NULL, NULL, 0 }
};

void R_init_pkgcache(DllInfo *dll) {
  R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
