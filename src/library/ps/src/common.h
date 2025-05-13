
#ifndef R_PS_COMMON_H
#define R_PS_COMMON_H

#define R_USE_C99_IN_CXX 1
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <Rversion.h>

#include "ps-internal.h"

/* ---------------------------------------------------------------------*/
/* Internals                                                            */
/* ---------------------------------------------------------------------*/

static R_INLINE SEXP ps_new_env(void) {
  SEXP env;
#if R_VERSION >= R_Version(4, 1, 0)
  PROTECT(env = R_NewEnv(R_EmptyEnv, 1, 29));
#else
  PROTECT(env = Rf_allocSExp(ENVSXP));
  SET_FRAME(env, R_NilValue);
  SET_ENCLOS(env, R_EmptyEnv);
  SET_HASHTAB(env, R_NilValue);
  SET_ATTRIB(env, R_NilValue);
#endif
  UNPROTECT(1);
  return env;
}

extern int PS__TESTING;
extern int PS__DEBUG;

// a signaler for connections without an actual status
static const int PS__CONN_NONE = 128;

void ps__set_testing(void);
void ps__debug(const char* format, ...);
void R_init_ps(DllInfo *dll);

#endif // PSUTIL_PSUTIL_COMMON_H
