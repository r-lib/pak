
#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif

#define R_USE_C99_IN_CXX 1
#include <Rinternals.h>

#include "ps-internal.h"

SEXP psll_pid(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  if (!handle) error("Process pointer cleaned up already");
  return ScalarInteger(handle->pid);
}

SEXP psll_create_time(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);

  if (!handle) error("Process pointer cleaned up already");
  return ScalarReal(handle->create_time);
}

SEXP ps__os_type(void) {
  SEXP res, names;

  PROTECT(res = allocVector(LGLSXP, 4));
  PROTECT(names = allocVector(STRSXP, 4));

  SET_STRING_ELT(names, 0, mkChar("POSIX"));
  SET_STRING_ELT(names, 1, mkChar("WINDOWS"));
  SET_STRING_ELT(names, 2, mkChar("LINUX"));
  SET_STRING_ELT(names, 3, mkChar("MACOS"));

  /* SET_STRING_ELT(names, 4, mkChar("FREEBSD")); */
  /* SET_STRING_ELT(names, 5, mkChar("OPENBSD")); */
  /* SET_STRING_ELT(names, 6, mkChar("NETBSD")); */
  /* SET_STRING_ELT(names, 7, mkChar("BSD")); */
  /* SET_STRING_ELT(names, 8, mkChar("SUNOS")); */
  /* SET_STRING_ELT(names, 9, mkChar("AIX")); */

  LOGICAL(res)[0] = LOGICAL(res)[1] = LOGICAL(res)[2] = LOGICAL(res)[3] = 0;

#ifdef PS__POSIX
  LOGICAL(res)[0] = 1;
#endif
#ifdef PS__WINDOWS
  LOGICAL(res)[1] = 1;
#endif
#ifdef PS__LINUX
  LOGICAL(res)[2] = 1;
#endif
#ifdef PS__MACOS
  LOGICAL(res)[3] = 1;
#endif

/* #ifdef PS__FREEBSD */
/*   LOGICAL(res)[4] = 1; */
/* #endif */
/* #ifdef PS__OPENBSD */
/*   LOGICAL(res)[5] = 1; */
/* #endif */
/* #ifdef PS__NETBSD */
/*   LOGICAL(res)[6] = 1; */
/* #endif */
/* #ifdef PS__BSD */
/*   LOGICAL(res)[7] = 1; */
/* #endif */
/* #ifdef PS__SUNOS */
/*   LOGICAL(res)[8] = 1; */
/* #endif */
/* #ifdef PS__AIX */
/*   LOGICAL(res)[9] = 1; */
/* #endif */

  setAttrib(res, R_NamesSymbol, names);
  UNPROTECT(2);
  return res;
}
