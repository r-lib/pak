#include <Rinternals.h>
#include "modp_numtoa.h"
#define NA_INTEGER64 LLONG_MIN

SEXP R_integer64_to_char(SEXP x, SEXP na_as_string){
  int len = length(x);
  int na_string = asLogical(na_as_string);
  long long * xint = (long long *) REAL(x);
  char buf[32];
  SEXP out = PROTECT(allocVector(STRSXP, len));
  for (int i = 0; i < len; i++) {
    if(xint[i] == NA_INTEGER64){
      if(na_string == NA_LOGICAL){
        SET_STRING_ELT(out, i, NA_STRING);
      } else if(na_string){
        SET_STRING_ELT(out, i, mkChar("\"NA\""));
      } else {
        SET_STRING_ELT(out, i, mkChar("null"));
      }
    } else {
      #ifdef _WIN32
        snprintf(buf, 32, "%lld", xint[i]);
      #else
        //snprintf(buf, 32, "%lld", xint[i]);
        //modp is faster (but does not work on windows)
        modp_litoa10(xint[i], buf);
      #endif
      SET_STRING_ELT(out, i, mkChar(buf));
    }
  }
  UNPROTECT(1);
  return out;
}
