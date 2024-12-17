#include <Rinternals.h>
#include <stdlib.h>
#include <string.h>
#include "base64.h"

SEXP R_base64_encode(SEXP buf){
  if(TYPEOF(buf) != RAWSXP)
    Rf_error("base64 buf must be raw");
  size_t len = Rf_length(buf);
  size_t outlen = 0;
  unsigned char * out = base64_encode(RAW(buf), len, &outlen);
  if(out == NULL)
    Rf_error("Error in base64 encode");
  SEXP res = PROTECT(allocVector(STRSXP, 1));
  SET_STRING_ELT(res, 0, mkCharLen((char*) out, outlen));
  free(out);
  UNPROTECT(1);
  return res;
}

SEXP R_base64_decode(SEXP buf){
  if(TYPEOF(buf) != RAWSXP)
    Rf_error("base64 buf must be raw");
  size_t len = Rf_length(buf);
  size_t outlen = 0;
  unsigned char * out = base64_decode(RAW(buf), len, &outlen);
  if(out == NULL)
    Rf_error("Error in base64 decode");
  SEXP res = allocVector(RAWSXP, outlen);
  memcpy(RAW(res), out, outlen);
  free(out);
  return res;
}
