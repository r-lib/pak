
#include <R.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>

#include "date.h"

SEXP R_parse_date(SEXP chr, SEXP approx) {
  int i, no_dates = LENGTH(chr);
  SEXP result, *chr_ptr, class;
  unsigned long timestamp;
  int *result_ptr;
  int approx_;

  if (! IS_CHARACTER(chr)) {
    error("parse_date argument must be character");
  }

  if (! IS_LOGICAL(approx) || LENGTH(approx) != 1) {
    error("approx must the logical of length 1");
  }

  chr_ptr = STRING_PTR(chr);
  approx_ = LOGICAL(approx)[0];

  PROTECT(result = NEW_INTEGER(no_dates));
  result_ptr = INTEGER(result);

  for (i = 0; i < no_dates; i++) {
    int ret = 0;

    if (approx_) {
      timestamp = approxidate_careful(CHAR(*chr_ptr), &ret);
    } else {
      ret = parse_date_basic(CHAR(*chr_ptr), &timestamp, /* offset= */ 0);
    }

    if (ret) {
      *result_ptr = NA_INTEGER;
    } else {
      *result_ptr = (int) timestamp;
    }

    chr_ptr++;
    result_ptr++;
  }

  PROTECT(class = NEW_CHARACTER(2));
  SET_STRING_ELT(class, 0, mkChar("POSIXct"));
  SET_STRING_ELT(class, 1, mkChar("POSIXt"));
  SET_CLASS(result, class);

  UNPROTECT(2);
  return result;
}

static const R_CallMethodDef callMethods[]  = {
  {"R_parse_date", (DL_FUNC) &R_parse_date, 2},
  {NULL, NULL, 0}
};

void R_init_parsedate(DllInfo *dll) {
  R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
