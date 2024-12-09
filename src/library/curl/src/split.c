#include <Rinternals.h>
#include <string.h>

SEXP R_split_string(SEXP string, SEXP split){
  const char * str = CHAR(STRING_ELT(string, 0));
  cetype_t enc = Rf_getCharCE(STRING_ELT(string, 0));
  const char * cut = CHAR(STRING_ELT(split, 0));
  char * out = strstr(str, cut);
  if(!out)
    return string;
  SEXP res = PROTECT(allocVector(STRSXP, 2));
  SET_STRING_ELT(res, 0, mkCharLenCE(str, out - str, enc));
  SET_STRING_ELT(res, 1, mkCharCE(out + strlen(cut), enc));
  UNPROTECT(1);
  return res;
}
