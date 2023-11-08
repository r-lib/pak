#include <Rdefines.h>
#include <Rinternals.h>
#include <stdlib.h>

SEXP C_collapse_array(SEXP x) {
  if (!isString(x))
    error("x must be a character vector.");

  int len = length(x);
  size_t nchar_total = 0;

  for (int i=0; i<len; i++) {
    nchar_total += strlen(translateCharUTF8(STRING_ELT(x, i)));
  }

  char *s = malloc(nchar_total+len+3); //if len is 0, we need at least: '[]\0'
  char *olds = s;
  size_t size;

  for (int i=0; i<len; i++) {
    s[0] = ',';
    size = strlen(translateCharUTF8(STRING_ELT(x, i)));
    memcpy(++s, translateCharUTF8(STRING_ELT(x, i)), size);
    s += size;
  }
  if(olds == s) s++;
  olds[0] = '[';
  s[0] = ']';
  s[1] = '\0';

  //get character encoding from first element
  SEXP out = PROTECT(allocVector(STRSXP, 1));
  SET_STRING_ELT(out, 0, mkCharCE(olds,  CE_UTF8));
  UNPROTECT(1);
  free(olds);
  return out;
}
