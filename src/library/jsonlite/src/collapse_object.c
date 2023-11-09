#include <Rdefines.h>
#include <Rinternals.h>
#include <stdlib.h>

SEXP C_collapse_object(SEXP x, SEXP y) {
  if (!isString(x) || !isString(y))
    error("x and y must character vectors.");

  int len = length(x);
  if (len != length(y))
    error("x and y must same length.");

  size_t nchar_total = 0;

  for (int i=0; i<len; i++) {
    if(STRING_ELT(y, i) == NA_STRING) continue;
    nchar_total += strlen(translateCharUTF8(STRING_ELT(x, i)));
    nchar_total += strlen(translateCharUTF8(STRING_ELT(y, i)));
    nchar_total += 2;
  }

  char *s = malloc(nchar_total + 3); //if len is 0, we need at least: '{}\0'
  char *olds = s;
  size_t size;

  for (int i=0; i<len; i++) {
    if(STRING_ELT(y, i) == NA_STRING) continue;
    s[0] = ',';
    //add x
    size = strlen(translateCharUTF8(STRING_ELT(x, i)));
    memcpy(++s, translateCharUTF8(STRING_ELT(x, i)), size);
    s += size;

    //add :
    s[0] = ':';

    //add y
    size = strlen(translateCharUTF8(STRING_ELT(y, i)));
    memcpy(++s, translateCharUTF8(STRING_ELT(y, i)), size);
    s += size;
  }
  if(olds == s) s++;
  olds[0] = '{';
  s[0] = '}';
  s[1] = '\0';

  //get character encoding from first element
  SEXP out = PROTECT(allocVector(STRSXP, 1));
  SET_STRING_ELT(out, 0, mkCharCE(olds,  CE_UTF8));
  UNPROTECT(1);
  free(olds);
  return out;
}
