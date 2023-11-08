#include <Rdefines.h>
#include <Rinternals.h>
#include <stdlib.h>

/* a function to insert n spaces */
static void append_whitespace(char** cur, size_t n){
  memset(*cur, ' ', n);
  *cur += n;
}

/* add and increment */
static void append_text(char **cur, const char* val, int n){
  if(n < 0)
    n = strlen(val);
  memcpy(*cur, val, n);
  *cur += n;
}

/* collapse a json object with n spaces */
SEXP C_collapse_object_pretty(SEXP x, SEXP y, SEXP indent) {
  if (!isString(x) || !isString(y))
    error("x and y must character vectors.");

  int ni = asInteger(indent);
  if(ni == NA_INTEGER)
    error("indent must not be NA");

  int len = length(x);
  if (len != length(y))
    error("x and y must have same length.");

  //calculate required space
  size_t nchar_total = 0;
  for (int i=0; i<len; i++) {
    if(STRING_ELT(y, i) == NA_STRING) continue;
    nchar_total += strlen(translateCharUTF8(STRING_ELT(x, i)));
    nchar_total += strlen(translateCharUTF8(STRING_ELT(y, i)));
    nchar_total += ni + 6; //indent plus two extra spaces plus ": " and ",\n"
  }

  //final indent plus curly braces and linebreak and terminator
  nchar_total += (ni + 2 + 2);

  //allocate memory and create a cursor
  char *str = malloc(nchar_total);
  char *cursor = str;
  char **cur = &cursor;

  //init object
  append_text(cur, "{", 1);
  const char *start = *cur;

  //copy everything
  for (int i=0; i<len; i++) {
    if(STRING_ELT(y, i) == NA_STRING) continue;
    append_text(cur, "\n", 1);
    append_whitespace(cur, ni + 2);
    append_text(cur, translateCharUTF8(STRING_ELT(x, i)), -1);
    append_text(cur, ": ", 2);
    append_text(cur, translateCharUTF8(STRING_ELT(y, i)), -1);
    append_text(cur, ",", 1);
  }

  //finalize object
  if(cursor != start){
    cursor[-1] = '\n';
    append_whitespace(cur, ni);
  }
  append_text(cur, "}\0", 2);

  //encode as UTF8 string
  SEXP out = PROTECT(allocVector(STRSXP, 1));
  SET_STRING_ELT(out, 0, mkCharCE(str,  CE_UTF8));
  UNPROTECT(1);
  free(str);
  return out;
}

SEXP C_collapse_array_pretty_inner(SEXP x) {
  if (!isString(x))
    error("x must character vector.");

  //calculate required space
  size_t len = Rf_length(x);
  size_t nchar_total = 0;
  for (int i=0; i<len; i++) {
    nchar_total += strlen(translateCharUTF8(STRING_ELT(x, i)));
  }

  // n-1 ", " separators
  if(len){
    nchar_total += (len-1)*2;
  }

  //outer parentheses plus terminator
  nchar_total += 3;

  //allocate memory and create a cursor
  char *str = malloc(nchar_total);
  char *cursor = str;
  char **cur = &cursor;

  //init object
  append_text(cur, "[", 1);

  //copy everything
  for (int i=0; i<len; i++) {
    append_text(cur, translateCharUTF8(STRING_ELT(x, i)), -1);
    append_text(cur, ", ", 2);
  }

  //remove trailing ", "
  if(len) {
    cursor -= 2;
  }

  //finish up
  append_text(cur, "]\0", 2);

  //encode as UTF8 string
  SEXP out = PROTECT(allocVector(STRSXP, 1));
  SET_STRING_ELT(out, 0, mkCharCE(str,  CE_UTF8));
  UNPROTECT(1);
  free(str);
  return out;
}

SEXP C_collapse_array_pretty_outer(SEXP x, SEXP indent) {
  if (!isString(x))
    error("x must character vector.");

  int len = length(x);
  int ni = asInteger(indent);
  if(ni == NA_INTEGER)
    error("indent must not be NA");

  //calculate required space
  size_t nchar_total = 0;
  for (int i=0; i<len; i++) {
    nchar_total += strlen(translateCharUTF8(STRING_ELT(x, i)));
  }

  //for indent plus two extra spaces plus ",\n"
  nchar_total += len * (ni + 4);

  //outer parentheses plus final indent and linebreak and terminator
  nchar_total += ni + 4;

  //allocate memory and create a cursor
  char *str = malloc(nchar_total);
  char *cursor = str;
  char **cur = &cursor;

  //init object
  append_text(cur, "[", 1);
  const char *start = *cur;

  //copy everything
  for (int i=0; i<len; i++) {
    append_text(cur, "\n", 1);
    append_whitespace(cur, ni + 2);
    append_text(cur, translateCharUTF8(STRING_ELT(x, i)), -1);
    append_text(cur, ",", 1);
  }

  //remove trailing ", "
  if(cursor != start){
    cursor[-1] = '\n';
    append_whitespace(cur, ni);
  }

  //finish up
  append_text(cur, "]\0", 2);

  //encode as UTF8 string
  SEXP out = PROTECT(allocVector(STRSXP, 1));
  SET_STRING_ELT(out, 0, mkCharCE(str,  CE_UTF8));
  UNPROTECT(1);
  free(str);
  return out;
}
