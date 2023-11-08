#include <Rdefines.h>
#include <Rinternals.h>
#include <stdlib.h>
#include <stdbool.h>

// .Call("C_is_namedlist", PACKAGE = "jsonlite", 123)
static bool is_namedlist(SEXP x) {
  if(TYPEOF(x) == VECSXP && getAttrib(x, R_NamesSymbol) != R_NilValue){
    return true;
  }
  return false;
}

static bool is_unnamedlist(SEXP x) {
  if(TYPEOF(x) == VECSXP && getAttrib(x, R_NamesSymbol) == R_NilValue){
    return true;
  }
  return false;
}

static bool is_namedlist_or_null(SEXP x){
  return (is_namedlist(x) || (x == R_NilValue));
}

static bool is_recordlist(SEXP x){
  bool at_least_one_object = false;
  if(!is_unnamedlist(x)){
    return false;
  }
  int len = length(x);
  if(len < 1){
    return false;
  }
  for (int i=0; i<len; i++) {
    if(!is_namedlist_or_null(VECTOR_ELT(x, i))) return false;
    if(!at_least_one_object && is_namedlist(VECTOR_ELT(x, i))) {
      at_least_one_object = true;
    }
  }
  return at_least_one_object;
}

SEXP C_is_recordlist(SEXP x){
  return ScalarLogical(is_recordlist(x));
}
