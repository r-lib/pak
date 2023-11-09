#include <Rdefines.h>
#include <Rinternals.h>
#include <stdlib.h>
#include <stdbool.h>

SEXP C_is_scalarlist(SEXP x) {

  bool is_scalarlist = true;
  if (TYPEOF(x) != VECSXP){
    is_scalarlist = false;
  } else {
    SEXP el;
    int len = length(x);
    for (int i=0; i<len; i++) {
      el = VECTOR_ELT(x, i);
      switch(TYPEOF(el)) {
        case LGLSXP:
        case INTSXP:
        case REALSXP:
        case STRSXP:
        case NILSXP:
        case RAWSXP: //not used but for compatibility with is.atomic
        case CPLXSXP: //not used but for compatibility with is.atomic
          if(length(el) < 2) continue;
          //else fall through
        default:
          is_scalarlist = false;
          break;
      }
    }
  }

  //get character encoding from first element
  return ScalarLogical(is_scalarlist);
}
