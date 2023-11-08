#include <Rinternals.h>
#include <string.h>

SEXP C_transpose_list(SEXP x, SEXP names) {
  size_t ncol = Rf_length(names);
  size_t nrow = Rf_length(x);
  SEXP out = PROTECT(allocVector(VECSXP, ncol));
  for(size_t i = 0; i < ncol; i++){
    const char * targetname = CHAR(STRING_ELT(names, i));
    SEXP col = PROTECT(allocVector(VECSXP, nrow));
    for(size_t j = 0; j < nrow; j++){
      //search for 'targetname' in each record j
      SEXP list = VECTOR_ELT(x, j);
      SEXP listnames = getAttrib(list, R_NamesSymbol);
      for(size_t k = 0; k < Rf_length(listnames); k++){
        if(!strcmp(CHAR(STRING_ELT(listnames, k)), targetname)){
          SET_VECTOR_ELT(col, j, VECTOR_ELT(list, k));
          break;
        }
      }
    }
    SET_VECTOR_ELT(out, i, col);
    UNPROTECT(1);
  }
  //setAttrib(out, R_NamesSymbol, names);
  UNPROTECT(1);
  return out;
}
