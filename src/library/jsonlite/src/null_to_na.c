#include <Rdefines.h>
#include <Rinternals.h>
#include <stdlib.h>
#include <stdbool.h>

/*
This function takes a list and replaces all NULL values by NA.
In addition, it will parse strings "NA" "NaN" "Inf" and "-Inf",
unless there is at least one non-na string element in the list.
In that case converting to real values has no point because
unlist() will coerse them back into a string anyway.
*/

SEXP C_null_to_na(SEXP x) {
  int len = length(x);
  if(len == 0) return x;

  //null always turns into NA
  bool looks_like_character_vector = false;
  for (int i=0; i<len; i++) {
    if(VECTOR_ELT(x, i) == R_NilValue) {
      SET_VECTOR_ELT(x, i, ScalarLogical(NA_LOGICAL));
    } else if(!looks_like_character_vector && TYPEOF(VECTOR_ELT(x, i)) == STRSXP){
      if((strcmp("NA", CHAR(STRING_ELT(VECTOR_ELT(x, i), 0))) == 0) ||
         (strcmp("NaN", CHAR(STRING_ELT(VECTOR_ELT(x, i), 0))) == 0) ||
         (strcmp("Inf", CHAR(STRING_ELT(VECTOR_ELT(x, i), 0))) == 0) ||
         (strcmp("-Inf", CHAR(STRING_ELT(VECTOR_ELT(x, i), 0))) == 0)) continue;
      looks_like_character_vector = true;
    }
  }

  // if this is a character vector, do not parse NA strings.
  if(looks_like_character_vector) return(x);

  //parse NA strings
  for (int i=0; i<len; i++) {
    if(TYPEOF(VECTOR_ELT(x, i)) == STRSXP){
      if(strcmp("NA", CHAR(STRING_ELT(VECTOR_ELT(x, i), 0))) == 0) {
        SET_VECTOR_ELT(x, i, ScalarLogical(NA_LOGICAL));
        continue;
      }
      if(strcmp("NaN", CHAR(STRING_ELT(VECTOR_ELT(x, i), 0))) == 0) {
        SET_VECTOR_ELT(x, i, ScalarReal(R_NaN));
        continue;
      }
      if(strcmp("Inf", CHAR(STRING_ELT(VECTOR_ELT(x, i), 0))) == 0) {
        SET_VECTOR_ELT(x, i, ScalarReal(R_PosInf));
        continue;
      }
      if(strcmp("-Inf", CHAR(STRING_ELT(VECTOR_ELT(x, i), 0))) == 0) {
        SET_VECTOR_ELT(x, i, ScalarReal(R_NegInf));
        continue;
      }
    }
  }

  //return updated list
  return x;
}
