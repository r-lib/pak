#include <Rinternals.h>

#include <errno.h>
#include <string.h>

static int total_open_writers = 0;

void fin_file_writer(SEXP ptr){
  FILE *fp = R_ExternalPtrAddr(ptr);
  if(fp != NULL){
    fclose(fp);
    R_ClearExternalPtr(ptr);
    total_open_writers--;
  }
}

SEXP R_write_file_writer(SEXP ptr, SEXP buf, SEXP close){
  FILE *fp = R_ExternalPtrAddr(ptr);
  size_t len = 0;
  if(Rf_length(buf)){
    if(fp == NULL){
      SEXP path = VECTOR_ELT(R_ExternalPtrTag(ptr), 0);
      SEXP append = VECTOR_ELT(R_ExternalPtrTag(ptr), 1);
      fp = fopen(CHAR(STRING_ELT(path, 0)), Rf_asLogical(append) ? "ab" : "wb");
      if(!fp){
        const char *errmsg =  strerror(errno);
        Rf_error("Failed to open file: %s\n%s", CHAR(STRING_ELT(path, 0)), errmsg);
      }
      R_SetExternalPtrAddr(ptr, fp);
      total_open_writers++;
    }
    len = fwrite(RAW(buf), 1, Rf_xlength(buf), fp);
  }
  if(Rf_asLogical(close)){
    fin_file_writer(ptr);
  } else if(Rf_length(buf)) {
    fflush(fp);
  }
  return ScalarInteger(len);
}

SEXP R_new_file_writer(SEXP opts){
  SEXP ptr = PROTECT(R_MakeExternalPtr(NULL, opts, R_NilValue));
  R_RegisterCFinalizerEx(ptr, fin_file_writer, TRUE);
  setAttrib(ptr, R_ClassSymbol, mkString("file_writer"));
  UNPROTECT(1);
  return ptr;
}

SEXP R_total_writers(void){
  return(ScalarInteger(total_open_writers));
}
