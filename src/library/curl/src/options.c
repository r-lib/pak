#include "curl-common.h"

SEXP R_option_types(void){
#ifdef HAS_CURL_EASY_OPTION
  int len = 0;
  const struct curl_easyoption *o = NULL;
  while((o = curl_easy_option_next(o))){
    if(!(o->flags & CURLOT_FLAG_ALIAS))
      len++;
  }
  SEXP names = PROTECT(Rf_allocVector(STRSXP, len));
  SEXP values = PROTECT(Rf_allocVector(INTSXP, len));
  SEXP types = PROTECT(Rf_allocVector(INTSXP, len));
  int i = 0;
  while((o = curl_easy_option_next(o))){
    if(!(o->flags & CURLOT_FLAG_ALIAS)){
      SET_STRING_ELT(names, i, Rf_mkChar(o->name ? o->name : "???"));
      INTEGER(values)[i] = o->id;
      INTEGER(types)[i] = o->type;
      i++;
    }
  }

  SEXP out = PROTECT(Rf_allocVector(VECSXP, 3));
  SEXP listnms = PROTECT(Rf_allocVector(STRSXP, 3));
  Rf_setAttrib(out, R_NamesSymbol, listnms);
  SET_VECTOR_ELT(out, 0, names);
  SET_VECTOR_ELT(out, 1, values);
  SET_VECTOR_ELT(out, 2, types);
  SET_STRING_ELT(listnms, 0, Rf_mkChar("name"));
  SET_STRING_ELT(listnms, 1, Rf_mkChar("value"));
  SET_STRING_ELT(listnms, 2, Rf_mkChar("type"));
  UNPROTECT(5);
  return out;
#else
  return R_NilValue;
#endif
}
