#include <curl/curl.h>
#include <Rinternals.h>

SEXP R_curl_getdate(SEXP datestring) {
  if(!isString(datestring))
    error("Argument 'datestring' must be string.");

  int len = length(datestring);
  SEXP out = PROTECT(allocVector(INTSXP, len));

  for(int i = 0; i < len; i++){
    time_t date = curl_getdate(CHAR(STRING_ELT(datestring, i)), NULL);
    INTEGER(out)[i] = date < 0 ? NA_INTEGER : (int) date;
  }
  UNPROTECT(1);
  return out;
}
