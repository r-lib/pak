#include <curl/curl.h>
#include <Rinternals.h>

SEXP R_curl_escape(SEXP url, SEXP unescape_) {
  if (!Rf_isString(url))
    Rf_error("`url` must be a character vector.");
  CURL *handle = curl_easy_init();
  int n = Rf_length(url);
  SEXP output = PROTECT(Rf_allocVector(STRSXP, n));

  for (int i = 0; i < n; i++) {
    const char *input = CHAR(STRING_ELT(url, i));
    char *out = Rf_asLogical(unescape_) ?
      curl_easy_unescape(handle, input, 0, NULL) : curl_easy_escape(handle, input, 0);
    if(out != NULL){
      SET_STRING_ELT(output, i, Rf_mkCharCE(out, CE_UTF8));
      curl_free(out);
    }
  }
  curl_easy_cleanup(handle);
  UNPROTECT(1);
  return output;
}
