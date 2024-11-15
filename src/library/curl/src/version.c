#include "curl-common.h"

SEXP R_curl_version(void) {
  /* retrieve info from curl */
  const curl_version_info_data *data = curl_version_info(CURLVERSION_NOW);

  /* put stuff in a list */
  SEXP list = PROTECT(Rf_allocVector(VECSXP, 12));
  SET_VECTOR_ELT(list, 0, make_string(data->version));
  SET_VECTOR_ELT(list, 1, make_string(LIBCURL_VERSION));
  SET_VECTOR_ELT(list, 2, make_string(data->ssl_version));
  SET_VECTOR_ELT(list, 3, make_string(data->libz_version));
  SET_VECTOR_ELT(list, 4, make_string(data->libssh_version));
  SET_VECTOR_ELT(list, 5, make_string(data->libidn));
  SET_VECTOR_ELT(list, 6, make_string(data->host));

  /* create vector of protocols */
  int len = 0;
  const char *const * temp = data->protocols;
  while(*temp++) len++;
  SEXP protocols = PROTECT(Rf_allocVector(STRSXP, len));
  for (int i = 0; i < len; i++){
    SET_STRING_ELT(protocols, i, Rf_mkChar(*(data->protocols + i)));
  }
  SET_VECTOR_ELT(list, 7, protocols);

  /* add list names */
  SEXP names = PROTECT(Rf_allocVector(STRSXP, 12));
  SET_STRING_ELT(names, 0, Rf_mkChar("version"));
  SET_STRING_ELT(names, 1, Rf_mkChar("headers"));
  SET_STRING_ELT(names, 2, Rf_mkChar("ssl_version"));
  SET_STRING_ELT(names, 3, Rf_mkChar("libz_version"));
  SET_STRING_ELT(names, 4, Rf_mkChar("libssh_version"));
  SET_STRING_ELT(names, 5, Rf_mkChar("libidn_version"));
  SET_STRING_ELT(names, 6, Rf_mkChar("host"));
  SET_STRING_ELT(names, 7, Rf_mkChar("protocols"));
  SET_STRING_ELT(names, 8, Rf_mkChar("ipv6"));
  SET_STRING_ELT(names, 9, Rf_mkChar("http2"));
  SET_STRING_ELT(names, 10, Rf_mkChar("idn"));
  SET_STRING_ELT(names, 11, Rf_mkChar("url_parser"));
  Rf_setAttrib(list, R_NamesSymbol, names);

  #ifdef CURL_VERSION_IPV6
  SET_VECTOR_ELT(list, 8, Rf_ScalarLogical(data->features & CURL_VERSION_IPV6));
  #else
  SET_VECTOR_ELT(list, 8, Rf_ScalarLogical(0));
  #endif

  #ifdef CURL_VERSION_HTTP2
  SET_VECTOR_ELT(list, 9, Rf_ScalarLogical(data->features & CURL_VERSION_HTTP2));
  #else
  SET_VECTOR_ELT(list, 9, Rf_ScalarLogical(0));
  #endif

  #ifdef CURL_VERSION_IDN
    SET_VECTOR_ELT(list, 10, Rf_ScalarLogical(data->features & CURL_VERSION_IDN));
  #else
    SET_VECTOR_ELT(list, 10, Rf_ScalarLogical(0));
  #endif

  #ifdef HAS_CURL_PARSER
    SET_VECTOR_ELT(list, 11, Rf_ScalarLogical(1));
  #else
    SET_VECTOR_ELT(list, 11, Rf_ScalarLogical(0));
  #endif

  /* return */
  UNPROTECT(3);
  return list;
}
