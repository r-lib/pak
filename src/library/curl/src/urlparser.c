#include "curl-common.h"

static SEXP make_url_names(void){
  SEXP names = PROTECT(Rf_allocVector(STRSXP, 9));
  SET_STRING_ELT(names, 0, Rf_mkChar("url"));
  SET_STRING_ELT(names, 1, Rf_mkChar("scheme"));
  SET_STRING_ELT(names, 2, Rf_mkChar("host"));
  SET_STRING_ELT(names, 3, Rf_mkChar("port"));
  SET_STRING_ELT(names, 4, Rf_mkChar("path"));
  SET_STRING_ELT(names, 5, Rf_mkChar("query"));
  SET_STRING_ELT(names, 6, Rf_mkChar("fragment"));
  SET_STRING_ELT(names, 7, Rf_mkChar("user"));
  SET_STRING_ELT(names, 8, Rf_mkChar("password"));
  UNPROTECT(1);
  return names;
}

static void fail_if(CURLUcode err){
  if(err != CURLUE_OK)
#ifdef HAS_CURL_PARSER_STRERROR
    Rf_error("Failed to parse URL: %s", curl_url_strerror(err));
#else
    Rf_error("Failed to parse URL: error code %d", err);
#endif
}

static SEXP get_field(CURLU *h, CURLUPart part, CURLUcode field_missing){
  char *str = NULL;
  SEXP field = NULL;
  CURLUcode err = curl_url_get(h, part, &str, 0);
  if(err == field_missing && err != CURLUE_OK){
    field = R_NilValue;
  } else {
    fail_if(err);
    field = make_string(str);
  }
  curl_free(str);
  return field;
}

/* We use CURLU_NON_SUPPORT_SCHEME to make results consistent across different libcurl configurations and Ada URL
 * We use CURLU_URLENCODE to normalize input URLs and also be consistent with Ada URL */


static void set_url(CURLU *h, const char *str, int default_scheme){
  int flags = CURLU_NON_SUPPORT_SCHEME | CURLU_URLENCODE | (default_scheme * CURLU_DEFAULT_SCHEME);
  fail_if(curl_url_set(h, CURLUPART_URL, str, flags));
}

SEXP R_parse_url(SEXP url, SEXP baseurl, SEXP default_https) {
  CURLU *h = curl_url();
  int default_scheme = Rf_length(default_https) && Rf_asLogical(default_https);
  if(Rf_length(baseurl)){
    set_url(h, get_string(baseurl), default_scheme);
  }
  set_url(h, get_string(url), default_scheme);
  SEXP out = PROTECT(Rf_allocVector(VECSXP, 9));
  SET_VECTOR_ELT(out, 0, get_field(h, CURLUPART_URL, CURLUE_OK));
  SET_VECTOR_ELT(out, 1, get_field(h, CURLUPART_SCHEME, CURLUE_NO_SCHEME));
  SET_VECTOR_ELT(out, 2, get_field(h, CURLUPART_HOST, CURLUE_NO_HOST));
  SET_VECTOR_ELT(out, 3, get_field(h, CURLUPART_PORT, CURLUE_NO_PORT));
  SET_VECTOR_ELT(out, 4, get_field(h, CURLUPART_PATH, CURLUE_OK));
  SET_VECTOR_ELT(out, 5, get_field(h, CURLUPART_QUERY, CURLUE_NO_QUERY));
  SET_VECTOR_ELT(out, 6, get_field(h, CURLUPART_FRAGMENT, CURLUE_NO_FRAGMENT));
  SET_VECTOR_ELT(out, 7, get_field(h, CURLUPART_USER, CURLUE_NO_USER));
  SET_VECTOR_ELT(out, 8, get_field(h, CURLUPART_PASSWORD, CURLUE_NO_PASSWORD));
  curl_url_cleanup(h);
  Rf_setAttrib(out, R_NamesSymbol, make_url_names());
  UNPROTECT(1);
  return out;
}

static void set_value(CURLU *h, CURLUPart part, SEXP value){
  if(Rf_length(value) && Rf_isString(value)){
    if(STRING_ELT(value, 0) == NA_STRING || Rf_length(STRING_ELT(value, 0)) == 0){
      fail_if(curl_url_set(h, part, NULL, 0));
    } else if(Rf_inherits(value, "AsIs")){
      fail_if(curl_url_set(h, part, get_string(value), 0));
    } else {
      fail_if(curl_url_set(h, part, get_string(value), CURLU_NON_SUPPORT_SCHEME | CURLU_URLENCODE));
    }
  }
}

SEXP R_modify_url(SEXP url, SEXP scheme, SEXP host, SEXP port, SEXP path, SEXP query, SEXP fragment, SEXP user, SEXP password){
  CURLU *h = curl_url();
  set_value(h, CURLUPART_URL, url);
  set_value(h, CURLUPART_SCHEME, scheme);
  set_value(h, CURLUPART_HOST, host);
  set_value(h, CURLUPART_PORT, port);
  set_value(h, CURLUPART_PATH, path);
  set_value(h, CURLUPART_QUERY, query);
  set_value(h, CURLUPART_FRAGMENT, fragment);
  set_value(h, CURLUPART_USER, user);
  set_value(h, CURLUPART_PASSWORD, password);
  char *str = NULL;
  fail_if(curl_url_get(h, CURLUPART_URL, &str, 0));
  SEXP out = make_string(str);
  curl_free(str);
  return out;
}
