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

#ifdef USE_ADA_PARSER
#include "ada_c.h"
static SEXP get_ada_field(ada_string val){
  if(val.length == 0)
    return R_NilValue;
  return Rf_ScalarString(Rf_mkCharLenCE(val.data, val.length, CE_UTF8));
}

SEXP R_parse_url(SEXP url, SEXP baseurl) {
  ada_url *result = Rf_length(baseurl) == 0 ?
    ada_parse(get_string(url), Rf_length(STRING_ELT(url, 0))) :
    ada_parse_with_base(get_string(url), Rf_length(STRING_ELT(url, 0)), get_string(baseurl), Rf_length(STRING_ELT(baseurl, 0)));
  if(!ada_is_valid(result)){
    ada_free(result);
    Rf_error("ADA failed to parse URL");
  }
  SEXP out = PROTECT(Rf_allocVector(VECSXP, 9));
  SET_VECTOR_ELT(out, 0, get_ada_field(ada_get_href(result)));
  SET_VECTOR_ELT(out, 1, get_ada_field(ada_get_protocol(result)));
  SET_VECTOR_ELT(out, 2, get_ada_field(ada_get_hostname(result)));
  SET_VECTOR_ELT(out, 3, get_ada_field(ada_get_port(result)));
  SET_VECTOR_ELT(out, 4, get_ada_field(ada_get_pathname(result)));
  SET_VECTOR_ELT(out, 5, get_ada_field(ada_get_search(result)));
  SET_VECTOR_ELT(out, 6, get_ada_field(ada_get_hash(result)));
  SET_VECTOR_ELT(out, 7, get_ada_field(ada_get_username(result)));
  SET_VECTOR_ELT(out, 8, get_ada_field(ada_get_password(result)));
  Rf_setAttrib(out, R_NamesSymbol, make_url_names());
  Rf_setAttrib(out, R_ClassSymbol, Rf_mkString("ada"));
  UNPROTECT(1);
  ada_free(result);
  return out;
}

#elif defined(HAS_CURL_PARSER)
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
SEXP R_parse_url(SEXP url, SEXP baseurl) {
  CURLU *h = curl_url();
  if(Rf_length(baseurl)){
    fail_if(curl_url_set(h, CURLUPART_URL, get_string(baseurl), CURLU_NON_SUPPORT_SCHEME | CURLU_URLENCODE));
  }
  fail_if(curl_url_set(h, CURLUPART_URL, get_string(url), CURLU_NON_SUPPORT_SCHEME | CURLU_URLENCODE));
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
#else
SEXP R_parse_url(SEXP url, SEXP baseurl) {
  Rf_error("URL parser not suppored, this libcurl is too old");
}
#endif
