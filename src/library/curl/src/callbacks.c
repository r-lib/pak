#include "curl-common.h"

int R_curl_callback_progress(SEXP fun,
                             double dltotal, double dlnow,
                             double ultotal, double ulnow) {

  SEXP down = PROTECT(Rf_allocVector(REALSXP, 2));
  REAL(down)[0] = dltotal;
  REAL(down)[1] = dlnow;

  SEXP up = PROTECT(Rf_allocVector(REALSXP, 2));
  REAL(up)[0] = ultotal;
  REAL(up)[1] = ulnow;

  SEXP call = PROTECT(Rf_lang3(fun, down, up));
  int ok;
  SEXP res = PROTECT(R_tryEval(call, R_GlobalEnv, &ok));

  if (ok != 0) {
    UNPROTECT(4);
    return CURL_READFUNC_ABORT;
  }

  if (TYPEOF(res) != LGLSXP || Rf_length(res) != 1) {
    UNPROTECT(4);
    Rf_warning("progress callback must return boolean");
    return 0;
  }

  int out = Rf_asLogical(res);
  UNPROTECT(4);
  return !out;
}

size_t R_curl_callback_read(char *buffer, size_t size, size_t nitems, SEXP fun) {
  SEXP nbytes = PROTECT(Rf_ScalarInteger(size * nitems));
  SEXP call = PROTECT(Rf_lang2(fun, nbytes));

  int ok;
  SEXP res = PROTECT(R_tryEval(call, R_GlobalEnv, &ok));

  if (ok != 0) {
    UNPROTECT(3);
    return CURL_READFUNC_ABORT;
  }

  if (TYPEOF(res) != RAWSXP) {
    UNPROTECT(3);
    Rf_warning("read callback must raw vector");
    return CURL_READFUNC_ABORT;
  }

  size_t bytes_read = Rf_length(res);
  memcpy(buffer, RAW(res), bytes_read);

  UNPROTECT(3);
  return bytes_read;
}

/* origin is always SEEK_SET in libcurl, not really useful to pass on */
int R_curl_callback_seek(SEXP fun, curl_off_t offset, int origin){
  SEXP soffset = PROTECT(Rf_ScalarReal(offset));
  SEXP call = PROTECT(Rf_lang2(fun, soffset));
  int ok;
  R_tryEval(call, R_GlobalEnv, &ok);
  UNPROTECT(2);
  return ok ? CURL_SEEKFUNC_FAIL : CURL_SEEKFUNC_OK;
}

int R_curl_callback_debug(CURL *handle, curl_infotype type_, char *data,
                          size_t size, SEXP fun) {

  /* wrap type and msg into R types */
  SEXP type = PROTECT(Rf_ScalarInteger(type_));
  SEXP msg = PROTECT(Rf_allocVector(RAWSXP, size));
  memcpy(RAW(msg), data, size);

  /* call the R function */
  SEXP call = PROTECT(Rf_lang3(fun, type, msg));
  R_tryEval(call, R_GlobalEnv, NULL);

  UNPROTECT(3);
  // Debug function must always return 0
  return 0;
}


int R_curl_callback_xferinfo(SEXP fun,
                             curl_off_t  dltotal, curl_off_t  dlnow,
                             curl_off_t  ultotal, curl_off_t  ulnow) {
  return R_curl_callback_progress(fun, dltotal, dlnow, ultotal, ulnow);
}

/* See the man page for ?ssl_ctx in the openssl R package for examples*/
int R_curl_callback_ssl_ctx(CURL *handle, void *ssl_ctx, SEXP fun){
  SEXP ptr = PROTECT(R_MakeExternalPtr(ssl_ctx, R_NilValue, R_NilValue));
  Rf_setAttrib(ptr, R_ClassSymbol, Rf_mkString("ssl_ctx"));
  SEXP call = PROTECT(Rf_lang2(fun, ptr));
  int err = 0;
  R_tryEval(call, R_GlobalEnv, &err);
  UNPROTECT(2);
  return err;
}
