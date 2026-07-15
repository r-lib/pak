
#include <R.h>
#include <Rinternals.h>
#include <Rversion.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>
#define r_export attribute_visible extern

#include "cleancall.h"

/* .Call calls */
extern SEXP R_zip_list(SEXP, SEXP);
extern SEXP R_zip_zip(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP R_zip_unzip(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP R_make_big_file(SEXP, SEXP);
extern SEXP R_inflate(SEXP, SEXP, SEXP, SEXP);
extern SEXP R_deflate(SEXP, SEXP, SEXP, SEXP);
extern SEXP R_zip_cp437_to_utf8(SEXP);
extern SEXP R_threaded_unzip(SEXP, SEXP, SEXP, SEXP);
extern SEXP R_crypto_pbkdf2_sha1(SEXP, SEXP, SEXP, SEXP);
extern SEXP R_crypto_hmac_sha1(SEXP, SEXP);
extern SEXP R_crypto_aes_ctr(SEXP, SEXP);
extern SEXP R_crypto_winzip_keys(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  CLEANCALL_METHOD_RECORD,
  { "R_zip_list",           (DL_FUNC) &R_zip_list,           2 },
  { "R_zip_zip",            (DL_FUNC) &R_zip_zip,           10 },
  { "R_zip_unzip",          (DL_FUNC) &R_zip_unzip,          8 },
  { "R_make_big_file",      (DL_FUNC) &R_make_big_file,      2 },
  { "R_inflate",            (DL_FUNC) &R_inflate,            4 },
  { "R_deflate",            (DL_FUNC) &R_deflate,            4 },
  { "R_zip_cp437_to_utf8",  (DL_FUNC) &R_zip_cp437_to_utf8,  1 },
  { "R_threaded_unzip",     (DL_FUNC) &R_threaded_unzip,     4 },
  { "R_crypto_pbkdf2_sha1", (DL_FUNC) &R_crypto_pbkdf2_sha1, 4 },
  { "R_crypto_hmac_sha1",   (DL_FUNC) &R_crypto_hmac_sha1,   2 },
  { "R_crypto_aes_ctr",     (DL_FUNC) &R_crypto_aes_ctr,     2 },
  { "R_crypto_winzip_keys", (DL_FUNC) &R_crypto_winzip_keys, 3 },
  { NULL, NULL, 0 }
};

#if (R_VERSION < R_Version(4, 5, 0))
#define R_getVar(x,y,z) Rf_findVar(x,y)
#endif

r_export void R_init_zip(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);

  cleancall_fns_dot_call = R_getVar(Rf_install(".Call"), R_BaseEnv, TRUE);
}
