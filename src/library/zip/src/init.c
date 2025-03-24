
#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>
#define r_export attribute_visible extern

/* .Call calls */
extern SEXP R_zip_list(SEXP);
extern SEXP R_zip_zip(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP R_zip_unzip(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP R_make_big_file(SEXP, SEXP);
extern SEXP R_inflate(SEXP, SEXP, SEXP);
extern SEXP R_deflate(SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  { "R_zip_list",      (DL_FUNC) &R_zip_list,      1 },
  { "R_zip_zip",       (DL_FUNC) &R_zip_zip,       7 },
  { "R_zip_unzip",     (DL_FUNC) &R_zip_unzip,     5 },
  { "R_make_big_file", (DL_FUNC) &R_make_big_file, 2 },
  { "R_inflate",       (DL_FUNC) &R_inflate,       3 },
  { "R_deflate",       (DL_FUNC) &R_deflate,       4 },
  { NULL, NULL, 0 }
};

r_export void R_init_zip(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
