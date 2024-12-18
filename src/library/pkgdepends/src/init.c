#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

#include "cleancall.h"

SEXP code_query(SEXP input, SEXP pattern, SEXP rlanguage, SEXP ranges);
SEXP code_query_path(SEXP path, SEXP pattern, SEXP rlanguage, SEXP ranges);
SEXP s_expr(SEXP input, SEXP rlanguage, SEXP ranges);

SEXP yaml_parse_scalar(SEXP x);

static const R_CallMethodDef callMethods[]  = {
  CLEANCALL_METHOD_RECORD,
  { "code_query",        (DL_FUNC) &code_query,        4 },
  { "code_query_path",   (DL_FUNC) &code_query_path,   4 },
  { "s_expr",            (DL_FUNC) &s_expr,            3 },
  { "yaml_parse_scalar", (DL_FUNC) &yaml_parse_scalar, 1 },
  { NULL, NULL, 0 }
};

void R_init_pkgdepends(DllInfo *dll) {
  R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
  cleancall_init();
}
