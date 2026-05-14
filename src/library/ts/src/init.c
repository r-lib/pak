#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

#include "cleancall.h"

SEXP code_query(SEXP tree, SEXP query);
SEXP s_expr(SEXP tree);
SEXP parse(SEXP input, SEXP rlanguage, SEXP rranges);

SEXP glue(SEXP x, SEXP f, SEXP open_arg, SEXP close_arg, SEXP cli_arg);
SEXP trim(SEXP x);

static const R_CallMethodDef callMethods[]  = {
  CLEANCALL_METHOD_RECORD,
  { "code_query",        (DL_FUNC) &code_query,        2 },
  { "s_expr",            (DL_FUNC) &s_expr,            1 },
  { "parse",             (DL_FUNC) &parse,             3 },
  { "glue",              (DL_FUNC) &glue,              5 },
  { "trim",              (DL_FUNC) &trim,              1 },
  { NULL, NULL, 0 }
};

void R_init_ts(DllInfo *dll) {
  R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
  cleancall_init();
}
