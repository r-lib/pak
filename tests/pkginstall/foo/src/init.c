#include <R.h>
#include <R_ext/Rdynload.h>
#include <Rinternals.h>
#include <stdlib.h>  // for NULL

/* .Call calls */
extern SEXP foo_();

static const R_CallMethodDef CallEntries[] = {{"foo_", (DL_FUNC)&foo_, 0},
                                              {NULL, NULL, 0}};

void R_init_foo(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}

SEXP foo_() {
  return R_NilValue;
}
