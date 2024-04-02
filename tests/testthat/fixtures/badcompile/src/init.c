#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#define r_export attribute_visible extern

static const R_CallMethodDef CallEntries[] = {
  { NULL, NULL, 0 }
};

r_export void R_init_zip(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
