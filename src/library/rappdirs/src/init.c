#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP win_path_(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"win_path_", (DL_FUNC) &win_path_, 1},
    {NULL, NULL, 0}
};

void R_init_rappdirs(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
