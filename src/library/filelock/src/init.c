
#include "filelock.h"

static const R_CallMethodDef callMethods[]  = {
  { "filelock_lock",        (DL_FUNC) &filelock_lock,         3 },
  { "filelock_unlock",      (DL_FUNC) &filelock_unlock,       1 },
  { "filelock_is_unlocked", (DL_FUNC) &filelock_is_unlocked,  1 },
  { NULL, NULL, 0 }
};

void R_init_filelock(DllInfo *dll) {
  R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
