
#ifndef R_PS_COMMON_H
#define R_PS_COMMON_H

#define R_USE_C99_IN_CXX 1
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#include "ps-internal.h"

/* ---------------------------------------------------------------------*/
/* Internals                                                            */
/* ---------------------------------------------------------------------*/

extern int PS__TESTING;
extern int PS__DEBUG;

// a signaler for connections without an actual status
static const int PS__CONN_NONE = 128;

void ps__set_testing(void);
void ps__debug(const char* format, ...);
void R_init_ps(DllInfo *dll);

#endif // PSUTIL_PSUTIL_COMMON_H
