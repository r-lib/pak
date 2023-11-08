// On non-windows platforms, we still need the C interfaces, but they simply
// give errors.

#ifndef _WIN32

#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif

#include <Rdefines.h>

#include "../errors.h"

SEXP processx_is_named_pipe_open(SEXP pipe_ext) {
    R_THROW_ERROR("processx_is_named_pipe_open only valid on Windows.");
    return R_NilValue;
}

SEXP processx_close_named_pipe(SEXP pipe_ext) {
    R_THROW_ERROR("processx_close_named_pipe only valid on Windows.");
    return R_NilValue;
}

SEXP processx_create_named_pipe(SEXP name, SEXP mode) {
    R_THROW_ERROR("processx_create_named_pipe only valid on Windows.");
    return R_NilValue;
}

SEXP processx_write_named_pipe(SEXP pipe_ext, SEXP text) {
    R_THROW_ERROR("processx_write_named_pipe only valid on Windows.");
    return R_NilValue;
}

#endif
