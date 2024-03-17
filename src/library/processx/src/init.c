
#include "processx.h"
#include "cleancall.h"

#include <R_ext/Rdynload.h>
#include <R.h>

void R_init_processx_win(void);
void R_init_processx_unix(void);
SEXP processx__unload_cleanup(void);
SEXP run_testthat_tests(void);
SEXP processx__echo_on(void);
SEXP processx__echo_off(void);
SEXP processx__set_boot_time(SEXP);

#ifdef GCOV_COMPILE

void __gcov_dump();
SEXP gcov_flush() {
  REprintf("Flushing coverage info\n");
  __gcov_dump();
  return R_NilValue;
}

#else

SEXP gcov_flush(void) {
  return R_NilValue;
}

#endif

static const R_CallMethodDef callMethods[]  = {
  CLEANCALL_METHOD_RECORD,
  { "processx_exec",               (DL_FUNC) &processx_exec,              14 },
  { "processx_wait",               (DL_FUNC) &processx_wait,               3 },
  { "processx_is_alive",           (DL_FUNC) &processx_is_alive,           2 },
  { "processx_get_exit_status",    (DL_FUNC) &processx_get_exit_status,    2 },
  { "processx_signal",             (DL_FUNC) &processx_signal,             3 },
  { "processx_interrupt",          (DL_FUNC) &processx_interrupt,          2 },
  { "processx_kill",               (DL_FUNC) &processx_kill,               3 },
  { "processx_get_pid",            (DL_FUNC) &processx_get_pid,            1 },
  { "processx_create_time",        (DL_FUNC) &processx_create_time,        1 },
  { "processx_poll",               (DL_FUNC) &processx_poll,               3 },
  { "processx__process_exists",    (DL_FUNC) &processx__process_exists,    1 },
  { "processx__unload_cleanup",    (DL_FUNC) &processx__unload_cleanup,    0 },
  { "processx_is_named_pipe_open", (DL_FUNC) &processx_is_named_pipe_open, 1 },
  { "processx_close_named_pipe",   (DL_FUNC) &processx_close_named_pipe,   1 },
  { "processx_create_named_pipe",  (DL_FUNC) &processx_create_named_pipe,  2 },
  { "processx_write_named_pipe",   (DL_FUNC) &processx_write_named_pipe,   2 },
  { "processx__proc_start_time",   (DL_FUNC) &processx__proc_start_time,   1 },
  { "processx__set_boot_time",     (DL_FUNC) &processx__set_boot_time,     1 },

  { "processx_connection_create",     (DL_FUNC) &processx_connection_create,     2 },
  { "processx_connection_read_chars", (DL_FUNC) &processx_connection_read_chars, 2 },
  { "processx_connection_read_lines", (DL_FUNC) &processx_connection_read_lines, 2 },
  { "processx_connection_write_bytes",(DL_FUNC) &processx_connection_write_bytes,2 },
  { "processx_connection_file_name",  (DL_FUNC) &processx_connection_file_name,  1 },
  { "processx_connection_is_eof",     (DL_FUNC) &processx_connection_is_eof,     1 },
  { "processx_connection_close",      (DL_FUNC) &processx_connection_close,      1 },
  { "processx_connection_poll",       (DL_FUNC) &processx_connection_poll,       2 },

  { "processx_connection_create_fifo",
    (DL_FUNC) processx_connection_create_fifo,     5 },
  { "processx_connection_connect_fifo",
    (DL_FUNC) processx_connection_connect_fifo,    5 },
  { "processx_connection_create_socket",
    (DL_FUNC) processx_connection_create_socket,     2 },
  { "processx_connection_connect_socket",
    (DL_FUNC) processx_connection_connect_socket,    2 },
  { "processx_connection_accept_socket",
    (DL_FUNC) processx_connection_accept_socket,     1 },
  { "processx_connection_socket_state",
    (DL_FUNC) processx_connection_socket_state,      1 },
  { "processx_connection_create_pipepair",
    (DL_FUNC) processx_connection_create_pipepair, 2 },
  { "processx_connection_create_fd",  (DL_FUNC) &processx_connection_create_fd,  3 },
  { "processx_connection_create_file",
    (DL_FUNC) &processx_connection_create_file,    3 },
  { "processx_connection_set_stdout", (DL_FUNC) &processx_connection_set_stdout,  2 },
  { "processx_connection_set_stderr", (DL_FUNC) &processx_connection_set_stderr,  2 },
  { "processx_connection_get_fileno", (DL_FUNC) &processx_connection_get_fileno,  1 },
  { "processx_connection_disable_inheritance",
    (DL_FUNC) &processx_connection_disable_inheritance, 0 },
  { "processx_is_valid_fd",           (DL_FUNC) &processx_is_valid_fd,            1 },

  { "processx_disable_crash_dialog",  (DL_FUNC) &processx_disable_crash_dialog,   0 },
  { "processx_base64_encode", (DL_FUNC) &processx_base64_encode, 1 },
  { "processx_base64_decode", (DL_FUNC) &processx_base64_decode, 1 },
  { "processx__echo_on", (DL_FUNC) &processx__echo_on, 0 },
  { "processx__echo_off", (DL_FUNC) &processx__echo_off, 0 },

  { "gcov_flush", (DL_FUNC) gcov_flush, 0 },

  { NULL, NULL, 0 }
};

void R_init_processx(DllInfo *dll) {
  R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
  cleancall_fns_dot_call = Rf_findVar(Rf_install(".Call"), R_BaseEnv);
#ifdef _WIN32
  R_init_processx_win();
#else
  R_init_processx_unix();
#endif
}
