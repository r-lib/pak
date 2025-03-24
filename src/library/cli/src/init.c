
#include "cli.h"
#include "keypress.h"
#include "cleancall.h"

#include <R_ext/Rdynload.h>
#include <Rversion.h>

// Compile with `C_VISIBILITY = -fvisibility=hidden` if you link to
// this library
#include <R_ext/Visibility.h>
#define r_export attribute_visible extern

SEXP clic_unload(void) {
  clic_stop_thread();
  return R_NilValue;
}

SEXP clic_dataptr(SEXP x);

#ifdef GCOV_COMPILE

void __gcov_dump();
SEXP clic__gcov_flush() {
  REprintf("Flushing coverage info\n");
  __gcov_dump();
  return R_NilValue;
}

#else

SEXP clic__gcov_flush(void) {
  return R_NilValue;
}

#endif

static const R_CallMethodDef callMethods[]  = {
  CLEANCALL_METHOD_RECORD,

  { "clic_diff_chr",       (DL_FUNC) clic_diff_chr,       3 },
  { "clic_getppid",        (DL_FUNC) clic_getppid,        0 },
  { "clic_md5",            (DL_FUNC) clic_md5,            1 },
  { "clic_md5_raw",        (DL_FUNC) clic_md5_raw,        1 },
  { "clic_md5_file",       (DL_FUNC) clic_md5_file,       1 },
  { "clic_sha256",         (DL_FUNC) clic_sha256,         1 },
  { "clic_sha256_raw",     (DL_FUNC) clic_sha256_raw,     1 },
  { "clic_sha256_file",    (DL_FUNC) clic_sha256_file,    1 },
  { "clic_sha1",           (DL_FUNC) clic_sha1,           1 },
  { "clic_sha1_raw",       (DL_FUNC) clic_sha1_raw,       1 },
  { "clic_sha1_file",      (DL_FUNC) clic_sha1_file,      1 },
  { "clic_xxhash",         (DL_FUNC) clic_xxhash,         1 },
  { "clic_xxhash_raw",     (DL_FUNC) clic_xxhash_raw,     1 },
  { "clic_xxhash_file",    (DL_FUNC) clic_xxhash_file,    1 },
  { "clic_xxhash64",       (DL_FUNC) clic_xxhash64,       1 },
  { "clic_xxhash64_raw",   (DL_FUNC) clic_xxhash64_raw,   1 },
  { "clic_xxhash64_file",  (DL_FUNC) clic_xxhash64_file,  1 },
  { "clic_tty_size",       (DL_FUNC) clic_tty_size,       0 },
  { "clic_ansi_simplify",  (DL_FUNC) clic_ansi_simplify,  2 },
  { "clic_ansi_substr",    (DL_FUNC) clic_ansi_substr,    3 },
  { "clic_ansi_html",      (DL_FUNC) clic_ansi_html,      2 },
  { "clic_ansi_has_any",   (DL_FUNC) clic_ansi_has_any,   4 },
  { "clic_ansi_strip",     (DL_FUNC) clic_ansi_strip,     4 },
  { "clic_ansi_nchar",     (DL_FUNC) clic_ansi_nchar,     2 },

  { "clic_utf8_display_width",   (DL_FUNC) clic_utf8_display_width,   1 },
  { "clic_utf8_nchar_graphemes", (DL_FUNC) clic_utf8_nchar_graphemes, 1 },
  { "clic_utf8_substr",          (DL_FUNC) clic_utf8_substr,          3 },
  { "clic_utf8_graphemes",       (DL_FUNC) clic_utf8_graphemes,       1 },

  { "clic_dataptr",        (DL_FUNC) clic_dataptr,        1 },
  { "clic_start_thread",   (DL_FUNC) clic_start_thread,   3 },
  { "clic_stop_thread",    (DL_FUNC) clic_stop_thread,    0 },
  { "clic_tick_reset",     (DL_FUNC) clic_tick_reset,     0 },
  { "clic_tick_set",       (DL_FUNC) clic_tick_set,       2 },
  { "clic_tick_pause",     (DL_FUNC) clic_tick_pause,     1 },
  { "clic_tick_resume",    (DL_FUNC) clic_tick_resume,    1 },
  { "clic_unload",         (DL_FUNC) clic_unload,         0 },
  { "clic_get_time",       (DL_FUNC) clic_get_time,       0 },
  { "clic_make_timer",     (DL_FUNC) clic_make_timer,     0 },
  { "clic_update_due",     (DL_FUNC) clic_update_due,     0 },

  { "clic_progress_along", (DL_FUNC) clic_progress_along, 2 },

  { "clic__find_var",      (DL_FUNC) clic__find_var,      2 },
  { "clic__gcov_flush",    (DL_FUNC) clic__gcov_flush,    0 },

  { "clic_get_embedded_utf8", (DL_FUNC) clic_get_embedded_utf8, 0 },
  { "clic_set_embedded_utf8", (DL_FUNC) clic_set_embedded_utf8, 1 },

  { "glue_",               (DL_FUNC) glue_,               5 },
  { "trim_",               (DL_FUNC) trim_,               1 },

  { "clic_vt_output",      (DL_FUNC) clic_vt_output,      3 },

  { "cli_keypress",            (DL_FUNC) cli_keypress,            1 },

  { NULL, NULL, 0 }
};

#define RCC(fun) R_RegisterCCallable("cli", # fun, (DL_FUNC) fun);

r_export
void R_init_cli(DllInfo *dll) {
#if R_VERSION >= R_Version(3, 5, 0)
  cli_init_altrep(dll);
#endif

  R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);

  cleancall_fns_dot_call = Rf_findVar(Rf_install(".Call"), R_BaseEnv);

  RCC(cli_progress_add);
  RCC(cli_progress_bar);
  RCC(cli_progress_done);
  RCC(cli_progress_init_timer);
  RCC(cli_progress_num);
  RCC(cli_progress_set);
  RCC(cli_progress_set_clear);
  RCC(cli_progress_set_format);
  RCC(cli_progress_set_name);
  RCC(cli_progress_set_status);
  RCC(cli_progress_set_type);
  RCC(cli_progress_update);
  RCC(cli_progress_sleep);
}
