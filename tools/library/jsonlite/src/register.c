#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>

/* .Call calls */
extern SEXP C_collapse_array(SEXP);
extern SEXP C_collapse_array_pretty_inner(SEXP);
extern SEXP C_collapse_array_pretty_outer(SEXP, SEXP);
extern SEXP C_collapse_object(SEXP, SEXP);
extern SEXP C_collapse_object_pretty(SEXP, SEXP, SEXP);
extern SEXP C_escape_chars(SEXP);
extern SEXP C_is_datelist(SEXP);
extern SEXP C_is_recordlist(SEXP);
extern SEXP C_is_scalarlist(SEXP);
extern SEXP C_null_to_na(SEXP);
extern SEXP C_row_collapse_array(SEXP, SEXP);
extern SEXP C_row_collapse_object(SEXP, SEXP, SEXP);
extern SEXP C_transpose_list(SEXP, SEXP);
extern SEXP R_base64_decode(SEXP);
extern SEXP R_base64_encode(SEXP);
extern SEXP R_integer64_to_char(SEXP, SEXP);
extern SEXP R_num_to_char(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP R_parse(SEXP, SEXP);
extern SEXP R_parse_connection(SEXP, SEXP);
extern SEXP R_reformat(SEXP, SEXP, SEXP);
extern SEXP R_validate(SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"C_collapse_array",              (DL_FUNC) &C_collapse_array,              1},
  {"C_collapse_array_pretty_inner", (DL_FUNC) &C_collapse_array_pretty_inner, 1},
  {"C_collapse_array_pretty_outer", (DL_FUNC) &C_collapse_array_pretty_outer, 2},
  {"C_collapse_object",             (DL_FUNC) &C_collapse_object,             2},
  {"C_collapse_object_pretty",      (DL_FUNC) &C_collapse_object_pretty,      3},
  {"C_escape_chars",                (DL_FUNC) &C_escape_chars,                1},
  {"C_is_datelist",                 (DL_FUNC) &C_is_datelist,                 1},
  {"C_is_recordlist",               (DL_FUNC) &C_is_recordlist,               1},
  {"C_is_scalarlist",               (DL_FUNC) &C_is_scalarlist,               1},
  {"C_null_to_na",                  (DL_FUNC) &C_null_to_na,                  1},
  {"C_row_collapse_array",          (DL_FUNC) &C_row_collapse_array,          2},
  {"C_row_collapse_object",         (DL_FUNC) &C_row_collapse_object,         3},
  {"C_transpose_list",              (DL_FUNC) &C_transpose_list,              2},
  {"R_base64_decode",               (DL_FUNC) &R_base64_decode,               1},
  {"R_base64_encode",               (DL_FUNC) &R_base64_encode,               1},
  {"R_integer64_to_char",           (DL_FUNC) &R_integer64_to_char,           2},
  {"R_num_to_char",                 (DL_FUNC) &R_num_to_char,                 5},
  {"R_parse",                       (DL_FUNC) &R_parse,                       2},
  {"R_parse_connection",            (DL_FUNC) &R_parse_connection,            2},
  {"R_reformat",                    (DL_FUNC) &R_reformat,                    3},
  {"R_validate",                    (DL_FUNC) &R_validate,                    1},
  {NULL, NULL, 0}
};

attribute_visible void R_init_jsonlite(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
