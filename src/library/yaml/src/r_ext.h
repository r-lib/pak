#ifndef _R_EXT_H
#define _R_EXT_H

#ifdef __FAST_MATH__
#error The R yaml package will not work correctly with the -ffast-math option of GCC.
#endif

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <limits.h>
#include "R.h"
#include "Rdefines.h"
#include "Rversion.h"
#include "R_ext/Rdynload.h"
#include "R_ext/Parse.h"
#include "R_ext/Print.h"
#include "R_ext/PrtUtil.h"
#include "yaml.h"

#define REAL_BUF_SIZE 256
#define ERROR_MSG_SIZE 512

/* From implicit.c */
char *Ryaml_find_implicit_tag(const char *value, size_t size);

/* Common functions */
int Ryaml_is_named_list(SEXP s_obj);
SEXP Ryaml_collapse(SEXP s_obj, char *collapse);
SEXP Ryaml_inspect(SEXP s_obj);
SEXP Ryaml_get_classes(SEXP s_obj);
int Ryaml_has_class(SEXP s_obj, char *name);
void Ryaml_set_error_msg(const char *format, ...);
SEXP Ryaml_sanitize_handlers(SEXP s_handlers);
SEXP Ryaml_find_handler(SEXP s_handlers, const char *name);
int Ryaml_run_handler(SEXP s_handler, SEXP s_arg, SEXP *s_result);

/* Exported functions */
SEXP Ryaml_serialize_to_yaml(SEXP s_obj, SEXP s_line_sep, SEXP s_indent, SEXP s_omap,
    SEXP s_column_major, SEXP s_unicode, SEXP s_precision,
    SEXP s_indent_mapping_sequence, SEXP s_handlers);

SEXP Ryaml_unserialize_from_yaml(SEXP s_string, SEXP s_as_named_list,
    SEXP s_handlers, SEXP s_error_label, SEXP s_eval_expr,
    SEXP s_eval_warning, SEXP s_merge_precedence, SEXP s_merge_warning);

#endif
