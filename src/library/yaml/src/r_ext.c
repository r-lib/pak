#include "r_ext.h"

SEXP Ryaml_KeysSymbol = NULL;
SEXP Ryaml_TagSymbol = NULL;
SEXP Ryaml_QuotedSymbol = NULL;
SEXP Ryaml_IdenticalFunc = NULL;
SEXP Ryaml_FormatFunc = NULL;
SEXP Ryaml_PasteFunc = NULL;
SEXP Ryaml_DeparseFunc = NULL;
SEXP Ryaml_ClassFunc = NULL;
SEXP Ryaml_CollapseSymbol = NULL;
SEXP Ryaml_Sentinel = NULL;
SEXP Ryaml_SequenceStart = NULL;
SEXP Ryaml_MappingStart = NULL;
SEXP Ryaml_MappingEnd = NULL;
char Ryaml_error_msg[ERROR_MSG_SIZE];

void Ryaml_set_error_msg(const char *format, ...)
{
  va_list args;
  int result;

  va_start(args, format);
  result = vsnprintf(Ryaml_error_msg, ERROR_MSG_SIZE, format, args);
  va_end(args);

  if (result >= ERROR_MSG_SIZE) {
    warning("an error occurred, but the message was too long to format properly");

    /* ensure the string is null terminated */
    Ryaml_error_msg[ERROR_MSG_SIZE-1] = 0;
  }
}

/* Returns true if obj is a named list */
int Ryaml_is_named_list(SEXP s_obj)
{
  SEXP s_names = NULL;
  if (TYPEOF(s_obj) != VECSXP)
    return 0;

  s_names = GET_NAMES(s_obj);
  return (TYPEOF(s_names) == STRSXP && LENGTH(s_names) == LENGTH(s_obj));
}

/* Call R's paste() function with collapse */
SEXP Ryaml_collapse(
  SEXP s_obj,
  char *collapse)
{
  SEXP s_call = NULL, s_retval = NULL;

  PROTECT(s_call = lang3(Ryaml_PasteFunc, s_obj, ScalarString(mkCharCE(collapse, CE_UTF8))));
  SET_TAG(CDDR(s_call), Ryaml_CollapseSymbol);
  s_retval = eval(s_call, R_GlobalEnv);
  UNPROTECT(1);

  return s_retval;
}

/* Return a string representation of the object for error messages */
SEXP Ryaml_inspect(SEXP s_obj)
{
  SEXP s_call = NULL, s_str = NULL, s_result = NULL;

  /* Using format/paste here is not really what I want, but without
   * jumping through all kinds of hoops so that I can get the output
   * of print(), this is the most effort I want to put into this. */

  PROTECT(s_call = lang2(Ryaml_FormatFunc, s_obj));
  s_str = eval(s_call, R_GlobalEnv);
  UNPROTECT(1);

  PROTECT(s_str);
  s_result = Ryaml_collapse(s_str, " ");
  UNPROTECT(1);

  return s_result;
}

SEXP Ryaml_get_classes(SEXP s_obj)
{
  SEXP s_call = NULL, s_result = NULL;

  PROTECT(s_call = lang2(Ryaml_ClassFunc, s_obj));
  s_result = eval(s_call, R_GlobalEnv);
  UNPROTECT(1);

  return s_result;
}

/* Return 1 if obj is of the specified class */
int Ryaml_has_class(SEXP s_obj, char *name)
{
  SEXP s_classes = NULL;
  int i = 0, len = 0, result = 0;

  PROTECT(s_obj);
  PROTECT(s_classes = Ryaml_get_classes(s_obj));
  if (TYPEOF(s_classes) == STRSXP) {
    len = length(s_classes);
    for (i = 0; i < len; i++) {
      if (strcmp(CHAR(STRING_ELT(s_classes, i)), name) == 0) {
        result = 1;
        break;
      }
    }
  }
  UNPROTECT(2);
  return result;
}

SEXP Ryaml_sanitize_handlers(SEXP s_handlers)
{
  SEXP s_handlers_2 = NULL, s_handler = NULL, s_names = NULL, s_names_2 = NULL,
       s_name = NULL;
  const char *name = NULL, *name_2 = NULL;
  cetype_t encoding = CE_NATIVE;
  int i = 0;

  if (s_handlers == R_NilValue) {
    return R_NilValue;
  }
  else if (!Ryaml_is_named_list(s_handlers)) {
    error("handlers must be either NULL or a named list of functions");
    return R_NilValue;
  }
  else {
    PROTECT(s_names = GET_NAMES(s_handlers));

    PROTECT(s_handlers_2 = allocVector(VECSXP, length(s_handlers)));
    PROTECT(s_names_2 = allocVector(STRSXP, length(s_names)));

    for (i = 0; i < length(s_handlers); i++) {
      /* Possibly convert name to UTF-8 */
      PROTECT(s_name = STRING_ELT(s_names, i));
      encoding = getCharCE(s_name);
      if (encoding == CE_UTF8) {
        SET_STRING_ELT(s_names_2, i, s_name);
      }
      else {
        name = CHAR(s_name);
        name_2 = reEnc(name, encoding, CE_UTF8, 1);
        UNPROTECT(1); /* s_name */

        PROTECT(s_name = mkCharCE(name_2, CE_UTF8));
        SET_STRING_ELT(s_names_2, i, s_name);
      }
      name = CHAR(s_name);

      /* Validate handler */
      s_handler = VECTOR_ELT(s_handlers, i);

      if (TYPEOF(s_handler) != CLOSXP &&
          TYPEOF(s_handler) != BUILTINSXP)
      {
        warning("Your handler for type '%s' is not a function; using default", name);
        s_handler = R_NilValue;
      }
      else if (strcmp(name, "merge") == 0 || strcmp(name, "default") == 0)
      {
        /* custom handlers for merge and default are illegal */
        warning("Custom handling for type '%s' is not allowed; handler ignored", name);
        s_handler = R_NilValue;
      }

      SET_VECTOR_ELT(s_handlers_2, i, s_handler);
      UNPROTECT(1); /* s_name */
    }

    SET_NAMES(s_handlers_2, s_names_2);
    s_handlers = s_handlers_2;

    UNPROTECT(3); /* s_names, s_names_2, s_handlers_2 */
  }

  return s_handlers_2;
}

SEXP Ryaml_find_handler(SEXP s_handlers, const char *name)
{
  SEXP s_names = NULL, s_name = NULL, s_retval = R_NilValue;
  const char *handler_name = NULL;
  int i = 0, found = 0;

  /* Look for a custom R handler */
  if (s_handlers != R_NilValue) {
    PROTECT(s_names = GET_NAMES(s_handlers));
    for (i = 0; i < length(s_names); i++) {
      PROTECT(s_name = STRING_ELT(s_names, i));
      if (s_name != NA_STRING) {
        handler_name = CHAR(s_name);
        if (strcmp(handler_name, name) == 0) {
          /* Found custom handler */
          s_retval = VECTOR_ELT(s_handlers, i);
          found = 1;
        }
      }
      UNPROTECT(1); /* s_name */

      if (found) break;
    }
    UNPROTECT(1); /* s_names */
  }

  return s_retval;
}

int Ryaml_run_handler(SEXP s_handler, SEXP s_arg, SEXP *s_result)
{
  SEXP s_cmd = NULL;
  int err = 0;

  PROTECT(s_cmd = lang2(s_handler, s_arg));
  *s_result = R_tryEval(s_cmd, R_GlobalEnv, &err);
  UNPROTECT(1);

  return err;
}

R_CallMethodDef callMethods[] = {
  {"unserialize_from_yaml", (DL_FUNC)&Ryaml_unserialize_from_yaml, 8},
  {"serialize_to_yaml",     (DL_FUNC)&Ryaml_serialize_to_yaml,     9},
  {NULL, NULL, 0}
};

void R_init_yaml(DllInfo *dll) {
  Ryaml_KeysSymbol = install("keys");
  Ryaml_TagSymbol = install("tag");
  Ryaml_QuotedSymbol = install("quoted");
  Ryaml_CollapseSymbol = install("collapse");
  Ryaml_IdenticalFunc = findFun(install("identical"), R_GlobalEnv);
  Ryaml_FormatFunc = findFun(install("format"), R_GlobalEnv);
  Ryaml_PasteFunc = findFun(install("paste"), R_GlobalEnv);
  Ryaml_DeparseFunc = findFun(install("deparse"), R_GlobalEnv);
  Ryaml_ClassFunc = findFun(install("class"), R_GlobalEnv);
  Ryaml_Sentinel = install("sentinel");
  Ryaml_SequenceStart = install("sequence.start");
  Ryaml_MappingStart = install("mapping.start");
  Ryaml_MappingEnd = install("mapping.end");
  R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}

/*      .;   ;.
 *     / |   | \
 *    |  |   |  |
 *   .-----------.
 *  '             '
 *  |             |
 *  '============='  viking was here <3
 */
