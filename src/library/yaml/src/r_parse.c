#include "r_ext.h"

extern SEXP Ryaml_KeysSymbol;
extern SEXP Ryaml_IdenticalFunc;
extern SEXP Ryaml_Sentinel;
extern SEXP Ryaml_SequenceStart;
extern SEXP Ryaml_MappingStart;
extern SEXP Ryaml_MappingEnd;
extern char Ryaml_error_msg[ERROR_MSG_SIZE];

/* Compare two R objects (with the R identical function).
 * Returns 0 or 1 */
static int Ryaml_cmp(SEXP s_first, SEXP s_second)
{
  int i = 0, retval = 0, *arr = NULL;
  SEXP s_call = NULL, s_result = NULL, s_bool = NULL;

  PROTECT(s_bool = allocVector(LGLSXP, 1));
  LOGICAL(s_bool)[0] = 1;
  PROTECT(s_call = LCONS(Ryaml_IdenticalFunc, list4(s_first, s_second, s_bool, s_bool)));
  PROTECT(s_result = eval(s_call, R_GlobalEnv));

  arr = LOGICAL(s_result);
  for(i = 0; i < length(s_result); i++) {
    if (!arr[i]) {
      retval = 1;
      break;
    }
  }
  UNPROTECT(3);
  return retval;
}

/* Returns the index of the first instance of needle in haystack */
static int Ryaml_index(
  SEXP s_haystack,
  SEXP s_needle,
  int character,
  int upper_bound)
{
  int i = 0;

  if (character) {
    for (i = 0; i < upper_bound; i++) {
      if (strcmp(CHAR(s_needle), CHAR(STRING_ELT(s_haystack, i))) == 0) {
        return i;
      }
    }
  }
  else {
    for (i = 0; i < upper_bound; i++) {
      if (Ryaml_cmp(s_needle, VECTOR_ELT(s_haystack, i)) == 0) {
        return i;
      }
    }
  }

  return -1;
}

/* Returns true if obj is a list with a keys attribute */
static int Ryaml_is_pseudo_hash(SEXP s_obj)
{
  SEXP s_keys = NULL;
  if (TYPEOF(s_obj) != VECSXP)
    return 0;

  s_keys = getAttrib(s_obj, Ryaml_KeysSymbol);
  return (s_keys != R_NilValue && TYPEOF(s_keys) == VECSXP);
}

/* Set a character attribute on an R object */
static void Ryaml_set_str_attrib(SEXP s_obj, SEXP s_sym, char *str)
{
  SEXP s_val = NULL;
  PROTECT(s_val = ScalarString(mkCharCE(str, CE_UTF8)));
  setAttrib(s_obj, s_sym, s_val);
  UNPROTECT(1);
}

/* Set the R object's class attribute */
static void Ryaml_set_class(SEXP s_obj, char *name)
{
  Ryaml_set_str_attrib(s_obj, R_ClassSymbol, name);
}

/* Get the type part of the tag, throw away any !'s */
static const char *process_tag(const char *tag)
{
  const char *retval = tag;

  if (strncmp(retval, "tag:yaml.org,2002:", 18) == 0) {
    retval = retval + 18;
  }
  else {
    while (*retval == '!') {
      retval++;
    }
  }
  return retval;
}

static int handle_alias(
  yaml_event_t *event,
  SEXP *s_stack_tail,
  SEXP s_aliases_head)
{
  SEXP s_curr = NULL, s_obj = NULL;
  int handled = 0;
  const char *name = NULL, *anchor = NULL;

  /* Try to find object with the supplied anchor */
  anchor = (const char *)event->data.alias.anchor;
  s_curr = CDR(s_aliases_head);
  while (s_curr != R_NilValue) {
    s_obj = CAR(s_curr);
    name = CHAR(TAG(s_curr));
    if (strcmp(name, anchor) == 0) {
      /* Found object, push onto stack */
      SETCDR(*s_stack_tail, list1(s_obj));
      *s_stack_tail = CDR(*s_stack_tail);

      MARK_NOT_MUTABLE(s_obj);
      handled = 1;
      break;
    }
    s_curr = CDR(s_curr);
  }

  if (!handled) {
    warning("Unknown anchor: %s", (char *)event->data.alias.anchor);
    PROTECT(s_obj = ScalarString(mkCharCE("_yaml.bad-anchor_", CE_UTF8)));
    Ryaml_set_class(s_obj, "_yaml.bad-anchor_");
    UNPROTECT(1);

    SETCDR(*s_stack_tail, list1(s_obj));
    *s_stack_tail = CDR(*s_stack_tail);
  }

  return 0;
}

static int handle_scalar(
  yaml_event_t *event,
  SEXP *s_stack_tail,
  SEXP s_handlers,
  int eval_expr,
  int eval_warning)
{
  SEXP s_obj = NULL, s_handler = NULL, s_new_obj = NULL, s_expr = NULL;
  const char *value = NULL, *tag = NULL, *nptr = NULL;
  char *endptr = NULL;
  size_t len = 0;
  int handled = 0, coercion_err = 0, base = 0, n = 0;
  long int long_n = 0;
  double f = 0.0f;
  ParseStatus parse_status;

  tag = (const char *)event->data.scalar.tag;
  value = (const char *)event->data.scalar.value;
  len = event->data.scalar.length;
  if (tag == NULL || strcmp(tag, "!") == 0) {
    /* There's no tag! */

    /* If this is a quoted string, leave it as a string */
    switch (event->data.scalar.style) {
      case YAML_SINGLE_QUOTED_SCALAR_STYLE:
      case YAML_DOUBLE_QUOTED_SCALAR_STYLE:
        tag = "str";
        break;
      default:
        /* Try to tag it */
        tag = Ryaml_find_implicit_tag(value, len);
    }
  }
  else {
    tag = process_tag(tag);
  }

#if DEBUG
  Rprintf("Value: (%s), Tag: (%s)\n", value, tag);
#endif

  /* 'Vanilla' object */
  PROTECT(s_obj = ScalarString(mkCharCE(value, CE_UTF8)));

  /* Look for a custom R handler */
  PROTECT(s_handler = Ryaml_find_handler(s_handlers, (const char *)tag));
  if (s_handler != R_NilValue) {
    if (Ryaml_run_handler(s_handler, s_obj, &s_new_obj) != 0) {
      warning("an error occurred when handling type '%s'; using default handler", tag);
    }
    else {
      handled = 1;
    }
  }
  UNPROTECT(1); /* s_handler */

  if (!handled) {
    /* Default handlers */

    if (strcmp(tag, "str") == 0) {
      /* already a string */
    }
    else if (strcmp(tag, "seq") == 0) {
      coercion_err = 1;
    }
    else if (strcmp(tag, "int#na") == 0) {
      s_new_obj = ScalarInteger(NA_INTEGER);
    }
    else if (strcmp(tag, "int") == 0 || strncmp(tag, "int#", 4) == 0) {
      base = -1;
      if (strcmp(tag, "int") == 0) {
        base = 10;
      }
      else if (strcmp(tag, "int#hex") == 0) {
        base = 16;
      }
      else if (strcmp(tag, "int#oct") == 0) {
        base = 8;
      }

      if (base >= 0) {
        errno = 0;
        nptr = CHAR(STRING_ELT(s_obj, 0));
        long_n = strtol(nptr, &endptr, base);
        if (*endptr != 0) {
          /* strtol is perfectly happy converting partial strings to
           * integers, but R isn't. If you call as.integer() on a
           * string that isn't completely an integer, you get back
           * an NA. So I'm reproducing that behavior here. */

          warning("NAs introduced by coercion: %s is not an integer", nptr);
          n = NA_INTEGER;
        } else if (errno == ERANGE) {
          warning("NAs introduced by coercion: %s is out of integer range", nptr);
          n = NA_INTEGER;
        } else if (long_n < INT_MIN || long_n > INT_MAX || (int)long_n == NA_INTEGER) {
          warning("NAs introduced by coercion: %s is out of integer range", nptr);
          n = NA_INTEGER;
        } else {
          n = (int)long_n;
        }

        s_new_obj = ScalarInteger(n);
      }
      else {
        /* unknown integer base; no-op */
      }
    }
    else if (strcmp(tag, "float") == 0 || strcmp(tag, "float#fix") == 0 || strcmp(tag, "float#exp") == 0) {
      errno = 0;
      nptr = CHAR(STRING_ELT(s_obj, 0));
      f = strtod(nptr, &endptr);
      if (*endptr != 0) {
        /* No valid floats found (see note above about integers) */
        warning("NAs introduced by coercion: %s is not a real", nptr);
        f = NA_REAL;
      } else if (errno == ERANGE || f == NA_REAL) {
        warning("NAs introduced by coercion: %s is out of real range", nptr);
        f = NA_REAL;
      }

      s_new_obj = ScalarReal(f);
    }
    else if (strcmp(tag, "bool") == 0) {
      /* This would happen if someone explicitly specified a tag of 'bool' */
      tag = Ryaml_find_implicit_tag(value, len);
      if (strcmp(tag, "bool#yes") == 0) {
        s_new_obj = ScalarLogical(TRUE);
      }
      else if (strcmp(tag, "bool#no") == 0) {
        s_new_obj = ScalarLogical(FALSE);
      }
      else if (strcmp(tag, "bool#na") == 0) {
        s_new_obj = ScalarLogical(NA_LOGICAL);
      }
      else {
        warning("NAs introduced by coercion: %s is not a recognized boolean value", value);
        s_new_obj = ScalarLogical(NA_LOGICAL);
      }
    }
    else if (strcmp(tag, "bool#yes") == 0) {
      s_new_obj = ScalarLogical(TRUE);
    }
    else if (strcmp(tag, "bool#no") == 0) {
      s_new_obj = ScalarLogical(FALSE);
    }
    else if (strcmp(tag, "bool#na") == 0) {
      s_new_obj = ScalarLogical(NA_LOGICAL);
    }
    else if (strcmp(tag, "omap") == 0) {
      coercion_err = 1;
    }
    else if (strcmp(tag, "merge") == 0) {
      /* see http://yaml.org/type/merge.html */
      PROTECT(s_new_obj = ScalarString(mkCharCE("_yaml.merge_", CE_UTF8)));
      Ryaml_set_class(s_new_obj, "_yaml.merge_");
      UNPROTECT(1);
    }
    else if (strcmp(tag, "float#na") == 0) {
      s_new_obj = ScalarReal(NA_REAL);
    }
    else if (strcmp(tag, "float#nan") == 0) {
      s_new_obj = ScalarReal(R_NaN);
    }
    else if (strcmp(tag, "float#inf") == 0) {
      s_new_obj = ScalarReal(R_PosInf);
    }
    else if (strcmp(tag, "float#neginf") == 0) {
      s_new_obj = ScalarReal(R_NegInf);
    }
    else if (strcmp(tag, "str#na") == 0) {
      s_new_obj = ScalarString(NA_STRING);
    }
    else if (strcmp(tag, "null") == 0) {
      s_new_obj = R_NilValue;
    }
    else if (strcmp(tag, "expr") == 0) {
      if (eval_expr) {
        PROTECT(s_obj);
        s_expr = R_ParseVector(s_obj, 1, &parse_status, R_NilValue);
        UNPROTECT(1);

        if (parse_status != PARSE_OK) {
          coercion_err = 1;
          Ryaml_set_error_msg("Could not parse expression: %s", CHAR(STRING_ELT(s_obj, 0)));
        }
        else {
          /* NOTE: R_tryEval will not return if R_Interactive is FALSE. */
          PROTECT(s_expr);
          PROTECT(s_new_obj = R_tryEval(VECTOR_ELT(s_expr, 0), R_GlobalEnv, &coercion_err));

          if (coercion_err) {
            Ryaml_set_error_msg("Could not evaluate expression: %s", CHAR(STRING_ELT(s_obj, 0)));
          } 
          UNPROTECT(2); /* s_expr, s_new_obj */
        }
      }
      else if (eval_warning) {
        warning("Evaluating R expressions (!expr) requires explicit `eval.expr=TRUE` option (see yaml.load help)");
      }
    }
  }
  UNPROTECT(1); /* s_obj */

  if (coercion_err == 1) {
    if (Ryaml_error_msg[0] == 0) {
      Ryaml_set_error_msg("Invalid tag for scalar: %s", tag);
    }
    return 1;
  }

  SETCDR(*s_stack_tail, list1(s_new_obj == NULL ? s_obj : s_new_obj));
  *s_stack_tail = CDR(*s_stack_tail);

  return 0;
}

static void handle_structure_start(
  yaml_event_t *event,
  SEXP *s_stack_tail,
  int is_map)
{
  SEXP s_sym = NULL, s_tag_obj = NULL, s_anchor_obj = NULL, s_tag = NULL;
  yaml_char_t *tag = NULL, *anchor = NULL;

  if (is_map) {
    s_sym = Ryaml_MappingStart;
    tag = event->data.mapping_start.tag;
    anchor = event->data.mapping_start.anchor;
  } else {
    s_sym = Ryaml_SequenceStart;
    tag = event->data.sequence_start.tag;
    anchor = event->data.sequence_start.anchor;
  }

  SETCDR(*s_stack_tail, list1(s_sym));
  *s_stack_tail = CDR(*s_stack_tail);

  /* Create pairlist tag */
  if (tag == NULL) {
    s_tag_obj = R_NilValue;
  }
  else {
    s_tag_obj = mkCharCE((const char *)tag, CE_UTF8);
  }
  if (anchor == NULL) {
    s_anchor_obj = R_NilValue;
  }
  else {
    PROTECT(s_tag_obj);
    s_anchor_obj = mkCharCE((const char *)anchor, CE_UTF8);
    UNPROTECT(1);
  }
  s_tag = list2(s_tag_obj, s_anchor_obj);
  SET_TAG(*s_stack_tail, s_tag);
}

static int handle_sequence(
  yaml_event_t *event,
  SEXP s_stack_head,
  SEXP *s_stack_tail,
  SEXP s_handlers,
  int coerce_keys)
{
  SEXP s_curr = NULL, s_obj = NULL, s_sequence_start = NULL, s_list = NULL,
       s_handler = NULL, s_new_obj = NULL, s_keys = NULL, s_key = NULL,
       s_tag = NULL, s_inspect = NULL;
  int count = 0, i = 0, j = 0, type = 0, child_type = 0, handled = 0,
      coercion_err = 0, len = 0, total_len = 0, dup_key = 0, idx = 0,
      obj_len = 0;
  const char *tag = NULL, *inspect = NULL;

  /* Find start of sequence and count elements */
  s_curr = CDR(s_stack_head);
  count = 0;
  while (s_curr != R_NilValue) {
    if (CAR(s_curr) == Ryaml_SequenceStart) {
      s_sequence_start = s_curr;
      count = 0;
    } else if (s_sequence_start != NULL) {
      count++;
    }
    s_curr = CDR(s_curr);
  }
  if (s_sequence_start == NULL) {
    Ryaml_set_error_msg("Internal error: couldn't find start of sequence!");
    return 1;
  }

  s_tag = CAR(TAG(s_sequence_start));
  tag = s_tag == R_NilValue ? NULL : CHAR(s_tag);

  /* Initialize list */
  PROTECT(s_list = allocVector(VECSXP, count));

  /* Populate the list, popping items off the stack as we go */
  type = -2;
  s_curr = CDR(s_sequence_start);
  for (i = 0; i < count; i++) {
    s_obj = CAR(s_curr);
    s_curr = CDR(s_curr);
    SET_VECTOR_ELT(s_list, i, s_obj);

    /* Treat primitive vectors with more than one element as a list for
     * coercion purposes. */
    child_type = TYPEOF(s_obj);
    switch (child_type) {
      case LGLSXP:
      case INTSXP:
      case REALSXP:
      case STRSXP:
        if (length(s_obj) != 1) {
          child_type = VECSXP;
        }
        break;
    }

    if (type == -2) {
      type = child_type;
    }
    else if (type != -1 && child_type != type) {
      type = -1;
    }
  }

  /* Tags! */
  if (tag == NULL) {
    tag = "seq";
  }
  else {
    tag = process_tag(tag);
  }

  /* Look for a custom R handler */
  s_handler = Ryaml_find_handler(s_handlers, (const char *)tag);
  if (s_handler != R_NilValue) {
    if (Ryaml_run_handler(s_handler, s_list, &s_new_obj) != 0) {
      warning("an error occurred when handling type '%s'; using default handler", tag);
    }
    else {
      handled = 1;
    }
  }

  if (!handled) {
    /* default handlers, ordered by most-used */

    if (strcmp(tag, "seq") == 0) {
      /* Let's try to coerce this list! */
      switch (type) {
        case LGLSXP:
        case INTSXP:
        case REALSXP:
        case STRSXP:
          s_new_obj = coerceVector(s_list, type);
          break;
      }
    }
    else if (strcmp(tag, "str") == 0) {
      coercion_err = 1;
    }
    else if (strcmp(tag, "int#na") == 0) {
      coercion_err = 1;
    }
    else if (strcmp(tag, "int") == 0 || strncmp(tag, "int#", 4) == 0) {
      coercion_err = 1;
    }
    else if (strcmp(tag, "float") == 0 || strcmp(tag, "float#fix") == 0 || strcmp(tag, "float#exp") == 0) {
      coercion_err = 1;
    }
    else if (strcmp(tag, "bool#yes") == 0) {
      coercion_err = 1;
    }
    else if (strcmp(tag, "bool#no") == 0) {
      coercion_err = 1;
    }
    else if (strcmp(tag, "bool#na") == 0) {
      coercion_err = 1;
    }
    else if (strcmp(tag, "omap") == 0) {
      /* NOTE: This is here mostly because of backwards compatibility
       * with R yaml 1.x package. All maps are ordered in 2.x, so there's
       * no real need to use omap */

      len = length(s_list);
      total_len = 0;
      for (i = 0; i < len; i++) {
        s_obj = VECTOR_ELT(s_list, i);
        if ((coerce_keys && !Ryaml_is_named_list(s_obj)) || (!coerce_keys && !Ryaml_is_pseudo_hash(s_obj))) {
          Ryaml_set_error_msg("omap must be a sequence of maps");
          coercion_err = 1;
          break;
        }
        total_len += length(s_obj);
      }

      /* Construct the list! */
      if (!coercion_err) {
        PROTECT(s_new_obj = allocVector(VECSXP, total_len));
        if (coerce_keys) {
          s_keys = allocVector(STRSXP, total_len);
          SET_NAMES(s_new_obj, s_keys);
        }
        else {
          s_keys = allocVector(VECSXP, total_len);
          setAttrib(s_new_obj, Ryaml_KeysSymbol, s_keys);
        }

        for (i = 0, idx = 0; i < len && dup_key == 0; i++) {
          s_obj = VECTOR_ELT(s_list, i);
          obj_len = length(s_obj);
          for (j = 0; j < obj_len && dup_key == 0; j++) {
            SET_VECTOR_ELT(s_new_obj, idx, VECTOR_ELT(s_obj, j));

            if (coerce_keys) {
              PROTECT(s_key = STRING_ELT(GET_NAMES(s_obj), j));
              SET_STRING_ELT(s_keys, idx, s_key);

              if (Ryaml_index(s_keys, s_key, 1, idx) >= 0) {
                dup_key = 1;
                Ryaml_set_error_msg("Duplicate omap key: '%s'", CHAR(s_key));
              }
              UNPROTECT(1); /* s_key */
            }
            else {
              s_key = VECTOR_ELT(getAttrib(s_obj, Ryaml_KeysSymbol), j);
              SET_VECTOR_ELT(s_keys, idx, s_key);

              if (Ryaml_index(s_keys, s_key, 0, idx) >= 0) {
                dup_key = 1;

                PROTECT(s_inspect = Ryaml_inspect(s_key));
                inspect = CHAR(STRING_ELT(s_inspect, 0));
                Ryaml_set_error_msg("Duplicate omap key: %s", inspect);
                UNPROTECT(1);
              }
            }
            idx++;
          }
        }
        UNPROTECT(1); /* s_new_obj */

        if (dup_key == 1) {
          coercion_err = 1;
        }
      }
    }
    else if (strcmp(tag, "merge") == 0) {
      coercion_err = 1;
    }
    else if (strcmp(tag, "float#na") == 0) {
      coercion_err = 1;
    }
    else if (strcmp(tag, "float#nan") == 0) {
      coercion_err = 1;
    }
    else if (strcmp(tag, "float#inf") == 0) {
      coercion_err = 1;
    }
    else if (strcmp(tag, "float#neginf") == 0) {
      coercion_err = 1;
    }
    else if (strcmp(tag, "str#na") == 0) {
      coercion_err = 1;
    }
    else if (strcmp(tag, "null") == 0) {
      s_new_obj = R_NilValue;
    }
    else if (strcmp(tag, "expr") == 0) {
      coercion_err = 1;
    }
  }
  UNPROTECT(1); /* s_list */

  if (coercion_err == 1) {
    if (Ryaml_error_msg[0] == 0) {
      Ryaml_set_error_msg("Invalid tag: %s for sequence", tag);
    }
    return 1;
  }

  SETCAR(s_sequence_start, s_new_obj == NULL ? s_list : s_new_obj);
  SETCDR(s_sequence_start, R_NilValue);
  *s_stack_tail = s_sequence_start;

  return 0;
}

static SEXP find_map_entry(
  SEXP s_map_head,
  SEXP s_key,
  int character)
{
  SEXP s_curr = NULL;

  s_curr = CDR(s_map_head);
  if (character) {
    while (s_curr != R_NilValue) {
      if (strcmp(CHAR(s_key), CHAR(CAR(TAG(s_curr)))) == 0) {
        return s_curr;
      }
      s_curr = CDR(s_curr);
    }
  }
  else {
    while (CAR(s_curr) != R_NilValue) {
      if (Ryaml_cmp(s_key, CAR(TAG(s_curr))) == 0) {
        return s_curr;
      }
      s_curr = CDR(s_curr);
    }
  }

  return NULL;
}

static int expand_merge(
  SEXP s_merge_list,
  SEXP s_map_head,
  SEXP *s_map_tail,
  int coerce_keys,
  int merge_warning)
{
  SEXP s_merge_keys = NULL, s_value = NULL, s_key = NULL, s_result = NULL,
       s_inspect = NULL;
  int i = 0, count = 0;
  const char *inspect = NULL;

  s_merge_keys = coerce_keys ? GET_NAMES(s_merge_list) : getAttrib(s_merge_list, Ryaml_KeysSymbol);
  for (i = 0; i < length(s_merge_list); i++) {
    s_value = VECTOR_ELT(s_merge_list, i);
    if (coerce_keys) {
      s_key = STRING_ELT(s_merge_keys, i);
    }
    else {
      s_key = VECTOR_ELT(s_merge_keys, i);
    }

    PROTECT(s_key);
    s_result = find_map_entry(s_map_head, s_key, coerce_keys);
    if (s_result != NULL) {
      /* A matching key is already in the map, so ignore this one. */
      if (merge_warning) {
        if (coerce_keys) {
          inspect = CHAR(s_key);
        }
        else {
          PROTECT(s_inspect = Ryaml_inspect(s_key));
          inspect = CHAR(STRING_ELT(s_inspect, 0));
        }
        warning("Duplicate map key ignored during merge: '%s'", inspect);

        if (!coerce_keys) {
          UNPROTECT(1); /* s_inspect */
        }
      }
    }
    else {
      SETCDR(*s_map_tail, list1(s_value));
      *s_map_tail = CDR(*s_map_tail);

      SET_TAG(*s_map_tail, list2(s_key, ScalarLogical(TRUE)));
      count++;
    }
    UNPROTECT(1); /* s_key */
  }

  return count;
}

static int is_mergeable(SEXP s_merge_list, int coerce_keys)
{
  return (coerce_keys && Ryaml_is_named_list(s_merge_list)) ||
         (!coerce_keys && Ryaml_is_pseudo_hash(s_merge_list));
}

/* Return -1 on error or number of entries added to map. */
static int handle_map_entry(
  SEXP s_key,
  SEXP s_value,
  SEXP s_map_head,
  SEXP *s_map_tail,
  int coerce_keys,
  int merge_warning)
{
  SEXP s_result = NULL, s_tag = NULL, s_inspect = NULL;
  const char *inspect = NULL;
  int len = 0, count = 0;

  if (coerce_keys) {
    /* (Possibly) convert this key to a character vector, and then save
     * the first element in the vector (CHARSXP element). Throw away
     * the containing vector, since it's not needed anymore. */
    PROTECT(s_key = AS_CHARACTER(s_key));
    len = length(s_key);

    if (len == 0) {
      warning("Empty character vector used as a list name");
      s_key = mkCharCE("", CE_UTF8);
    } else {
      if (len > 1) {
        warning("Character vector of length greater than 1 used as a list name");
      }
      s_key = STRING_ELT(s_key, 0);
    }
    UNPROTECT(1);
  }

  PROTECT(s_key);
  s_result = find_map_entry(s_map_head, s_key, coerce_keys);
  if (s_result != NULL) {
    /* A matching key is already in the map. If the existing key is from a
     * merge, it's okay to ignore it. If not, it's a duplicate key error. */
    s_tag = TAG(s_result);
    if (coerce_keys) {
      inspect = CHAR(s_key);
    }
    else {
      PROTECT(s_inspect = Ryaml_inspect(s_key));
      inspect = CHAR(STRING_ELT(s_inspect, 0));
    }

    if (LOGICAL(CADR(s_tag))[0] == FALSE) {
      Ryaml_set_error_msg("Duplicate map key: '%s'", inspect);
      count = -1;
    }
    else if (merge_warning) {
      warning("Duplicate map key ignored after merge: '%s'", inspect);
    }

    if (!coerce_keys) {
      UNPROTECT(1); /* s_inspect */
    }
  } else {
    SETCDR(*s_map_tail, list1(s_value));
    *s_map_tail = CDR(*s_map_tail);
    SET_TAG(*s_map_tail, list2(s_key, ScalarLogical(FALSE)));
    count = 1;
  }
  UNPROTECT(1); /* s_key */

  return count;
}

/* Return -1 on error or number of entries added to map. */
static int handle_merge(
  SEXP s_value,
  SEXP s_map_head,
  SEXP *s_map_tail,
  int coerce_keys,
  int merge_warning)
{
  SEXP s_obj = NULL, s_inspect = NULL;
  const char *inspect = NULL;
  int i = 0, count = 0, len = 0;

  if (is_mergeable(s_value, coerce_keys)) {
    /* i.e.
     *    - &bar { hey: dude }
     *    - foo:
     *        hello: friend
     *        <<: *bar
     */
    count = expand_merge(s_value, s_map_head, s_map_tail, coerce_keys, merge_warning);
  }
  else if (TYPEOF(s_value) == VECSXP) {
    /* i.e.
     *    - &bar { hey: dude }
     *    - &baz { hi: buddy }
     *    - foo:
     *        hello: friend
     *        <<: [*bar, *baz]
     */

    for (i = 0; i < length(s_value); i++) {
      s_obj = VECTOR_ELT(s_value, i);
      if (is_mergeable(s_obj, coerce_keys)) {
        len = expand_merge(s_obj, s_map_head, s_map_tail, coerce_keys, merge_warning);
        if (len >= 0) {
          count += len;
        }
        else {
          count = -1;
          break;
        }
      }
      else {
        /* Illegal merge */
        PROTECT(s_inspect = Ryaml_inspect(s_value));
        inspect = CHAR(STRING_ELT(s_inspect, 0));
        Ryaml_set_error_msg("Illegal merge: %s", inspect);
        UNPROTECT(1);

        count = -1;
        break;
      }
    }
  }
  else {
    /* Illegal merge */
    PROTECT(s_inspect = Ryaml_inspect(s_value));
    inspect = CHAR(STRING_ELT(s_inspect, 0));
    Ryaml_set_error_msg("Illegal merge: %s", inspect);
    UNPROTECT(1);

    count = -1;
  }

  return count;
}

static int handle_map(
  yaml_event_t *event,
  SEXP s_stack_head,
  SEXP *s_stack_tail,
  SEXP s_handlers,
  int coerce_keys,
  int merge_override,
  int merge_warning)
{
  SEXP s_list = NULL, s_keys = NULL, s_key = NULL, s_value = NULL,
       s_prev = NULL, s_curr = NULL, s_mapping_start = NULL,
       s_interim_map_head = NULL, s_interim_map_tail = NULL, s_new_obj = NULL,
       s_handler = NULL, s_tag = NULL;
  int count = 0, i = 0, map_err = 0, handled = 0, coercion_err = 0, len = 0;
  const char *tag = NULL, *original_tag = NULL;

  /* Find beginning of last map */
  s_curr = CDR(s_stack_head);
  while (s_curr != R_NilValue) {
    if (CAR(s_curr) == Ryaml_MappingStart) {
      s_mapping_start = s_curr;
    }
    s_curr = CDR(s_curr);
  }
  if (s_mapping_start == NULL) {
    Ryaml_set_error_msg("Internal error: couldn't find start of mapping!");
    return 1;
  }

  /* Set up interim map */
  PROTECT(s_interim_map_head = s_interim_map_tail = list1(Ryaml_MappingStart));

  if (merge_override) {
    /* If merge override is turned on, then normal map entries always take
     * precedence over any merged map entries. Therefore, go through and look
     * for any normal entries and place them in the interim map first. */

    s_prev = s_mapping_start;
    s_curr = CDR(s_mapping_start);
    while (!map_err && s_curr != R_NilValue) {
      s_key = CAR(s_curr);
      s_value = CADR(s_curr);

      if (!Ryaml_has_class(s_key, "_yaml.merge_")) {
        len = handle_map_entry(s_key, s_value, s_interim_map_head, &s_interim_map_tail, coerce_keys, merge_warning);
        if (len >= 0) {
          count += len;

          /* Remove key/value from stack to prevent double processing */
          SETCDR(s_prev, CDDR(s_curr));
          s_curr = CDDR(s_curr);
        }
        else {
          map_err = 1;
        }
      }
      else {
        /* Skip over merge key/value */
        s_prev = CDR(s_curr);
        s_curr = CDDR(s_curr);
      }
    }
  }

  /* Iterate keys and values */
  s_curr = CDR(s_mapping_start);
  while (!map_err && s_curr != R_NilValue) {
    s_key = CAR(s_curr);
    s_value = CADR(s_curr);
    s_curr = CDDR(s_curr);

    if (Ryaml_has_class(s_key, "_yaml.merge_")) {
      len = handle_merge(s_value, s_interim_map_head, &s_interim_map_tail, coerce_keys, merge_warning);
    }
    else {
      if (merge_override) {
        /* If merge override is turned on, merges should have already been processed. */
        Ryaml_set_error_msg("Merge override failed");
        map_err = 1;
        break;
      }

      len = handle_map_entry(s_key, s_value, s_interim_map_head, &s_interim_map_tail, coerce_keys, merge_warning);
    }

    if (len >= 0) {
      count += len;
    }
    else {
      map_err = 1;
    }
  }

  if (map_err) {
    UNPROTECT(1); /* s_map */
    return 1;
  }

  /* Initialize value list */
  PROTECT(s_list = allocVector(VECSXP, count));

  /* Initialize key list/vector */
  if (coerce_keys) {
    s_keys = NEW_STRING(count);
    SET_NAMES(s_list, s_keys);
  }
  else {
    s_keys = allocVector(VECSXP, count);
    setAttrib(s_list, Ryaml_KeysSymbol, s_keys);
  }

  /* Iterate map entries */
  s_curr = CDR(s_interim_map_head);
  for (i = 0; i < count; i++) {
    s_value = CAR(s_curr);
    s_key = CAR(TAG(s_curr));
    s_curr = CDR(s_curr);

    SET_VECTOR_ELT(s_list, i, s_value);

    /* map key */
    if (coerce_keys) {
      SET_STRING_ELT(s_keys, i, s_key);
    }
    else {
      SET_VECTOR_ELT(s_keys, i, s_key);
    }
  }
  UNPROTECT(2); /* s_interim_map_head, s_list */

  /* Tags! */
  s_tag = CAR(TAG(s_mapping_start));
  original_tag = tag = (s_tag == R_NilValue ? NULL : CHAR(s_tag));
  if (tag == NULL) {
    tag = "map";
  }
  else {
    tag = process_tag(tag);
  }

  /* Look for a custom R handler */
  PROTECT(s_list);
  s_handler = Ryaml_find_handler(s_handlers, (const char *) tag);
  if (s_handler != R_NilValue) {
    if (Ryaml_run_handler(s_handler, s_list, &s_new_obj) != 0) {
      warning("an error occurred when handling type '%s'; using default handler", tag);
    }
    else {
      handled = 1;
    }
  }
  UNPROTECT(1); /* s_list */

  if (!handled) {
    /* default handlers, ordered by most-used */

    if (strcmp(tag, "map") == 0) {
      /* already a map */
    }
    else if (strcmp(tag, "str") == 0) {
      coercion_err = 1;
    }
    else if (strcmp(tag, "seq") == 0) {
      coercion_err = 1;
    }
    else if (strcmp(tag, "int#na") == 0) {
      coercion_err = 1;
    }
    else if (strcmp(tag, "int") == 0 || strncmp(tag, "int#", 4) == 0) {
      coercion_err = 1;
    }
    else if (strcmp(tag, "float") == 0 || strcmp(tag, "float#fix") == 0 || strcmp(tag, "float#exp") == 0) {
      coercion_err = 1;
    }
    else if (strcmp(tag, "bool#yes") == 0) {
      coercion_err = 1;
    }
    else if (strcmp(tag, "bool#no") == 0) {
      coercion_err = 1;
    }
    else if (strcmp(tag, "bool#na") == 0) {
      coercion_err = 1;
    }
    else if (strcmp(tag, "omap") == 0) {
      coercion_err = 1;
    }
    else if (strcmp(tag, "merge") == 0) {
      coercion_err = 1;
    }
    else if (strcmp(tag, "float#na") == 0) {
      coercion_err = 1;
    }
    else if (strcmp(tag, "float#nan") == 0) {
      coercion_err = 1;
    }
    else if (strcmp(tag, "float#inf") == 0) {
      coercion_err = 1;
    }
    else if (strcmp(tag, "float#neginf") == 0) {
      coercion_err = 1;
    }
    else if (strcmp(tag, "str#na") == 0) {
      coercion_err = 1;
    }
    else if (strcmp(tag, "null") == 0) {
      s_new_obj = R_NilValue;
    }
    else if (strcmp(tag, "expr") == 0) {
      coercion_err = 1;
    }
  }

  if (coercion_err == 1) {
    if (Ryaml_error_msg[0] == 0) {
      Ryaml_set_error_msg("Invalid tag: %s for map", original_tag);
    }
    return 1;
  }

  SETCAR(s_mapping_start, s_new_obj == NULL ? s_list : s_new_obj);
  SETCDR(s_mapping_start, R_NilValue);
  *s_stack_tail = s_mapping_start;

  return 0;
}

static void possibly_record_alias(
  SEXP s_anchor,
  SEXP *s_aliases_tail,
  SEXP s_obj)
{
  if (s_anchor == NULL || TYPEOF(s_anchor) != CHARSXP) return;

  SETCDR(*s_aliases_tail, list1(s_obj));
  *s_aliases_tail = CDR(*s_aliases_tail);
  SET_TAG(*s_aliases_tail, s_anchor);
}

SEXP Ryaml_unserialize_from_yaml(
  SEXP s_string,
  SEXP s_as_named_list,
  SEXP s_handlers,
  SEXP s_error_label,
  SEXP s_eval_expr,
  SEXP s_eval_warning,
  SEXP s_merge_precedence,
  SEXP s_merge_warning)
{
  SEXP s_retval = NULL, s_stack_head = NULL, s_stack_tail = NULL,
       s_aliases_head = NULL, s_aliases_tail = NULL, s_anchor = NULL;
  yaml_parser_t parser;
  yaml_event_t event;
  const char *string = NULL, *error_label = NULL, *merge_precedence = NULL;
  char *error_msg_copy = NULL;
  long len = 0;
  int as_named_list = 0, done = 0, err = 0, eval_expr = 0, eval_warning = 0,
      merge_override = 0, merge_warning = 0;

  if (!isString(s_string) || length(s_string) != 1) {
    error("string argument must be a character vector of length 1");
    return R_NilValue;
  }

  if (!isLogical(s_as_named_list) || length(s_as_named_list) != 1) {
    error("as.named.list argument must be a logical vector of length 1");
    return R_NilValue;
  }

  if (s_error_label == R_NilValue) {
    error_label = NULL;
  }
  else if (!isString(s_error_label) || length(s_error_label) != 1) {
    error("error.label argument must be either NULL or a character vector of length 1");
    return R_NilValue;
  } else {
    error_label = CHAR(STRING_ELT(s_error_label, 0));
  }

  if (!isLogical(s_eval_expr) || length(s_eval_expr) != 1) {
    error("eval.expr argument must be a logical vector of length 1");
    return R_NilValue;
  }

  if (!isLogical(s_eval_warning) || length(s_eval_warning) != 1) {
    error("eval.warning argument must be a logical vector of length 1");
    return R_NilValue;
  }

  if (!isString(s_merge_precedence) || length(s_merge_precedence) != 1) {
    error("merge.precedence argument must be a character vector of length 1");
    return R_NilValue;
  }
  else {
    merge_precedence = CHAR(STRING_ELT(s_merge_precedence, 0));
    if (strcmp(merge_precedence, "order") == 0) {
      merge_override = 0;
    }
    else if (strcmp(merge_precedence, "override") == 0) {
      merge_override = 1;
    }
    else {
      error("merge.precedence must be either 'ordered' or 'override'");
      return R_NilValue;
    }
  }

  if (!isLogical(s_merge_warning) || length(s_merge_warning) != 1) {
    error("merge.warning argument must be a logical vector of length 1");
    return R_NilValue;
  }

  PROTECT(s_handlers = Ryaml_sanitize_handlers(s_handlers));

  string = CHAR(STRING_ELT(s_string, 0));
  len = length(STRING_ELT(s_string, 0));
  as_named_list = LOGICAL(s_as_named_list)[0];
  eval_expr = LOGICAL(s_eval_expr)[0];
  eval_warning = LOGICAL(s_eval_warning)[0];
  merge_warning = LOGICAL(s_merge_warning)[0];

  yaml_parser_initialize(&parser);
  yaml_parser_set_input_string(&parser, (const unsigned char *)string, len);

  PROTECT(s_stack_head = s_stack_tail = list1(Ryaml_Sentinel));
  PROTECT(s_aliases_head = s_aliases_tail = list1(Ryaml_Sentinel));
  Ryaml_error_msg[0] = 0;
  while (!done) {
    if (yaml_parser_parse(&parser, &event)) {
      err = 0;

      switch (event.type) {
        case YAML_NO_EVENT:
        case YAML_STREAM_START_EVENT:
        case YAML_DOCUMENT_START_EVENT:
        case YAML_DOCUMENT_END_EVENT:
          break;

        case YAML_ALIAS_EVENT:
#if DEBUG
          Rprintf("ALIAS: %s\n", event.data.alias.anchor);
#endif
          handle_alias(&event, &s_stack_tail, s_aliases_head);
          break;

        case YAML_SCALAR_EVENT:
#if DEBUG
          Rprintf("SCALAR: %s (%s) [%s]\n", event.data.scalar.value, event.data.scalar.tag, event.data.scalar.anchor);
#endif
          err = handle_scalar(&event, &s_stack_tail, s_handlers, eval_expr, eval_warning);
          if (!err && event.data.scalar.anchor != NULL) {
            PROTECT(s_anchor = mkCharCE((char *)event.data.scalar.anchor, CE_UTF8));
            possibly_record_alias(s_anchor, &s_aliases_tail, CAR(s_stack_tail));
            UNPROTECT(1);
          }
          break;

        case YAML_SEQUENCE_START_EVENT:
#if DEBUG
          Rprintf("SEQUENCE START: (%s) [%s]\n", event.data.sequence_start.tag, event.data.sequence_start.anchor);
#endif
          handle_structure_start(&event, &s_stack_tail, 0);
          break;

        case YAML_SEQUENCE_END_EVENT:
#if DEBUG
          Rprintf("SEQUENCE END\n");
#endif
          err = handle_sequence(&event, s_stack_head, &s_stack_tail, s_handlers, as_named_list);
          if (!err) {
            s_anchor = CADR(TAG(s_stack_tail));
            possibly_record_alias(s_anchor, &s_aliases_tail, CAR(s_stack_tail));
            SET_TAG(s_stack_tail, R_NilValue);
          }
          break;

        case YAML_MAPPING_START_EVENT:
#if DEBUG
          Rprintf("MAPPING START: (%s) [%s]\n", event.data.mapping_start.tag, event.data.mapping_start.anchor);
#endif
          handle_structure_start(&event, &s_stack_tail, 1);
          break;

        case YAML_MAPPING_END_EVENT:
#if DEBUG
          Rprintf("MAPPING END\n");
#endif
          err = handle_map(&event, s_stack_head, &s_stack_tail, s_handlers, as_named_list, merge_override, merge_warning);
          if (!err) {
            s_anchor = CADR(TAG(s_stack_tail));
            possibly_record_alias(s_anchor, &s_aliases_tail, CAR(s_stack_tail));
            SET_TAG(s_stack_tail, R_NilValue);
          }

          break;

        case YAML_STREAM_END_EVENT:
          if (CADR(s_stack_head) != Ryaml_Sentinel) {
            s_retval = CADR(s_stack_head);
          }
          else {
            s_retval = R_NilValue;
          }

          done = 1;
          break;
      }

      if (err) {
        s_retval = R_NilValue;
        done = 1;
      }
    }
    else {
      s_retval = R_NilValue;

      /* Parser error */
      switch (parser.error) {
        case YAML_MEMORY_ERROR:
          Ryaml_set_error_msg("Memory error: Not enough memory for parsing");
          break;

        case YAML_READER_ERROR:
          if (parser.problem_value != -1) {
            Ryaml_set_error_msg("Reader error: %s: #%X at %d", parser.problem,
              parser.problem_value, (int)parser.problem_offset);
          }
          else {
            Ryaml_set_error_msg("Reader error: %s at %d", parser.problem,
              (int)parser.problem_offset);
          }
          break;

        case YAML_SCANNER_ERROR:
          if (parser.context) {
            Ryaml_set_error_msg("Scanner error: %s at line %d, column %d "
              "%s at line %d, column %d\n", parser.context,
              (int)parser.context_mark.line+1,
              (int)parser.context_mark.column+1,
              parser.problem, (int)parser.problem_mark.line+1,
              (int)parser.problem_mark.column+1);
          }
          else {
            Ryaml_set_error_msg("Scanner error: %s at line %d, column %d",
              parser.problem, (int)parser.problem_mark.line+1,
              (int)parser.problem_mark.column+1);
          }
          break;

        case YAML_PARSER_ERROR:
          if (parser.context) {
            Ryaml_set_error_msg("Parser error: %s at line %d, column %d "
              "%s at line %d, column %d", parser.context,
              (int)parser.context_mark.line+1,
              (int)parser.context_mark.column+1,
              parser.problem, (int)parser.problem_mark.line+1,
              (int)parser.problem_mark.column+1);
          }
          else {
            Ryaml_set_error_msg("Parser error: %s at line %d, column %d",
              parser.problem, (int)parser.problem_mark.line+1,
              (int)parser.problem_mark.column+1);
          }
          break;

        default:
          /* Couldn't happen unless there is an undocumented/unhandled error
           * from LibYAML. */
          Ryaml_set_error_msg("Internal error: unknown parser error");
          break;
      }
      done = 1;
    }

    yaml_event_delete(&event);
  }
  yaml_parser_delete(&parser);

  if (Ryaml_error_msg[0] != 0) {
    /* Prepend label to error message if specified */
    if (error_label != NULL) {
      error_msg_copy = (char *)malloc(sizeof(char) * ERROR_MSG_SIZE);
      if (error_msg_copy == NULL) {
        Ryaml_set_error_msg("Ran out of memory!");
      } else {
        memcpy(error_msg_copy, Ryaml_error_msg, ERROR_MSG_SIZE);
        Ryaml_set_error_msg("(%s) %s", error_label, error_msg_copy);
        free(error_msg_copy);
      }
    }
    error("%s", Ryaml_error_msg);
  }

  UNPROTECT(3); /* s_stack_head, s_aliases_head, s_handlers */

  return s_retval;
}
