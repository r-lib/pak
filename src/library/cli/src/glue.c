
#define STRICT_R_HEADERS
#define R_NO_REMAP
#include "Rinternals.h"
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

SEXP set(SEXP x, int i, SEXP val) {
  R_xlen_t len = Rf_xlength(x);
  if (i >= len) {
    len *= 2;
    x = Rf_lengthgets(x, len);
  }
  SET_VECTOR_ELT(x, i, val);
  return x;
}

SEXP resize(SEXP out, R_xlen_t n) {
  if (n == Rf_xlength(out)) {
    return out;
  }
  return Rf_xlengthgets(out, n);
}

// This is almost like the original glue parser, except that
// - no comment_arg (always empty string)
// - no literal_arg
// - If cli_arg is FALSE, then literal = 1, always
// - If cli_arg is TRUE, then we use literal = 1, except in {}
//   evaluations (as opposed to {.} for cli styles), where literal = 0 is
//   used.

SEXP glue_(SEXP x, SEXP f, SEXP open_arg, SEXP close_arg, SEXP cli_arg) {

  typedef enum {
    text,
    escape,
    single_quote,
    double_quote,
    backtick,
    delim,
    comment
  } states;

  const char* xx = Rf_translateCharUTF8(STRING_ELT(x, 0));
  size_t str_len = strlen(xx);

  char* str = (char*)malloc(str_len + 1);

  const char* open = CHAR(STRING_ELT(open_arg, 0));
  size_t open_len = strlen(open);

  const char* close = CHAR(STRING_ELT(close_arg, 0));
  size_t close_len = strlen(close);

  char comment_char = '\0';

  Rboolean cli = LOGICAL(cli_arg)[0];

  Rboolean literal = 1;

  int delim_equal = strncmp(open, close, open_len) == 0;

  SEXP out = Rf_allocVector(VECSXP, 1);
  PROTECT_INDEX out_idx;
  PROTECT_WITH_INDEX(out, &out_idx);

  size_t j = 0;
  size_t k = 0;
  int delim_level = 0;
  size_t start = 0;
  states state = text;
  states prev_state = text;
  size_t i = 0;
  for (i = 0; i < str_len; ++i) {
    switch (state) {
    case text: {
      if (strncmp(&xx[i], open, open_len) == 0) {
        /* check for open delim doubled */
        if (strncmp(&xx[i + open_len], open, open_len) == 0) {
          i += open_len;
        } else {
          state = delim;
          delim_level = 1;
          start = i + open_len;
          // In cli mode we switch to literal = FALSE for a {} block
          if (cli && xx[i + open_len] != '.') literal = 0;
          break;
        }
      }
      if (strncmp(&xx[i], close, close_len) == 0 &&
          strncmp(&xx[i + close_len], close, close_len) == 0) {
        i += close_len;
      }

      str[j++] = xx[i];
      break;
    }
    case escape: {
      state = prev_state;
      break;
    }
    case single_quote: {
      if (xx[i] == '\\') {
        prev_state = single_quote;
        state = escape;
      } else if (xx[i] == '\'') {
        state = delim;
      }
      break;
    }
    case double_quote: {
      if (xx[i] == '\\') {
        prev_state = double_quote;
        state = escape;
      } else if (xx[i] == '\"') {
        state = delim;
      }
      break;
    }
    case backtick: {
      if (xx[i] == '\\') {
        prev_state = backtick;
        state = escape;
      } else if (xx[i] == '`') {
        state = delim;
      }
      break;
    }
    case comment: {
      if (xx[i] == '\n') {
        state = delim;
      }
      break;
    }
    case delim: {
      if (!delim_equal && strncmp(&xx[i], open, open_len) == 0) {
        ++delim_level;
        // In cli mode we switch to literal = FALSE for a {} block
        if (cli && xx[i + open_len] != '.') literal = 0;
        i += open_len - 1;
      } else if (strncmp(&xx[i], close, close_len) == 0) {
        // in non-{} blocks (i.e. {.}) we need literal = 1. We can always
        // switch to literal = 1 here, because {} blocks cannot be nested.
        literal = 1;
        --delim_level;
        i += close_len - 1;
      } else {
        if (!literal && xx[i] == comment_char) {
          state = comment;
        } else {
          switch (xx[i]) {
          case '\'':
            if (!literal) {
              state = single_quote;
            }
            break;
          case '"':
            if (!literal) {
              state = double_quote;
            }
            break;
          case '`':
            if (!literal) {
              state = backtick;
            }
            break;
          };
        }
      }
      if (delim_level == 0) {
        /* Result of the current glue statement */
        SEXP expr = PROTECT(Rf_ScalarString(
            Rf_mkCharLenCE(&xx[start], (i - close_len) + 1 - start, CE_UTF8)));
        SEXP call = PROTECT(Rf_lang2(f, expr));
        SEXP result = PROTECT(Rf_eval(call, R_EmptyEnv));

        /* text in between last glue statement */
        if (j > 0) {
          str[j] = '\0';
          SEXP str_ = PROTECT(Rf_ScalarString(Rf_mkCharLenCE(str, j, CE_UTF8)));
          REPROTECT(out = set(out, k++, str_), out_idx);
          UNPROTECT(1);
        }

        REPROTECT(out = set(out, k++, result), out_idx);

        /* Clear the string buffer */
        memset(str, 0, j);
        j = 0;
        UNPROTECT(3);
        state = text;
      }
      break;
    }
    };
  }

  if (k == 0 || j > 0) {
    str[j] = '\0';
    SEXP str_ = PROTECT(Rf_ScalarString(Rf_mkCharLenCE(str, j, CE_UTF8)));
    REPROTECT(out = set(out, k++, str_), out_idx);
    UNPROTECT(1);
  }

  if (state == delim) {
    free(str);
    Rf_error("Expecting '%s'", close);
  } else if (state == single_quote) {
    free(str);
    Rf_error("Unterminated quote (')");
  } else if (state == double_quote) {
    free(str);
    Rf_error("Unterminated quote (\")");
  } else if (state == backtick) {
    free(str);
    Rf_error("Unterminated quote (`)");
  } else if (state == comment) {
    free(str);
    Rf_error("Unterminated comment");
  }

  free(str);

  out = resize(out, k);

  UNPROTECT(1);

  return out;
}

SEXP trim_(SEXP x) {
  size_t len = LENGTH(x);

  SEXP out = PROTECT(Rf_allocVector(STRSXP, len));
  size_t num;
  for (num = 0; num < len; ++num) {
    const char* xx = Rf_translateCharUTF8(STRING_ELT(x, num));
    size_t str_len = strlen(xx);

    char* str = (char*)malloc(str_len + 1);
    size_t i = 0, start = 0;
    bool new_line = false;

    /* skip leading blanks on first line */
    while (start < str_len && (xx[start] == ' ' || xx[start] == '\t')) {
      ++start;
    }

    /* Skip first newline */
    if (start < str_len && xx[start] == '\n') {
      new_line = true;
      ++start;
    }

    i = start;

    /* Ignore first line */
    if (!new_line) {
      while (i < str_len && xx[i] != '\n') {
        ++i;
      }
      new_line = true;
    }

    size_t indent = 0;

    /* Maximum size of size_t */
    size_t min_indent = (size_t)-1;

    /* find minimum indent */
    while (i < str_len) {
      if (xx[i] == '\n') {
        new_line = true;
        indent = 0;
      } else if (new_line) {
        if (xx[i] == ' ' || xx[i] == '\t') {
          ++indent;
        } else {
          if (indent < min_indent) {
            min_indent = indent;
          }
          indent = 0;
          new_line = false;
        }
      }
      ++i;
    }

    /* if string ends with '\n', `indent = 0` only because we made it so */
    if (xx[str_len - 1] != '\n' && new_line && indent < min_indent) {
      min_indent = indent;
    }

    new_line = true;
    i = start;
    size_t j = 0;

    /*Rprintf("start: %i\nindent: %i\nmin_indent: %i", start, indent,
     * min_indent);*/

    /* copy the string removing the minimum indent from new lines */
    while (i < str_len) {
      if (xx[i] == '\n') {
        new_line = true;
      } else if (xx[i] == '\\' && i + 1 < str_len && xx[i + 1] == '\n') {
        new_line = true;
        i += 2;
        continue;
      } else if (new_line) {
        size_t skipped = strspn(xx + i, "\t ");
        /*
         * if the line consists only of tabs and spaces, and if the line is
         * shorter than min_indent, copy the entire line and proceed to the
         * next
         */
        if (*(xx + i + skipped) == '\n' && skipped < min_indent) {
          strncpy(str + j, xx + i, skipped);
          i += skipped;
          j += skipped;
        } else {
          if (i + min_indent < str_len && (xx[i] == ' ' || xx[i] == '\t')) {
            i += min_indent;
          }
        }
        new_line = false;
        continue;
      }
      str[j++] = xx[i++];
    }
    str[j] = '\0';

    /* Remove trailing whitespace up to the first newline */
    size_t end = j;
    while (j > 0) {
      if (str[j] == '\n') {
        end = j;
        break;
      } else if (str[j] == '\0' || str[j] == ' ' || str[j] == '\t') {
        --j;
      } else {
        break;
      }
    }
    str[end] = '\0';
    SET_STRING_ELT(out, num, Rf_mkCharCE(str, CE_UTF8));
    free(str);
  }

  UNPROTECT(1);
  return out;
}
