#define STRICT_R_HEADERS
#define R_NO_REMAP
#include "Rinternals.h"
#include <stdlib.h>
#include <string.h>

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

SEXP glue_(
    SEXP x,
    SEXP f,
    SEXP open_arg,
    SEXP close_arg,
    SEXP comment_arg,
    SEXP literal_arg) {
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
  if (Rf_xlength(comment_arg) > 0) {
    comment_char = CHAR(STRING_ELT(comment_arg, 0))[0];
  }

  Rboolean literal = LOGICAL(literal_arg)[0];

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
        i += open_len - 1;
      } else if (strncmp(&xx[i], close, close_len) == 0) {
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
