
/* Much of this is adapted from the great utf8lite library:
 * https://github.com/patperry/utf8lite
 */

#include "charwidth.h"
#include "graphbreak.h"
#include "cli.h"
#include "errors.h"

void utf8lite_decode_utf8(const uint8_t **bufptr, int32_t *codeptr) {
  const uint8_t *ptr = *bufptr;
  int32_t code;
  uint_fast8_t ch;
  unsigned nc;

  ch = *ptr++;
  if (!(ch & 0x80)) {
    code = ch;
    nc = 0;
  } else if (!(ch & 0x20)) {
    code = ch & 0x1F;
    nc = 1;
  } else if (!(ch & 0x10)) {
    code = ch & 0x0F;
    nc = 2;
  } else {
    code = ch & 0x07;
    nc = 3;
  }

  while (nc-- > 0) {
    ch = *ptr++;
    if (ch == 0) R_THROW_ERROR("Incomplete UTF-8 character");
    code = (code << 6) + (ch & 0x3F);
  }

  *bufptr = ptr;
  *codeptr = code;
}

void utf8lite_encode_utf8(int32_t code, uint8_t **bufptr) {
  uint8_t *ptr = *bufptr;
  int32_t x = code;

  if (x <= 0x7F) {
    *ptr++ = (uint8_t)x;
  } else if (x <= 0x07FF) {
    *ptr++ = (uint8_t)(0xC0 | (x >> 6));
    *ptr++ = (uint8_t)(0x80 | (x & 0x3F));
  } else if (x <= 0xFFFF) {
    *ptr++ = (uint8_t)(0xE0 | (x >> 12));
    *ptr++ = (uint8_t)(0x80 | ((x >> 6) & 0x3F));
    *ptr++ = (uint8_t)(0x80 | (x & 0x3F));
  } else {
    *ptr++ = (uint8_t)(0xF0 | (x >> 18));
    *ptr++ = (uint8_t)(0x80 | ((x >> 12) & 0x3F));
    *ptr++ = (uint8_t)(0x80 | ((x >> 6) & 0x3F));
    *ptr++ = (uint8_t)(0x80 | (x & 0x3F));
  }

  *bufptr = ptr;
}

static int display_width_map[7] = {
  /* CHARWIDTH_NONE =      */ 0,
  /* CHARWIDTH_IGNORABLE = */ 0,
  /* CHARWIDTH_MARK =      */ 0,
  /* CHARWIDTH_NARROW =    */ 1,
  /* CHARWIDTH_AMBIGUOUS = */ 1,
  /* CHARWIDTH_WIDE =      */ 2,
  /* CHARWIDTH_EMOJI =     */ 2
};

#define NEXT() do {                                             \
    iter->cnd = iter->nxt_ptr;                                  \
    if (*(iter->nxt_ptr) == '\0') {                             \
      iter->nxt_prop = -1;                                      \
    } else {                                                    \
      utf8lite_decode_utf8(&iter->nxt_ptr, &iter->nxt_code);    \
      iter->nxt_prop = graph_break(iter->nxt_code);             \
    }                                                           \
    if (iter->cnd_width_done >= 0) {                            \
      if (iter->nxt_cw >= 0) {                                  \
        if (!iter->cnd_width_done) {                            \
          iter->cnd_width += display_width_map[iter->nxt_cw];   \
          if (iter->nxt_cw == CHARWIDTH_EMOJI) {                \
            iter->cnd_width_done = 1;                           \
          }                                                     \
        }                                                       \
      }                                                         \
      if (iter->nxt_prop != -1) {                               \
        iter->nxt_cw = charwidth(iter->nxt_code);               \
      }                                                         \
    }                                                           \
  } while (0)

void clic_utf8_graphscan_make(struct grapheme_iterator *iter,
                              const uint8_t *txt,
                              int width) {
  iter->nxt_ptr = txt;
  iter->nxt_cw = -1;
  iter->cnd_width = 0;
  iter->cnd_width_done = -1 * (width == 0);
  NEXT();
}

void clic_utf8_graphscan_next(struct grapheme_iterator *iter,
                              uint8_t **ptr,
                              int *width) {
  if (ptr) *ptr = (uint8_t*) iter->cnd;

 Start:
  // GB2: Break at the end of text
  if (iter->nxt_prop < 0) {
    goto Break;
  }

  switch (iter->nxt_prop) {
  case GRAPH_BREAK_CR:
    NEXT();
    goto CR;

  case GRAPH_BREAK_CONTROL:
  case GRAPH_BREAK_LF:
    // Break after controls
    // GB4: (Newline | LF) +
    NEXT();
    goto Break;

  case GRAPH_BREAK_L:
    NEXT();
    goto L;

  case GRAPH_BREAK_LV:
  case GRAPH_BREAK_V:
    NEXT();
    goto V;

  case GRAPH_BREAK_LVT:
  case GRAPH_BREAK_T:
    NEXT();
    goto T;

  case GRAPH_BREAK_PREPEND:
    NEXT();
    goto Prepend;

  case GRAPH_BREAK_EXTENDED_PICTOGRAPHIC:
    NEXT();
    goto Extended_Pictographic;

  case GRAPH_BREAK_REGIONAL_INDICATOR:
    NEXT();
    goto Regional_Indicator;

  case GRAPH_BREAK_EXTEND:
  case GRAPH_BREAK_SPACINGMARK:
  case GRAPH_BREAK_ZWJ:
  case GRAPH_BREAK_OTHER:
    NEXT();
    goto MaybeBreak;
  }

  R_THROW_ERROR("internal error, unhandled grapheme break property");

 CR:
  // GB3: Do not break within CRLF
  // GB4: Otherwise break after controls
  if (iter->nxt_prop == GRAPH_BREAK_LF) {
    NEXT();
  }
  goto Break;

 L:
  // GB6: Do not break Hangul syllable sequences.
  switch (iter->nxt_prop) {
  case GRAPH_BREAK_L:
    NEXT();
    goto L;

  case GRAPH_BREAK_V:
  case GRAPH_BREAK_LV:
    NEXT();
    goto V;

  case GRAPH_BREAK_LVT:
    NEXT();
    goto T;

  default:
    goto MaybeBreak;
  }


 V:
  // GB7: Do not break Hangul syllable sequences.
  switch (iter->nxt_prop) {
  case GRAPH_BREAK_V:
    NEXT();
    goto V;

  case GRAPH_BREAK_T:
    NEXT();
    goto T;

  default:
    goto MaybeBreak;
  }

 T:
  // GB8: Do not break Hangul syllable sequences.
  switch (iter->nxt_prop) {
  case GRAPH_BREAK_T:
    NEXT();
    goto T;

  default:
    goto MaybeBreak;
  }

 Prepend:
  switch (iter->nxt_prop) {
  case GRAPH_BREAK_CONTROL:
  case GRAPH_BREAK_CR:
  case GRAPH_BREAK_LF:
    // GB5: break before controls
    goto Break;

  default:
    // GB9b: do not break after Prepend characters.
    goto Start;
  }

 Extended_Pictographic:
  // GB9:  Do not break before extending characters
  while (iter->nxt_prop == GRAPH_BREAK_EXTEND) {
    NEXT();
  }
  // GB9: Do not break before ZWJ
  if (iter->nxt_prop == GRAPH_BREAK_ZWJ) {
    NEXT();
    // GB11: Do not break within emoji modifier sequences
    // or emoji zwj sequences.
    if (iter->nxt_prop == GRAPH_BREAK_EXTENDED_PICTOGRAPHIC) {
      NEXT();
      goto Extended_Pictographic;
    }
  }
  goto MaybeBreak;

 Regional_Indicator:
  // Do not break within emoji flag sequences. That is, do not break
  // between regional indicator (RI) symbols if there is an odd number
  // of RI characters before the break point
  if (iter->nxt_prop == GRAPH_BREAK_REGIONAL_INDICATOR) {
    // GB12/13: [^RI] RI * RI
    NEXT();
  }
  goto MaybeBreak;

 MaybeBreak:
  // GB9: Do not break before extending characters or ZWJ.
  // GB9a: Do not break before SpacingMark [extended grapheme clusters]
  // GB999: Otherwise, break everywhere
  switch (iter->nxt_prop) {
  case GRAPH_BREAK_EXTEND:
  case GRAPH_BREAK_SPACINGMARK:
  case GRAPH_BREAK_ZWJ:
    NEXT();
    goto MaybeBreak;

  default:
    goto Break;
  }

 Break:
  if (width) *width = iter->cnd_width;
  iter->cnd_width = 0;
  if (iter->cnd_width_done > 0) iter->cnd_width_done = 0;
  return;
}

SEXP clic_utf8_display_width(SEXP x) {
  R_xlen_t i, len = XLENGTH(x);
  SEXP res = PROTECT(allocVector(INTSXP, len));
  int *pres = INTEGER(res);

  for (i = 0; i < len; i++) {
    SEXP x1 = STRING_ELT(x, i);
    if (x1 == NA_STRING) {
      pres[i] = NA_INTEGER;
    } else {
      struct grapheme_iterator iter;
      const uint8_t *chr = (const uint8_t*) CHAR(x1);
      clic_utf8_graphscan_make(&iter, chr, /* width= */ 1);
      int len = 0;
      int width;
      while (iter.nxt_prop != -1) {
        clic_utf8_graphscan_next(&iter, NULL, &width);
        len += width;
      }
      pres[i] = len;
    }
  }

  UNPROTECT(1);
  return res;
}

SEXP clic_utf8_nchar_graphemes(SEXP x) {
  R_xlen_t i, len = XLENGTH(x);
  SEXP res = PROTECT(allocVector(INTSXP, len));
  int *pres = INTEGER(res);

  for (i = 0; i < len; i++) {
    SEXP x1 = STRING_ELT(x, i);
    if (x1 == NA_STRING) {
      pres[i] = NA_INTEGER;
    } else {
      struct grapheme_iterator iter;
      const uint8_t *chr = (const uint8_t*) CHAR(x1);
      int len = 0;
      clic_utf8_graphscan_make(&iter, chr, /* width= */ 0);
      while (iter.nxt_prop != -1) {
        clic_utf8_graphscan_next(&iter, NULL, NULL);
        len ++;
      }

      pres[i] = len;
    }
  }

  UNPROTECT(1);
  return res;
}

SEXP clic_utf8_substr(SEXP x, SEXP sstart, SEXP sstop) {
  R_xlen_t i, len = XLENGTH(x);
  SEXP res = PROTECT(allocVector(STRSXP, len));

  for (i = 0; i < len; i++) {
    SEXP x1 = STRING_ELT(x, i);
    if (x1 == NA_STRING) {
      SET_STRING_ELT(res, i, x1);
    } else {
      const uint8_t *str = (const uint8_t*) CHAR(x1);
      int pos = 1;
      int start = INTEGER(sstart)[i];
      int stop = INTEGER(sstop)[i];
      struct grapheme_iterator iter;

      /* Skip initial part */
      clic_utf8_graphscan_make(&iter, (const uint8_t*) str, /* width = */ 0);
      while (pos < start && iter.nxt_prop != -1) {
        clic_utf8_graphscan_next(&iter, NULL, NULL);
        pos++;
      }
      const char* from = (const char*) iter.cnd;

      /* Iterate to the end */
      while (pos <= stop && iter.nxt_prop != -1) {
        clic_utf8_graphscan_next(&iter, NULL, NULL);
        pos++;
      }
      const char *to = (const char*) iter.cnd;

      if (from < to) {
        SET_STRING_ELT(res, i, Rf_mkCharLenCE(from, to - from, CE_UTF8));
      }
    }
  }

  UNPROTECT(1);
  return res;
}

SEXP clic_utf8_graphemes(SEXP x) {
  R_xlen_t i, len = XLENGTH(x);
  SEXP res = PROTECT(allocVector(VECSXP, len));

  for (i = 0; i < len; i++) {
    SEXP x1 = STRING_ELT(x, i);
    if (x1 == NA_STRING) {
      SET_VECTOR_ELT(res, i, Rf_ScalarString(x1));
    } else {
      const uint8_t *str = (const uint8_t*) CHAR(x1);
      struct grapheme_iterator iter;
      SEXP pieces = PROTECT(allocVector(STRSXP, strlen((const char*) str)));
      R_xlen_t idx = 0;
      uint8_t *start = 0;

      clic_utf8_graphscan_make(&iter, str, /* width = */ 0);
      while (iter.nxt_prop != -1) {
        clic_utf8_graphscan_next(&iter, &start, NULL);
        SET_STRING_ELT(
          pieces,
          idx++,
          Rf_mkCharLenCE((const char*) start, iter.cnd - start, CE_UTF8)
        );
      }

      SET_VECTOR_ELT(res, i, PROTECT(Rf_xlengthgets(pieces, idx)));
      UNPROTECT(2);
    }
  }

  UNPROTECT(1);
  return res;
}
