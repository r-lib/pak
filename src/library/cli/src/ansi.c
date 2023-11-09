
#include "cli.h"
#include "errors.h"
#include "cleancall.h"

#include <string.h>
#include <stdlib.h>

/* ---------------------------------------------------------------------- */

#define BUFFER_SIZE 4096

static char static_buffer[BUFFER_SIZE];

struct cli_buffer {
  char *buf;
  char *ptr;
  size_t size;
};

static void clic__buffer_init(struct cli_buffer *buf);
static void clic__buffer_free(struct cli_buffer *buf);
static void clic__buffer_reset(struct cli_buffer *buf);
static inline char *clic__buffer_get(struct cli_buffer *buf);
static inline size_t clic__buffer_size(struct cli_buffer *buf);
static inline void clic__buffer_push_str(struct cli_buffer *buf,
                                         const char *str);
/* static inline void clic__buffer_push_str_len(struct cli_buffer *buf, */
/*                                              const char *str, */
/*                                              size_t len); */
static inline void clic__buffer_push_piece(struct cli_buffer *buf,
                                           const char *from,
                                           const char *to);
static void clic__buffer_realloc(struct cli_buffer *buf, size_t size);
static void clic__buffer_checklen(struct cli_buffer *buf, size_t len);

static void clic__buffer_init(struct cli_buffer *buf) {
  buf->buf = static_buffer;
  buf->ptr = static_buffer;
  buf->size = sizeof(static_buffer);
}

static void clic__buffer_reset(struct cli_buffer *buf) {
  buf->ptr = buf->buf;
}

static inline char *clic__buffer_get(struct cli_buffer *buf) {
  return buf->buf;
}

static inline size_t clic__buffer_size(struct cli_buffer *buf) {
  return buf->ptr - buf->buf;
}

static void clic__buffer_free(struct cli_buffer *buf) {
  if (buf->buf != static_buffer) free(buf->buf);
}

static inline void clic__buffer_push_str(struct cli_buffer *buf,
                                         const char *str) {
  size_t len = strlen(str);
  clic__buffer_checklen(buf, len);
  strcpy(buf->ptr, str);
  buf->ptr += len;
}

/* static inline void clic__buffer_push_str_len(struct cli_buffer *buf, */
/*                                              const char *str, */
/*                                              size_t len) { */
/*   clic__buffer_checklen(buf, len); */
/*   memcpy(buf->ptr, str, len); */
/*   buf->ptr += len; */
/* } */

static inline void clic__buffer_push_piece(struct cli_buffer *buf,
                                           const char *from,
                                           const char *to) {
  size_t len = to - from;
  clic__buffer_checklen(buf, len);
  memcpy(buf->ptr, from, len);
  buf->ptr += len;
}

static void clic__buffer_realloc(struct cli_buffer *buf, size_t size) {
  size_t current = buf->ptr - buf->buf;
  char *old = buf->buf;
  buf->size = size;
  if (buf->buf == static_buffer) {
    buf->buf = malloc(size);
    if (!buf->buf) R_THROW_SYSTEM_ERROR("ANSI string error");
    memcpy(buf->buf, old, current);
  } else {
    buf->buf = realloc(buf->buf, size);
    if (!buf->buf) R_THROW_SYSTEM_ERROR("ANSI string error");
  }
  buf->ptr = buf->buf + current;
}

 static void clic__buffer_checklen(struct cli_buffer *buf, size_t len) {

  if (buf->ptr + len >= buf->buf + buf->size) {
    size_t current = buf->ptr - buf->buf;
    size_t prop = buf->size * 2;
    if (prop < current + len) prop = current + len;
    clic__buffer_realloc(buf, prop);
  }
}

/* ---------------------------------------------------------------------- */

static int clic__hyperlink_mode_posix(void) {
  char *ev = getenv("R_CLI_HYPERLINK_MODE");
  if (ev == NULL) return 0;
  return !strcmp("posix", ev);
}

#define CLI_COL_256 254
#define CLI_COL_RGB 255

struct cli_color {
  /* 0 is off
   * 30-37, 40-47, 90-97, 100-107
   * CLI_COL_256 (254) is 8 bit
   * CLI_COL_RGB (255) is 24 bit */
  unsigned char col;
  unsigned char r, g, b;
};

#define DIFFERENT_COLOR(c1,c2) memcmp(&(c1), &(c2), sizeof(struct cli_color))

struct cli_sgr_state {
  struct cli_color fg;
  struct cli_color bg;
  char bold;
  char faint;
  char italic;
  char underline;
  char blink;
  char inverse;
  char hide;
  char crossedout;
  const char* link_param;
  const char* link_uri;
  const char* link_end;
};

struct cli_ansi_state {
  struct cli_sgr_state old;
  struct cli_sgr_state new;
  char unknown;
  char off;
};

static void clic__readnum(char **ptr, unsigned int *num) {
  int len = 0;
  if ((*ptr)[0] != ';') return;
  (*ptr) ++;
  sscanf(*ptr, "%u%n", num, &len);
  *ptr += len;
  while (**ptr != ';' && **ptr != '\0') (*ptr) ++;
}

static void clic__parse_color(char **ptr, const char *end, struct cli_color *col) {
  /* This can be:
   * - 5;<n>
   * - 2;<r>;<g>;<b>
   */

  /* Has to start with ;5; or ;2;, otherwise we skip the whole tag */
  if ((*ptr)[0] != ';' ||
      ((*ptr)[1] != '5' && (*ptr)[1] != '2') ||
      (*ptr)[2] != ';') {
    *ptr = (char*) end;
    col->r = col->g = col->b = 0;
    return;
  }

  col->col = (*ptr)[1] == '5' ? CLI_COL_256 : CLI_COL_RGB;
  (*ptr) += 2;

  /* Temporarily create a zero terminated string for sscanf */
  char backup = *end;
  char *end2 = (char*) end;
  *end2 = '\0';

  unsigned int r = 0, g = 0, b = 0;

  clic__readnum(ptr, &r);
  if (col->col == CLI_COL_RGB) {
    clic__readnum(ptr, &g);
    clic__readnum(ptr, &b);
  }

  col->r = (unsigned char) r;
  col->g = (unsigned char) g;
  col->b = (unsigned char) b;

  *end2 = backup;
}

static void clic__ansi_update_state(const char *param,
                                    const char *intermed,
                                    const char *end,
                                    struct cli_buffer *buffer,
                                    struct cli_ansi_state *state) {

  char *startptr = (char*) param, *endptr;
  do {
    long num = strtol(startptr, &endptr, 10);
    if (endptr == startptr || num == 0) {
      memset(&state->new, 0, sizeof(state->new));
      state->off = 1;

    } else if (num == 1) {
      state->new.bold = 1;

    } else if (num == 2) {
      state->new.faint = 1;

    } else if (num == 3) {
      state->new.italic = 1;

    } else if (num == 4) {
      state->new.underline = 1;

    } else if (num == 5) {
      state->new.blink = 1;

    } else if (num == 7) {
      state->new.inverse = 1;

    } else if (num == 8) {
      state->new.hide = 1;

    } else if (num == 9) {
      state->new.crossedout = 1;

    } else if (num == 22) {
      state->new.bold = state->new.faint = 0;

    } else if (num == 23) {
      state->new.italic = 0;

    } else if (num == 24) {
      state->new.underline = 0;

    } else if (num == 25) {
      state->new.blink = 0;

    } else if (num == 27) {
      state->new.inverse = 0;

    } else if (num == 28) {
      state->new.hide = 0;

    } else if (num == 29) {
      state->new.crossedout = 0;

    } else if ((num >= 30 && num <= 37) || (num >= 90 && num <= 97)) {
      state->new.fg.col = num;

    } else if (num == 38) {
      clic__parse_color(&endptr, intermed, &state->new.fg);

    } else if (num == 39) {
      state->new.fg.col = 0;

    } else if ((num >= 40 && num <= 47) || (num >= 100 && num <= 107)) {
      state->new.bg.col = num;

    } else if (num == 48) {
      clic__parse_color(&endptr, intermed, &state->new.bg);

    } else if (num == 49) {
      state->new.bg.col = 0;

    } else {
      /* Keep tag as is, and emit it right away */
      state->unknown = 1;
      clic__buffer_push_piece(buffer, param - 2, end + 1);
    }

    /* The next attribute, if any */
    startptr = endptr + 1;
  } while (endptr < intermed && *endptr == ';');
}

static void clic__ansi_update_state_link(const char *param,
                                         const char *uri,
                                         const char *end,
                                         struct cli_ansi_state *state) {

  if ((*uri == '\033' && *(uri + 1) == '\\') || *uri == '\007') {
    // turn off links
    state->new.link_param = NULL;
    state->new.link_uri = NULL;
    state->new.link_end = NULL;

  } else {
    // start of a link
    state->new.link_param = param;
    state->new.link_uri = uri;
    state->new.link_end = end;
  }
}

#define EMIT(s) clic__buffer_push_str(buffer, "\033[" s "m")
#define EMITS(s) clic__buffer_push_str(buffer, (s))
#define EMITP(s,e) clic__buffer_push_piece(buffer, (s), (e))

static void clic__state_update_buffer(struct cli_buffer *buffer,
                                      struct cli_ansi_state *state) {

  char col[20];

  if (state->unknown && state->off) {
    state->unknown = state->off = 0;
    EMIT("0");
  }

  /* Closing tags ------------------------------------------------------ */

  if (state->old.bg.col != 0 && state->new.bg.col != state->old.bg.col) {
    EMIT("49");
  }

  if (state->old.fg.col != 0 && state->new.fg.col != state->old.fg.col) {
    EMIT("39");
  }

  if (state->new.crossedout < state->old.crossedout) {
    EMIT("29");
  }

  if (state->new.hide < state->old.hide) {
    EMIT("28");
  }

  if (state->new.inverse < state->old.inverse) {
    EMIT("27");
  }

  if (state->new.blink < state->old.blink) {
    EMIT("25");
  }

  if (state->new.underline < state->old.underline) {
    EMIT("24");
  }

  if (state->new.italic < state->old.italic) {
    EMIT("23");
  }

  if (state->new.faint < state->old.faint) {
    EMIT("22");
  }

  if (state->new.bold < state->old.bold) {
    /* TODO: handle bold + faint interaction */
    EMIT("22");
  }

  if (state->old.link_uri && state->new.link_uri != state->old.link_uri) {
    if (clic__hyperlink_mode_posix()) {
      EMITS("\033]8;;\033\\");
    } else {
      EMITS("\033]8;;\007");
    }
  }

  /* Opening tags in reverse order ------------------------------------- */

  if (state->new.link_uri && state->new.link_uri != state->old.link_uri) {
    EMITS("\033]8;");
    // EMITP(state->new.link_param, state->new.link_end + 1);
    EMITP(state->new.link_param, state->new.link_uri);
    if (*(state->new.link_end) == '\007') {
      EMITP(state->new.link_uri, state->new.link_end);
    } else if (*(state->new.link_end) == '\\' && *(state->new.link_end-1) == '\033') {
      EMITP(state->new.link_uri, state->new.link_end - 1);
    } else {
      EMITP(state->new.link_uri, state->new.link_end - 1);
    }
    if (clic__hyperlink_mode_posix()) {
      EMITS("\033\\");
    } else {
      EMITS("\007");
    }
  }

  if (state->new.bold > state->old.bold) {
    EMIT("1");
  }

  if (state->new.faint > state->old.faint) {
    EMIT("2");
  }

  if (state->new.italic > state->old.italic) {
    EMIT("3");
  }

  if (state->new.underline > state->old.underline) {
    EMIT("4");
  }

  if (state->new.blink > state->old.blink) {
    EMIT("5");
  }

  if (state->new.inverse > state->old.inverse) {
    EMIT("7");
  }

  if (state->new.hide > state->old.hide) {
    EMIT("8");
  }

  if (state->new.crossedout > state->old.crossedout) {
    EMIT("9");
  }

  if (state->new.fg.col != 0 &&
      DIFFERENT_COLOR(state->new.fg, state->old.fg)) {
    if (state->new.fg.col == CLI_COL_256) {
      snprintf(col, sizeof(col), "\033[38;5;%um", state->new.fg.r);
    } else if (state->new.fg.col == CLI_COL_RGB) {
      snprintf(col, sizeof(col), "\033[38;2;%u;%u;%um",
               state->new.fg.r, state->new.fg.g, state->new.fg.b);
    } else {
      snprintf(col, sizeof(col), "\033[%um", state->new.fg.col);
    }
    EMITS(col);
  }

  if (state->new.bg.col != 0 &&
      DIFFERENT_COLOR(state->new.bg, state->old.bg)) {
    if (state->new.bg.col == CLI_COL_256) {
      snprintf(col, sizeof(col), "\033[48;5;%um", state->new.bg.r);
    } else if (state->new.bg.col == CLI_COL_RGB) {
      snprintf(col, sizeof(col), "\033[48;2;%u;%u;%um",
               state->new.bg.r, state->new.bg.g, state->new.bg.b);
    } else {
      snprintf(col, sizeof(col), "\033[%um", state->new.bg.col);
    }
    EMITS(col);
  }

  state->old = state->new;
}

typedef int (*clic__start_callback_t)(SEXP rstr,
                                      const char *str,
                                      void *data);
typedef int (*clic__tag_callback_t)(const char *param,
                                   const char *intermed,
                                   const char *end,
                                   void *data);
typedef int (*clic__text_callback_t)(const char *str,
                                      const char *end,
                                      void *data);
typedef int (*clic__end_callback_t)(SEXP rstr,
                                    const char *str,
                                    void *data);

void clic__ansi_iterator(SEXP sx,
                         clic__start_callback_t start_cb,
                         clic__tag_callback_t sgr_cb,
                         clic__tag_callback_t csi_cb,
                         clic__tag_callback_t link_cb,
                         clic__text_callback_t text_cb,
                         clic__end_callback_t end_cb,
                         void *data) {

  R_xlen_t i, len = XLENGTH(sx);
  for (i = 0; i < len; i++) {
    SEXP str = STRING_ELT(sx, i);
    const char *ox = CHAR(str);
    const char *x = ox;
    const char *shaft = ox;
    const char *s_start;
    const char *s_param;
    const char *s_intermed;
    const char *s_end;
    const char *s_uri;

    if (start_cb) if (start_cb(str, ox, data)) goto end;
    if (str == NA_STRING) goto end;

    while (*x != 0) {
      if (*x == '\033' && *(x + 1) == '[') {
        // CSI
        s_start = x;
        s_param = s_intermed = x + 2;
        while (*s_intermed >= 0x30 && *s_intermed <= 0x3f) s_intermed++;
        s_end = s_intermed;
        while (*s_end >= 0x20 && *s_end <= 0x2f) s_end++;
        if (s_start > shaft && text_cb) {
          if (text_cb(shaft, s_start, data)) goto end;
        }
        if (*s_end == 'm') {
          if (sgr_cb) {
            if (sgr_cb(s_param, s_intermed, s_end, data)) goto end;
          }
        } else {
          if (csi_cb) {
            if (csi_cb(s_param, s_intermed, s_end, data)) goto end;
          }
        }
        shaft = s_end + 1;
        x = *s_end ? s_end + 1 : s_end;

      } else if (*x == '\033' && *(x + 1) == ']' && *(x + 2) == '8' &&
                 *(x + 3) == ';') {
        // OSC
        s_start = x;
        s_param = s_uri = x + 4;
        while (*s_uri != ';' && *s_uri != '\0') s_uri++;
        s_uri++;
        s_end = s_uri;
        for (;;) {
          if (*s_end == '\0') break;
          if (*s_end == '\007') break;
          if (*s_end == '\\' && *(s_end - 1) == '\033') break;
          s_end++;
        }
        if (s_start > shaft && text_cb) {
          if (text_cb(shaft, s_start, data)) goto end;
        }
        if (link_cb) {
          if (link_cb(s_param, s_uri, s_end, data)) goto end;
        }
        shaft = s_end + 1;
        x = *s_end ? s_end + 1 : s_end;

      } else {
        x++;
      }
      /* This is not needed, but slightly faster this way */
      while (*x != '\033' && *x != '\0') x++;
    }

    if (x > shaft && text_cb) text_cb(shaft, x, data);

  end:
    if (end_cb) end_cb(str, ox, data);
  }
}

/* ---------------------------------------------------------------------- */

struct simplify_data {
  struct cli_ansi_state state;
  struct cli_buffer buffer;
  R_xlen_t done;
  size_t num_tags;
  SEXP result;
  char keep_csi;
};

static int simplify_cb_start(SEXP rstr, const char *str, void *vdata) {
  struct simplify_data *data = vdata;
  data->num_tags = 0;
  clic__buffer_reset(&data->buffer);
  return 0;
}

static int simplify_cb_sgr(const char *param,
                           const char *intermed,
                           const char *end,
                           void *vdata) {
  struct simplify_data *data = vdata;
  data->num_tags ++;
  clic__ansi_update_state(param, intermed, end, &data->buffer, &data->state);
  return 0;
}

static int simplify_cb_csi(const char *param,
                           const char *intermed,
                           const char *end,
                           void *vdata) {
  struct simplify_data *data = vdata;
  if (data->keep_csi) {
    clic__buffer_push_piece(&data->buffer, param - 2, end + 1);
  } else {
    /* Need to count, to avoid a verbatim STRSXP copy */
    data->num_tags ++;
  }
  return 0;
}

static int simplify_cb_link(const char *param,
                            const char *uri,
                            const char *end,
                            void *vdata) {
  struct simplify_data *data = vdata;
  data->num_tags ++;
  clic__ansi_update_state_link(param, uri, end, &data->state);
  return 0;
}

static int simplify_cb_text(const char *str,
                            const char *end,
                            void *vdata) {
  struct simplify_data *data = vdata;
  clic__state_update_buffer(&data->buffer, &data->state);
  clic__buffer_push_piece(&data->buffer, str, end);
  return 0;
}

static int simplify_cb_end(SEXP rstr,
                           const char *str,
                           void *vdata) {
  struct simplify_data *data = vdata;
  memset(&data->state.new, 0, sizeof(struct cli_sgr_state));
  clic__state_update_buffer(&data->buffer, &data->state);
  if (data->num_tags == 0) {
    SET_STRING_ELT(data->result, data->done, rstr);

  } else {
    SET_STRING_ELT(
      data->result,
      data->done,
      Rf_mkCharLenCE(
        clic__buffer_get(&data->buffer),
        clic__buffer_size(&data->buffer),
        CE_UTF8
      )
    );
  }

  data->done ++;
  return 0;
}

SEXP clic_ansi_simplify(SEXP sx, SEXP keep_csi) {
  struct simplify_data data;
  memset(&data.state, 0, sizeof(data.state));
  clic__buffer_init(&data.buffer);
  data.done = 0;
  data.result = PROTECT(allocVector(STRSXP, XLENGTH(sx)));
  data.keep_csi = LOGICAL(keep_csi)[0];

  clic__ansi_iterator(
    sx,
    simplify_cb_start,
    simplify_cb_sgr,
    simplify_cb_csi,
    simplify_cb_link,
    simplify_cb_text,
    simplify_cb_end,
    &data
  );

  clic__buffer_free(&data.buffer);

  SEXP ocls = PROTECT(getAttrib(sx, R_ClassSymbol));
  int oclslen = isNull(ocls) ? 0 : LENGTH(ocls);
  int has_as = oclslen == 0 ? 0 : Rf_inherits(sx, "cli_ansi_string");
  int has_as2 = oclslen == 0 ? 0 : Rf_inherits(sx, "ansi_string");
  int has_ch = oclslen == 0 ? 0 : Rf_inherits(sx, "character");
  int i, j = 0, clslen = oclslen + !has_as + !has_as2 + !has_ch;
  SEXP cls = PROTECT(allocVector(STRSXP, clslen));
  if (!has_as) SET_STRING_ELT(cls, j++, mkChar("cli_ansi_string"));
  if (!has_as2) SET_STRING_ELT(cls, j++, mkChar("ansi_string"));
  for (i = 0; i < oclslen; i++) {
    SET_STRING_ELT(cls, j++, STRING_ELT(ocls, i));
  }
  if (!has_ch) SET_STRING_ELT(cls, j++, mkChar("character"));
  setAttrib(data.result, R_ClassSymbol, cls);
  UNPROTECT(3);
  return data.result;
}

/* ---------------------------------------------------------------------- */

struct substr_data {
  struct cli_ansi_state state;
  struct cli_buffer buffer;
  R_xlen_t done;
  SEXP result;
  int *start;
  int *stop;
  int pos;
};

static int substr_cb_start(SEXP rstr, const char *str, void *vdata) {
  struct substr_data *data = vdata;
  data->pos = 1;
  clic__buffer_reset(&data->buffer);
  return rstr == NA_STRING;
}

static int substr_cb_sgr(const char *param,
                         const char *intermed,
                         const char *end,
                         void *vdata) {
  struct substr_data *data = vdata;
  clic__ansi_update_state(param, intermed, end, &data->buffer, &data->state);
  return 0;
}

static int substr_cb_link(const char *param,
                          const char *uri,
                          const char *end,
                          void *vdata) {
  struct substr_data *data = vdata;
  clic__ansi_update_state_link(param, uri, end, &data->state);
  return 0;
}

static int substr_cb_text(const char *str,
                          const char *end,
                          void *vdata) {
  struct substr_data *data = vdata;
  int start = data->start[data->done];
  int stop = data->stop[data->done];

  char *end2 = (char*) end;
  char oldend = *end2;
  *end2 = '\0';

  /* Skip before start */
  struct grapheme_iterator iter;
  if (data->pos < start) {
    clic_utf8_graphscan_make(&iter, (const uint8_t*) str, /* width = */ 0);
    while (data->pos < start && iter.nxt_prop != -1) {
      clic_utf8_graphscan_next(&iter, NULL,  NULL);
      data->pos++;
    }
    str = (const char*) iter.cnd;
  }

  /* Add before stop */
  if (data->pos <= stop) {
    const char *from = str;
    clic_utf8_graphscan_make(&iter, (const uint8_t*) str, /* width = */ 0);
    while (data->pos <= stop && iter.nxt_prop != -1) {
      clic_utf8_graphscan_next(&iter, NULL, NULL);
      data->pos++;
    }
    str = (const char*) iter.cnd;
    if (from < str) {
      clic__state_update_buffer(&data->buffer, &data->state);
      clic__buffer_push_piece(&data->buffer, from, str);
    }
  }

  *end2 = oldend;

  /* If we are done, then just close all open tags */
  if (data->pos > stop) {
    memset(&data->state.new, 0, sizeof(struct cli_sgr_state));
    clic__state_update_buffer(&data->buffer, &data->state);
    return 1;
  } else {
    return 0;
  }
}

static int substr_cb_end(SEXP rstr,
                         const char *str,
                         void *vdata) {
  struct substr_data *data = vdata;
  memset(&data->state.new, 0, sizeof(struct cli_sgr_state));
  clic__state_update_buffer(&data->buffer, &data->state);
  if (rstr == NA_STRING) {
    SET_STRING_ELT(data->result, data->done, rstr);
  } else {
    SET_STRING_ELT(
      data->result,
      data->done,
      Rf_mkCharLenCE(
        clic__buffer_get(&data->buffer),
        clic__buffer_size(&data->buffer),
        CE_UTF8
     )
    );
  }

  data->done++;
  return 0;
}

SEXP clic_ansi_substr(SEXP sx, SEXP start, SEXP stop) {
  struct substr_data data;
  memset(&data.state, 0, sizeof(data.state));
  clic__buffer_init(&data.buffer);
  data.done = 0;
  data.result = PROTECT(allocVector(STRSXP, XLENGTH(sx)));
  data.start = INTEGER(start);
  data.stop = INTEGER(stop);

  clic__ansi_iterator(
    sx,
    substr_cb_start,
    substr_cb_sgr,
    NULL,
    substr_cb_link,
    substr_cb_text,
    substr_cb_end,
    &data
  );

  clic__buffer_free(&data.buffer);

  SEXP ocls = PROTECT(getAttrib(sx, R_ClassSymbol));
  int oclslen = isNull(ocls) ? 0 : LENGTH(ocls);
  int has_as = oclslen == 0 ? 0 : Rf_inherits(sx, "cli_ansi_string");
  int has_as2 = oclslen == 0 ? 0 : Rf_inherits(sx, "ansi_string");
  int has_ch = oclslen == 0 ? 0 : Rf_inherits(sx, "character");
  int i, j = 0, clslen = oclslen + !has_as + !has_as2 + !has_ch;
  SEXP cls = PROTECT(allocVector(STRSXP, clslen));
  if (!has_as) SET_STRING_ELT(cls, j++, mkChar("cli_ansi_string"));
  if (!has_as2) SET_STRING_ELT(cls, j++, mkChar("ansi_string"));
  for (i = 0; i < oclslen; i++) {
    SET_STRING_ELT(cls, j++, STRING_ELT(ocls, i));
  }
  if (!has_ch) SET_STRING_ELT(cls, j++, mkChar("character"));
  setAttrib(data.result, R_ClassSymbol, cls);
  UNPROTECT(3);
  return data.result;
}

/* ---------------------------------------------------------------------- */

struct html_data {
  struct cli_ansi_state state;
  struct cli_buffer buffer;
  const char *str;
  R_xlen_t done;
  SEXP result;
  char had_tags;
  char is_link;
  char keep_csi;
};

#define EMITS1(s) do {                            \
  if (first) {                                    \
    EMITS("<span class=\"ansi");                  \
    first = 0;                                    \
  }                                               \
  EMITS(s); } while (0)


static void clic__html_start(struct html_data *data) {

  struct cli_buffer *buffer = &data->buffer;
  struct cli_ansi_state *state = &data->state;

  char col[64];

  int first = 1;
  data->is_link = 0;
 /* Opening tags ------------------------------------------------------ */

  if (state->new.link_uri && state->new.link_uri != state->old.link_uri) {
    EMITS("<a class=\"ansi-link\" href=\"");
    if (*(state->new.link_end) == '\007') {
      EMITP(state->new.link_uri, state->new.link_end);
    } else if (*(state->new.link_end) == '\\' && *(state->new.link_end-1) == '\033') {
      EMITP(state->new.link_uri, state->new.link_end - 1);
    } else {
      EMITP(state->new.link_uri, state->new.link_end - 1);
    }
    EMITS("\">");
    data->is_link = 1;
  }

  if (state->new.bold > state->old.bold) {
    EMITS1(" ansi-bold");
  }

  if (state->new.faint > state->old.faint) {
    EMITS1(" ansi-faint");
  }

  if (state->new.italic > state->old.italic) {
    EMITS1(" ansi-italic");
  }

  if (state->new.underline > state->old.underline) {
    EMITS1(" ansi-underline");
  }

  if (state->new.blink > state->old.blink) {
    EMITS1(" ansi-blink");
  }

  if (state->new.inverse > state->old.inverse) {
    EMITS1(" ansi-inverse");
  }

  if (state->new.hide > state->old.hide) {
    EMITS1(" ansi-hide");
  }

  if (state->new.crossedout > state->old.crossedout) {
    EMITS1(" ansi-crossedout");
  }

  if (state->new.fg.col != 0 &&
      DIFFERENT_COLOR(state->new.fg, state->old.fg)) {
    if (state->new.fg.col == CLI_COL_256) {
      snprintf(col, sizeof(col), " ansi-color-%u", state->new.fg.r);
    } else if (state->new.fg.col == CLI_COL_RGB) {
      snprintf(col, sizeof(col), " ansi-color-%u-%u-%u",
               state->new.fg.r, state->new.fg.g, state->new.fg.b);
    } else {
      unsigned char ncol = state->new.fg.col - 30;
      if (ncol > 7) ncol -= 60;
      snprintf(col, sizeof(col), " ansi-color-%u", ncol);
    }
    EMITS1(col);
  }

  if (state->new.bg.col != 0 &&
      DIFFERENT_COLOR(state->new.bg, state->old.bg)) {
    if (state->new.bg.col == CLI_COL_256) {
      snprintf(col, sizeof(col), " ansi-bg-color-%u",
               state->new.bg.r);
    } else if (state->new.bg.col == CLI_COL_RGB) {
      snprintf(col, sizeof(col), " ansi-bg-color-%u-%u-%u",
               state->new.bg.r, state->new.bg.g, state->new.bg.b);
    } else {
      unsigned char ncol = state->new.bg.col - 40;
      if (ncol > 7) ncol -= 60;
      snprintf(col, sizeof(col), " ansi-bg-color-%u", ncol);
    }
    EMITS1(col);
  }

  state->old = state->new;

  if (!first) EMITS("\">");
  data->had_tags = !first;
}

static void clic__html_end(struct html_data *data) {

  struct cli_buffer *buffer = &data->buffer;
  if (data->had_tags) EMITS("</span>");
  if (data->is_link) EMITS("</a>");
}

static int html_cb_start(SEXP rstr, const char *str, void *vdata) {
  struct html_data *data = vdata;
  clic__buffer_reset(&data->buffer);
  return rstr == NA_STRING;
}

static int html_cb_sgr(const char *param,
                       const char *intermed,
                       const char *end,
                       void *vdata) {
  struct html_data *data = vdata;
  clic__ansi_update_state(param, intermed, end, &data->buffer, &data->state);
  return 0;
}

static int html_cb_csi(const char *param,
                       const char *intermed,
                       const char *end,
                       void *vdata) {
  struct html_data *data = vdata;
  if (data->keep_csi) {
    clic__buffer_push_piece(&data->buffer, param - 2, end + 1);
  }
  return 0;
}

static int html_cb_link(const char *param,
                        const char *uri,
                        const char *end,
                        void *vdata) {
  struct html_data *data = vdata;
  clic__ansi_update_state_link(param, uri, end, &data->state);
  return 0;
}

static int html_cb_text(const char *str,
                        const char *end,
                        void *vdata) {
  struct html_data *data = vdata;
  clic__html_start(data);
  clic__buffer_push_piece(&data->buffer, str, end);
  clic__html_end(data);
  return 0;
}

static int html_cb_end(SEXP rstr,
                       const char *str,
                       void *vdata) {
  struct html_data *data = vdata;
  memset(&data->state.new, 0, sizeof(data->state.new));
  if (rstr == NA_STRING) {
    SET_STRING_ELT(data->result, data->done, rstr);
  } else {
    SET_STRING_ELT(
      data->result,
      data->done,
      Rf_mkCharLenCE(
        clic__buffer_get(&data->buffer),
        clic__buffer_size(&data->buffer),
        CE_UTF8
      )
    );
  }

  data->done++;
  return 0;
}

SEXP clic_ansi_html(SEXP sx, SEXP keep_csi) {
  struct html_data data;
  memset(&data.state, 0, sizeof(data.state));
  clic__buffer_init(&data.buffer);
  data.done = 0;
  data.result = PROTECT(allocVector(STRSXP, XLENGTH(sx)));
  data.keep_csi = LOGICAL(keep_csi)[0];

  clic__ansi_iterator(
    sx,
    html_cb_start,
    html_cb_sgr,
    html_cb_csi,
    html_cb_link,
    html_cb_text,
    html_cb_end,
    &data
  );

  clic__buffer_free(&data.buffer);

  UNPROTECT(1);
  return data.result;
}

/* ---------------------------------------------------------------------- */

struct has_any_data {
  R_xlen_t done;
  SEXP result;
  char sgr;
  char csi;
  char link;
  char has;
};

static int has_any_cb_sgr(const char *param,
                          const char *intermed,
                          const char *end,
                          void *vdata) {
  struct has_any_data *data = vdata;
  if (data->sgr) {
    data->has = 1;
    return 1;
  } else {
    return 0;
  }
}

static int has_any_cb_csi(const char *param,
                          const char *intermed,
                          const char *end,
                          void *vdata) {
  struct has_any_data *data = vdata;
  if (data->csi) {
    data->has = 1;
    return 1;
  } else {
    return 0;
  }
}

static int has_any_cb_link(const char *param,
                           const char *uri,
                           const char *end,
                           void *vdata) {
  struct has_any_data *data = vdata;
  if (data->link) {
    data->has = 1;
    return 1;
  } else {
    return 0;
  }
}

static int has_any_cb_end(SEXP rstr,
                          const char *str,
                          void *vdata) {
  struct has_any_data *data = vdata;
  if (rstr == NA_STRING) {
    LOGICAL(data->result)[data->done] = NA_LOGICAL;
  } else {
    LOGICAL(data->result)[data->done] = data->has;
  }
  data->has = 0;
  data->done ++;
  return 0;
}

SEXP clic_ansi_has_any(SEXP sx, SEXP sgr, SEXP csi, SEXP link) {
  struct has_any_data data;
  data.done = 0;
  data.has = 0;
  data.result = PROTECT(allocVector(LGLSXP, XLENGTH(sx)));
  data.sgr = LOGICAL(sgr)[0];
  data.csi = LOGICAL(csi)[0];
  data.link = LOGICAL(link)[0];

  clic__ansi_iterator(
    sx,
    /* cb_start = */ 0,
    has_any_cb_sgr,
    has_any_cb_csi,
    has_any_cb_link,
    /* cb_text = */ 0,
    has_any_cb_end,
    &data
  );

  UNPROTECT(1);
  return data.result;
}

/* ---------------------------------------------------------------------- */

struct strip_data {
  struct cli_buffer buffer;
  R_xlen_t done;
  size_t num_tags;
  SEXP result;
  char sgr;
  char csi;
  char link;
};

static int strip_cb_start(SEXP rstr, const char *str, void *vdata) {
  struct strip_data *data = vdata;
  data->num_tags = 0;
  clic__buffer_reset(&data->buffer);
  return 0;
}

static int strip_cb_sgr(const char *param,
                        const char *intermed,
                        const char *end,
                        void *vdata) {
  struct strip_data *data = vdata;
  if (data->sgr) {
    data->num_tags ++;
  } else {
    clic__buffer_push_piece(&data->buffer, param - 2, end + 1);
  }
  return 0;
}

static int strip_cb_csi(const char *param,
                        const char *intermed,
                        const char *end,
                        void *vdata) {
  struct strip_data *data = vdata;
  if (data->csi) {
    data->num_tags ++;
  } else {
    clic__buffer_push_piece(&data->buffer, param - 2, end + 1);
  }
  return 0;
}

static int strip_cb_link(const char *param,
                         const char *uri,
                         const char *end,
                         void *vdata) {
  struct strip_data *data = vdata;
  if (data->link) {
    data->num_tags ++;
  } else {
    clic__buffer_push_piece(&data->buffer, param - 4, end + 1);
  }
  return 0;
}

static int strip_cb_text(const char *str,
                        const char *end,
                        void *vdata) {
  struct strip_data *data = vdata;
  clic__buffer_push_piece(&data->buffer, str, end);
  return 0;
}

static int strip_cb_end(SEXP rstr,
                        const char *str,
                        void *vdata) {
  struct strip_data *data = vdata;
  if (data->num_tags == 0) {
    SET_STRING_ELT(data->result, data->done, rstr);

  } else {
    SET_STRING_ELT(
      data->result,
      data->done,
      Rf_mkCharLenCE(
        clic__buffer_get(&data->buffer),
        clic__buffer_size(&data->buffer),
        CE_UTF8
      )
    );
  }

  data->done ++;
  return 0;
}

/* TODO: this would benefit from a non-iterator implementation, that
   would be much faster. */

/* TODO: strip hyperlinks */

SEXP clic_ansi_strip(SEXP sx, SEXP sgr, SEXP csi, SEXP link) {
  struct strip_data data;
  clic__buffer_init(&data.buffer);
  data.done = 0;
  data.result = PROTECT(allocVector(STRSXP, XLENGTH(sx)));
  data.sgr = LOGICAL(sgr)[0];
  data.csi = LOGICAL(csi)[0];
  data.link = LOGICAL(link)[0];

  clic__ansi_iterator(
    sx,
    strip_cb_start,
    strip_cb_sgr,
    strip_cb_csi,
    strip_cb_link,
    strip_cb_text,
    strip_cb_end,
    &data
  );

  clic__buffer_free(&data.buffer);

  UNPROTECT(1);
  return data.result;
}

/* ---------------------------------------------------------------------- */

struct nchar_data {
  R_xlen_t done;
  int *resptr;
  int *result;
};

static int nchar_cb_start(SEXP rstr, const char *str, void *vdata) {
  struct nchar_data *data = vdata;
  data->resptr = data->result + data->done;
  if (rstr == NA_STRING) {
    *data->resptr = NA_INTEGER;
    return 1;
  } else {
    *data->resptr = 0;
    return 0;
  }
}

static int nchar_cb_text_graphemes(const char *str,
                                   const char *end,
                                   void *vdata) {
  struct nchar_data *data = vdata;
  char *end2 = (char*) end;
  char oldend = *end2;
  int len = 0;
  struct grapheme_iterator iter;

  *end2 = '\0';
  clic_utf8_graphscan_make(&iter, (const uint8_t*) str, /* width = */ 0);
  while (iter.nxt_prop != -1) {
    clic_utf8_graphscan_next(&iter, NULL, NULL);
    len ++;
  }
  *data->resptr += len;

  *end2 = oldend;
  return 0;
}

static int nchar_cb_text_bytes(const char *str,
                               const char *end,
                               void *vdata) {
  struct nchar_data *data = vdata;
  *data->resptr += (end - str);
  return 0;
}

static int nchar_cb_text_width(const char *str,
                               const char *end,
                               void *vdata) {
  struct nchar_data *data = vdata;
  char *end2 = (char*) end;
  char oldend = *end2;
  int len = 0, width;
  struct grapheme_iterator iter;

  *end2 = '\0';
  clic_utf8_graphscan_make(&iter, (const uint8_t*) str, /* width = */ 1);
  while (iter.nxt_prop != -1) {
    clic_utf8_graphscan_next(&iter, NULL, &width);
    len += width;
  }
  *data->resptr += len;

  *end2 = oldend;
  return 0;
}

static int nchar_cb_text_codepoints(const char *str,
                                    const char *end,
                                    void *vdata) {
  struct nchar_data *data = vdata;

  while (str < end) {
    int len = UTF8LITE_UTF8_TOTAL_LEN(*str);
    str += len;
    *data->resptr += 1;
  }

  return 0;
}

static int nchar_cb_end(SEXP rstr,
                          const char *str,
                          void *vdata) {
  struct nchar_data *data = vdata;
  data->done ++;
  return 0;
}

  static clic__text_callback_t nchar_text_cbs[] = {
    nchar_cb_text_graphemes,
    nchar_cb_text_bytes,
    nchar_cb_text_width,
    nchar_cb_text_codepoints
  };

  /* TODO: this would benefit from a non-iterator implementation, that
     would be much faster. */

SEXP clic_ansi_nchar(SEXP sx, SEXP type) {
  int ctype = INTEGER(type)[0] - 1;
  struct nchar_data data;
  data.done = 0;
  SEXP result = PROTECT(allocVector(INTSXP, XLENGTH(sx)));
  data.result = INTEGER(result);

  clic__ansi_iterator(
    sx,
    nchar_cb_start,
    /* sgr   = */ NULL,
    /* csi   = */ NULL,
    /* link  = */ NULL,
    nchar_text_cbs[ctype],
    nchar_cb_end,
    &data
  );

  UNPROTECT(1);
  return result;
}
