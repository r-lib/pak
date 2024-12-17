
#ifndef CLI_H
#define CLI_H

#include <stdint.h>

#include <R.h>
#include <Rinternals.h>
#include <Rversion.h>

SEXP clic_diff_chr(SEXP a, SEXP b, SEXP max);

SEXP clic_getppid(void);
SEXP clic_md5(SEXP strs);
SEXP clic_md5_raw(SEXP r);
SEXP clic_md5_file(SEXP paths);
SEXP clic_sha256(SEXP strs);
SEXP clic_sha256_raw(SEXP r);
SEXP clic_sha256_file(SEXP paths);
SEXP clic_sha1(SEXP strs);
SEXP clic_sha1_raw(SEXP r);
SEXP clic_sha1_file(SEXP paths);
SEXP clic_xxhash(SEXP strs);
SEXP clic_xxhash_raw(SEXP r);
SEXP clic_xxhash_file(SEXP paths);
SEXP clic_xxhash64(SEXP strs);
SEXP clic_xxhash64_raw(SEXP r);
SEXP clic_xxhash64_file(SEXP paths);
SEXP clic_tty_size(void);
SEXP clic_ansi_simplify(SEXP x, SEXP keep_csi);
SEXP clic_ansi_substr(SEXP x, SEXP start, SEXP stop);
SEXP clic_ansi_html(SEXP x, SEXP keep_csi);
SEXP clic_ansi_has_any(SEXP x, SEXP sgr, SEXP csi, SEXP link);
SEXP clic_ansi_strip(SEXP x, SEXP sgr, SEXP csi, SEXP link);
SEXP clic_ansi_nchar(SEXP x, SEXP type);

SEXP clic_utf8_nchar_graphemes(SEXP x);
SEXP clic_utf8_display_width(SEXP x);
SEXP clic_utf8_substr(SEXP sx, SEXP start, SEXP stop);
SEXP clic_utf8_graphemes(SEXP sx);

typedef volatile int vint;

extern volatile int* cli_timer_flag;
extern volatile int cli__reset;
void cli_progress_add(SEXP bar, double inc);
SEXP cli_progress_bar(vint **ptr, double total, SEXP config);
void cli_progress_done(SEXP bar);
void cli_progress_init_timer(vint **ptr);
int cli_progress_num(void);
void cli_progress_set(SEXP bar, double set);
void cli_progress_set_clear(SEXP bar, int);
void cli_progress_set_format(SEXP bar, const char *name);
void cli_progress_set_name(SEXP bar, const char *name);
void cli_progress_set_status(SEXP bar, const char *name);
void cli_progress_set_type(SEXP bar, const char *name);
void cli_progress_sleep(int s, long ns);
void cli_progress_update(SEXP bar, double set, double inc, int force);

SEXP cli__progress_update(SEXP bar);
SEXP clic_progress_along(SEXP seq, SEXP bar);
extern SEXP cli_pkgenv;

#if R_VERSION >= R_Version(3, 5, 0)
void cli_init_altrep(DllInfo *dll);
#endif

double clic__get_time(void);
SEXP clic__find_var(SEXP rho, SEXP symbol);

SEXP clic_start_thread(SEXP pkgenv, SEXP tick, SEXP speed);
SEXP clic_stop_thread(void);
SEXP clic_tick_reset(void);
SEXP clic_get_time(void);
SEXP clic_tick_set(SEXP ticktime, SEXP speedtime);
SEXP clic_tick_pause(SEXP state);
SEXP clic_tick_resume(SEXP state);
SEXP clic_make_timer(void);
SEXP clic_update_due(void);

/** Indicates whether a given unsigned integer is a valid ASCII codepoint */
#define UTF8LITE_IS_ASCII(x) \
	((x) <= 0x7F)

/** Given the first byte in a valid UTF-8 byte sequence, determine the number of
 *  total bytes */

#define UTF8LITE_UTF8_TOTAL_LEN(x) \
	(  ((x) & 0x80) == 0x00 ? 1 \
	 : ((x) & 0xE0) == 0xC0 ? 2 \
	 : ((x) & 0xF0) == 0xE0 ? 3 : 4)

/** Last valid unicode codepoint */
#define UTF8LITE_CODE_MAX 0x10FFFF

/** Indicates whether a 16-bit code unit is a UTF-16 high surrogate.
 *  High surrogates are in the range 0xD800 `(1101 1000 0000 0000)`
 *  to 0xDBFF `(1101 1011 1111 1111)`. */
#define UTF8LITE_IS_UTF16_HIGH(x) (((x) & 0xFC00) == 0xD800)

/** Indicates whether a 16-bit code unit is a UTF-16 low surrogate.
 *  Low surrogates are in the range 0xDC00 `(1101 1100 0000 0000)`
 *  to 0xDFFF `(1101 1111 1111 1111)`. */
#define UTF8LITE_IS_UTF16_LOW(x) (((x) & 0xFC00) == 0xDC00)

/** Indicates whether a given unsigned integer is a valid unicode codepoint */
#define UTF8LITE_IS_UNICODE(x) \
	(((x) <= UTF8LITE_CODE_MAX) \
	 && !UTF8LITE_IS_UTF16_HIGH(x) \
	 && !UTF8LITE_IS_UTF16_LOW(x))

SEXP clic_get_embedded_utf8(void);
SEXP clic_set_embedded_utf8(SEXP value);
int clic__utf8_display_width_char(const uint8_t **x);

struct grapheme_iterator {
  const uint8_t *nxt_ptr;
  int32_t nxt_code;
  int nxt_prop;
  int nxt_cw;
  const uint8_t *cnd;
  int cnd_width;
  char cnd_width_done;          /* -1: do not measure width */
};

void clic_utf8_graphscan_make(struct grapheme_iterator *iter,
                              const uint8_t *txt,
                              int width);

void clic_utf8_graphscan_next(struct grapheme_iterator *iter,
                              uint8_t **ptr,
                              int *width);

SEXP glue_(SEXP x, SEXP f, SEXP open_arg, SEXP close_arg, SEXP cli_arg);
SEXP trim_(SEXP x);

SEXP clic_vt_output(SEXP bytes, SEXP width, SEXP height);

#endif
