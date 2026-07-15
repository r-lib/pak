
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <Rinternals.h>

#ifndef _WIN32
#include <pthread.h>
#endif

#include "zip.h"

static ZIP_THREAD_LOCAL char tl_error_msg[1024];

static void dll_error_handler(const char *reason, const char *file,
                               int line, int zip_errno, int eno) {
  (void) file; (void) line; (void) zip_errno; (void) eno;
  snprintf(tl_error_msg, sizeof(tl_error_msg), "%s", reason);
  /* return normally; ZIP_ERROR macro executes `return 1` next */
}

static int hex_nibble(unsigned int c) {
  if (c >= '0' && c <= '9') return (int)(c - '0');
  if (c >= 'a' && c <= 'f') return (int)(c - 'a') + 10;
  if (c >= 'A' && c <= 'F') return (int)(c - 'A') + 10;
  return -1;
}

static int decode_hex_password(const char *hex, unsigned char **out) {
  size_t hlen = strlen(hex);
  if (hlen % 2 != 0) return -1;
  size_t outlen = hlen / 2;
  *out = (unsigned char *) malloc(outlen + 1);
  if (!*out) return -1;
  for (size_t i = 0; i < outlen; i++) {
    int hi = hex_nibble((unsigned int) hex[2 * i]);
    int lo = hex_nibble((unsigned int) hex[2 * i + 1]);
    if (hi < 0 || lo < 0) { free(*out); return -1; }
    (*out)[i] = (unsigned char)((hi << 4) | lo);
  }
  return (int) outlen;
}

/*
 * Unzip zipfile into exdir. hex_password may be NULL or empty string.
 * All paths are UTF-8. Returns 0 on success, non-zero on failure.
 * On failure, error_buf is filled with a null-terminated message.
 */
#if defined(__GNUC__) || defined(__clang__)
__attribute__((visibility("default")))
#endif
int do_cmdunzip(const char *zipfile, const char *exdir,
                const char *hex_password,
                char *error_buf, size_t error_buf_len) {
  zip_set_error_handler(dll_error_handler);
  tl_error_msg[0] = '\0';

  unsigned char *password = NULL;
  int password_len = 0;
  if (hex_password && hex_password[0]) {
    password_len = decode_hex_password(hex_password, &password);
    if (password_len < 0) {
      snprintf(error_buf, error_buf_len, "Invalid hex password");
      return 1;
    }
  }

  int ret = zip_unzip(zipfile, /* cfiles= */ NULL, /* num_files= */ 0,
                      /* coverwrite= */ 1, /* cjunkpaths= */ 0,
                      /* exdir= */ exdir,
                      /* decode_fn= */ NULL, /* decode_data= */ NULL,
                      /* entry_fn= */ NULL, /* entry_data= */ NULL,
                      password, (size_t) password_len);
  free(password);

  if (ret != 0 && error_buf && error_buf_len > 0) {
    snprintf(error_buf, error_buf_len, "%s",
             tl_error_msg[0] ? tl_error_msg : "unknown error");
  }
  return ret;
}

/* ---- threaded batch unzip ------------------------------------------- */

typedef struct {
  const char **zipfiles;
  const char **exdirs;
  const char **passwords;  /* NULL when unused */
  int n;
  int *results;
  char **error_bufs;
#ifdef _WIN32
  CRITICAL_SECTION cs;
#else
  pthread_mutex_t mutex;
#endif
  int next_task;
} unzip_queue_t;

#ifdef _WIN32
static DWORD WINAPI unzip_worker(LPVOID arg)
#else
static void *unzip_worker(void *arg)
#endif
{
  unzip_queue_t *q = (unzip_queue_t *) arg;
  for (;;) {
#ifdef _WIN32
    EnterCriticalSection(&q->cs);
#else
    pthread_mutex_lock(&q->mutex);
#endif
    int i = q->next_task++;
#ifdef _WIN32
    LeaveCriticalSection(&q->cs);
#else
    pthread_mutex_unlock(&q->mutex);
#endif
    if (i >= q->n) break;
    const char *pw = (q->passwords && q->passwords[i] && q->passwords[i][0])
                   ? q->passwords[i] : NULL;
    q->results[i] = do_cmdunzip(q->zipfiles[i], q->exdirs[i],
                                pw, q->error_bufs[i], 1024);
  }
#ifdef _WIN32
  return 0;
#else
  return NULL;
#endif
}

SEXP R_threaded_unzip(SEXP zipfiles, SEXP exdirs, SEXP num_threads,
                      SEXP passwords) {
  int n = LENGTH(zipfiles);
  int nthreads = asInteger(num_threads);
  if (nthreads <= 0) nthreads = 1;
  if (nthreads > n) nthreads = n;

  /* Use malloc, not R_alloc: worker threads access these buffers. */
  int   *res  = (int *)   malloc(n * sizeof(int));
  char **errs = (char **) malloc(n * sizeof(char *));
  for (int i = 0; i < n; i++) {
    errs[i] = (char *) malloc(1024);
    errs[i][0] = '\0';
    res[i] = 0;
  }

  unzip_queue_t q;
  memset(&q, 0, sizeof(q));
  q.n          = n;
  q.zipfiles   = (const char **) malloc(n * sizeof(char *));
  q.exdirs     = (const char **) malloc(n * sizeof(char *));
  q.passwords  = isNull(passwords) ? NULL
                                   : (const char **) malloc(n * sizeof(char *));
  q.results    = res;
  q.error_bufs = errs;

  /* Capture CHAR pointers before threads start. R GC cannot run while we
     are in C (workers never call the R API), so these remain stable. */
  for (int i = 0; i < n; i++) {
    q.zipfiles[i] = CHAR(STRING_ELT(zipfiles, i));
    q.exdirs[i]   = CHAR(STRING_ELT(exdirs,   i));
    if (q.passwords) q.passwords[i] = CHAR(STRING_ELT(passwords, i));
  }

#ifdef _WIN32
  InitializeCriticalSection(&q.cs);
  HANDLE *threads = (HANDLE *) malloc(nthreads * sizeof(HANDLE));
  for (int t = 0; t < nthreads; t++)
    threads[t] = CreateThread(NULL, 0, unzip_worker, &q, 0, NULL);
  WaitForMultipleObjects((DWORD) nthreads, threads, TRUE, INFINITE);
  for (int t = 0; t < nthreads; t++) CloseHandle(threads[t]);
  free(threads);
  DeleteCriticalSection(&q.cs);
#else
  pthread_mutex_init(&q.mutex, NULL);
  pthread_t *threads = (pthread_t *) malloc(nthreads * sizeof(pthread_t));
  for (int t = 0; t < nthreads; t++)
    pthread_create(&threads[t], NULL, unzip_worker, &q);
  for (int t = 0; t < nthreads; t++)
    pthread_join(threads[t], NULL);
  free(threads);
  pthread_mutex_destroy(&q.mutex);
#endif

  free(q.zipfiles);
  free(q.exdirs);
  if (q.passwords) free(q.passwords);

  /* Build R result — back on the main thread, R API safe again. */
  SEXP r_res  = PROTECT(allocVector(INTSXP, n));
  SEXP r_errs = PROTECT(allocVector(STRSXP, n));
  for (int i = 0; i < n; i++) {
    INTEGER(r_res)[i] = res[i];
    SET_STRING_ELT(r_errs, i, mkChar(errs[i]));
    free(errs[i]);
  }
  free(res);
  free(errs);

  SEXP ret = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(ret, 0, r_res);
  SET_VECTOR_ELT(ret, 1, r_errs);
  UNPROTECT(3);
  return ret;
}
