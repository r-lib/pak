
#include "errors.h"

#include <Rinternals.h>

#include <string.h>

#define ERRORBUF_SIZE 4096
static char errorbuf[ERRORBUF_SIZE];
static char msgbuf[ERRORBUF_SIZE];

SEXP r_throw_error(const char *func, const char *filename, int line,
                   const char *msg, ...) {
  va_list args;
  errorbuf[0] = '\0';
  va_start(args, msg);
  vsnprintf(errorbuf, ERRORBUF_SIZE, msg, args);
  va_end (args);
  error("%s @%s:%d (%s)", errorbuf, filename, line, func);
  return R_NilValue;
}

SEXP r_format_error(const char *func, const char *filename, int line,
                    const char *msg, ...) {
  va_list args;
  errorbuf[0] = '\0';
  va_start(args, msg);
  vsnprintf(errorbuf, ERRORBUF_SIZE, msg, args);
  va_end (args);

  // Recent GCC warns if return value is not used
  int ret = snprintf(
    msgbuf,
    ERRORBUF_SIZE,
    "%s @%s:%d (%s)",
    errorbuf,
    filename,
    line,
    func
  );
  if (ret < 0) {
    msgbuf[ERRORBUF_SIZE - 1] = '\0';
    msgbuf[ERRORBUF_SIZE - 2] = '.';
    msgbuf[ERRORBUF_SIZE - 3] = '.';
    msgbuf[ERRORBUF_SIZE - 4] = '.';
  }

  return Rf_ScalarString(Rf_mkCharCE(msgbuf, CE_NATIVE));
}

#ifdef _WIN32

SEXP r_throw_system_error(const char *func, const char *filename, int line,
                          DWORD errorcode, const char *sysmsg,
                          const char *msg, ...) {

  va_list args;
  LPVOID lpMsgBuf;
  char *realsysmsg = sysmsg ? (char*) sysmsg : NULL;
  char *failmsg = "Formatting the system message failed :(";

  if (errorcode == -1) errorcode = GetLastError();

  /* TODO: FormatMessageW is better? */
  if (!realsysmsg) {
    DWORD ret = FormatMessage(
      FORMAT_MESSAGE_ALLOCATE_BUFFER |
      FORMAT_MESSAGE_FROM_SYSTEM |
      FORMAT_MESSAGE_IGNORE_INSERTS,
      NULL,
      errorcode,
      MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
      (LPTSTR) &lpMsgBuf,
      0, NULL);

    if (ret == 0) {
      realsysmsg = failmsg;
    } else {
      realsysmsg = R_alloc(1, strlen(lpMsgBuf) + 1);
      strcpy(realsysmsg, lpMsgBuf);
      LocalFree(lpMsgBuf);
    }
  }

  errorbuf[0] = '\0';
  va_start(args, msg);
  vsnprintf(errorbuf, ERRORBUF_SIZE, msg, args);
  va_end(args);
  error("%s (system error %ld, %s) @%s:%d (%s)", errorbuf, errorcode,
        realsysmsg, filename, line, func);
  return R_NilValue;
}

SEXP r_format_system_error(const char *func, const char *filename, int line,
                           DWORD errorcode, const char *sysmsg,
                           const char *msg, ...) {

  va_list args;
  LPVOID lpMsgBuf;
  char *realsysmsg = sysmsg ? (char*) sysmsg : NULL;
  char *failmsg = "Formatting the system message failed :(";

  if (errorcode == -1) errorcode = GetLastError();

  /* TODO: FormatMessageW is better? */
  if (!realsysmsg) {
    DWORD ret = FormatMessage(
      FORMAT_MESSAGE_ALLOCATE_BUFFER |
      FORMAT_MESSAGE_FROM_SYSTEM |
      FORMAT_MESSAGE_IGNORE_INSERTS,
      NULL,
      errorcode,
      MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
      (LPTSTR) &lpMsgBuf,
      0, NULL);

    if (ret == 0) {
      realsysmsg = failmsg;
    } else {
      realsysmsg = R_alloc(1, strlen(lpMsgBuf) + 1);
      strcpy(realsysmsg, lpMsgBuf);
      LocalFree(lpMsgBuf);
    }
  }

  errorbuf[0] = '\0';
  va_start(args, msg);
  vsnprintf(errorbuf, ERRORBUF_SIZE, msg, args);
  va_end(args);

  snprintf(
    msgbuf,
    ERRORBUF_SIZE,
    "%s (system error %ld, %s) @%s:%d (%s)",
    errorbuf,
    errorcode,
    realsysmsg,
    filename,
    line,
    func
  );

  return Rf_ScalarString(Rf_mkCharCE(msgbuf, CE_NATIVE));
}

#endif

#ifdef _WIN32
SEXP r_throw_posix_error(
#else
SEXP r_throw_system_error(
#endif
                          const char *func, const char *filename, int line,
                          int errorcode, const char *sysmsg,
                          const char *msg, ...) {
  va_list args;
  if (!sysmsg) sysmsg = strerror(errorcode);
  errorbuf[0] = '\0';
  va_start(args, msg);
  vsnprintf(errorbuf, ERRORBUF_SIZE, msg, args);
  va_end(args);
  error("%s (system error %d, %s) @%s:%d (%s)", errorbuf, errorcode, sysmsg,
        filename, line, func);
  return R_NilValue;
}

#ifdef _WIN32
SEXP r_format_posix_error(
#else
SEXP r_format_system_error(
#endif
                           const char *func, const char *filename, int line,
                           int errorcode, const char *sysmsg,
                           const char *msg, ...) {
  va_list args;
  if (!sysmsg) sysmsg = strerror(errorcode);
  errorbuf[0] = '\0';
  va_start(args, msg);
  vsnprintf(errorbuf, ERRORBUF_SIZE, msg, args);
  va_end(args);

  // Recent GCC warns if return value is not used
  int ret = snprintf(
    msgbuf,
    ERRORBUF_SIZE,
    "%s (system error %d, %s) @%s:%d (%s)",
    errorbuf,
    errorcode,
    sysmsg,
    filename,
    line,
    func
  );
  if (ret < 0) {
    msgbuf[ERRORBUF_SIZE - 1] = '\0';
    msgbuf[ERRORBUF_SIZE - 2] = '.';
    msgbuf[ERRORBUF_SIZE - 3] = '.';
    msgbuf[ERRORBUF_SIZE - 4] = '.';
  }

  return Rf_ScalarString(Rf_mkCharCE(msgbuf, CE_NATIVE));
}
