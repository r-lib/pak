
#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif

#ifdef _WIN32

#include <winsock2.h>
#include "win/processx-stdio.h"
#include <windows.h>
#include <io.h>
#include <fcntl.h>

#include <R_ext/Rdynload.h>
#include <Rinternals.h>
#include "errors.h"

int processx__stdio_verify(BYTE* buffer, WORD size) {
  unsigned int count;

  /* Check the buffer pointer. */
  if (buffer == NULL)
    return 0;

  /* Verify that the buffer is at least big enough to hold the count. */
  if (size < CHILD_STDIO_SIZE(0))
    return 0;

  /* Verify if the count is within range. */
  count = CHILD_STDIO_COUNT(buffer);
  if (count > 256)
    return 0;

  /* Verify that the buffer size is big enough to hold info for N FDs. */
  if (size < CHILD_STDIO_SIZE(count))
    return 0;

  return 1;
}

void processx__stdio_noinherit(BYTE* buffer) {
  int i, count;

  count = CHILD_STDIO_COUNT(buffer);
  for (i = 0; i < count; i++) {
    HANDLE handle = CHILD_STDIO_HANDLE(buffer, i);
    if (handle != INVALID_HANDLE_VALUE) {
      SetHandleInformation(handle, HANDLE_FLAG_INHERIT, 0);
    }
  }
}

/*
 * Clear the HANDLE_FLAG_INHERIT flag from all HANDLEs that were inherited
 * the parent process. Don't check for errors - the stdio handles may not be
 * valid, or may be closed already. There is no guarantee that this function
 * does a perfect job.
 */

SEXP processx_disable_inheritance(void) {
  HANDLE handle;
  STARTUPINFOW si;

  /* Make the windows stdio handles non-inheritable. */
  handle = GetStdHandle(STD_INPUT_HANDLE);
  if (handle != NULL && handle != INVALID_HANDLE_VALUE) {
    SetHandleInformation(handle, HANDLE_FLAG_INHERIT, 0);
  }

  handle = GetStdHandle(STD_OUTPUT_HANDLE);
  if (handle != NULL && handle != INVALID_HANDLE_VALUE) {
    SetHandleInformation(handle, HANDLE_FLAG_INHERIT, 0);
  }

  handle = GetStdHandle(STD_ERROR_HANDLE);
  if (handle != NULL && handle != INVALID_HANDLE_VALUE) {
    SetHandleInformation(handle, HANDLE_FLAG_INHERIT, 0);
  }

  /* Make inherited CRT FDs non-inheritable. */
  GetStartupInfoW(&si);
  if (processx__stdio_verify(si.lpReserved2, si.cbReserved2)) {
    processx__stdio_noinherit(si.lpReserved2);
  }

  return R_NilValue;
}

SEXP processx_write(SEXP fd, SEXP data) {
  int cfd = INTEGER(fd)[0];
  HANDLE h = (HANDLE) _get_osfhandle(cfd);
  DWORD written;

  BOOL ret = WriteFile(h, RAW(data), LENGTH(data), &written, NULL);
  if (!ret) R_THROW_SYSTEM_ERROR("Cannot write to fd");

  return ScalarInteger(written);
}

#else

#include <fcntl.h>
#include <errno.h>
#include <unistd.h>
#include <stdlib.h>
#include <signal.h>
#include <string.h>

#include <R_ext/Rdynload.h>
#include <Rinternals.h>
#include "errors.h"

static int processx__cloexec_fcntl(int fd, int set) {
  int flags;
  int r;

  do { r = fcntl(fd, F_GETFD); } while (r == -1 && errno == EINTR);
  if (r == -1) { return -errno; }

  /* Bail out now if already set/clear. */
  if (!!(r & FD_CLOEXEC) == !!set) { return 0; }

  if (set) { flags = r | FD_CLOEXEC; } else { flags = r & ~FD_CLOEXEC; }

  do { r = fcntl(fd, F_SETFD, flags); } while (r == -1 && errno == EINTR);
  if (r) { return -errno; }

  return 0;
}

SEXP processx_disable_inheritance(void) {
  int fd;

  /* Set the CLOEXEC flag on all open descriptors. Unconditionally try the
   * first 16 file descriptors. After that, bail out after the first error.
   * We skip the standard streams, because R and `system()` is not prepared
   * to not inheriting stdin and eg. an R subprocess does not even start in
   * system(). See https://github.com/r-lib/callr/issues/236. */

  int firstfd = 3;
  if (getenv("PROCESSX_CLOEXEC_STDIO")) firstfd = 0;
  for (fd = firstfd; ; fd++) {
    if (processx__cloexec_fcntl(fd, 1) && fd > 15) break;
  }

  return R_NilValue;
}

SEXP processx_write(SEXP fd, SEXP data) {
  int cfd = INTEGER(fd)[0];

  struct sigaction old_handler, new_handler;
  memset(&new_handler, 0, sizeof(new_handler));
  sigemptyset(&new_handler.sa_mask);
  new_handler.sa_handler = SIG_IGN;
  sigaction(SIGPIPE, &new_handler, &old_handler );

  ssize_t ret = write(cfd, RAW(data), LENGTH(data));
  int err = errno;

  sigaction(SIGPIPE, &old_handler, NULL );

  if (ret == -1) {
    if (err == EAGAIN || err == EWOULDBLOCK) {
      ret = 0;
    } else {
      R_THROW_SYSTEM_ERROR("Cannot write to fd");
    }
  }

  return ScalarInteger(ret);
}

#endif

static SEXP processx_set_std(int which, int fd, int drop) {
  int orig = -1;
  int ret;
  const char *what[] = { "stdin", "stdout", "stderr" };

  if (!drop) {
#ifdef _WIN32
    orig = _dup(which);
#else
    orig = dup(which);
#endif
    if (orig == -1) {
      R_THROW_SYSTEM_ERROR("Cannot reroute %s", what[which]);
    }
  }

#ifdef _WIN32
  ret = _dup2(fd, which);
#else
  ret = dup2(fd, which);
#endif
  if (ret == -1) {
    R_THROW_SYSTEM_ERROR("Cannot reroute %s", what[which]);
  }

  close(fd);

  if (!drop) {
    return ScalarInteger(orig);
  } else {
    return R_NilValue;
  }
}

SEXP processx_set_stdout(SEXP fd, SEXP drop) {
  return processx_set_std(1, INTEGER(fd)[0], LOGICAL(drop)[0]);
}

SEXP processx_set_stderr(SEXP fd, SEXP drop) {
  return processx_set_std(2, INTEGER(fd)[0], LOGICAL(drop)[0]);
}

SEXP processx_set_stdout_to_file(SEXP file) {
  const char *c_file = CHAR(STRING_ELT(file, 0));
#ifdef _WIN32
  int fd = open(c_file, _O_WRONLY | _O_CREAT | _O_TRUNC, 0644);
#else
  int fd = open(c_file, O_WRONLY | O_CREAT | O_TRUNC, 0644);
#endif
  if (fd == -1) {
    R_THROW_SYSTEM_ERROR("Cannot open new stdout file `%s`", c_file);
  }
  SEXP ret = processx_set_std(1, fd, 0);
  return ret;
}

SEXP processx_set_stderr_to_file(SEXP file) {
  const char *c_file = CHAR(STRING_ELT(file, 0));
#ifdef _WIN32
  int fd = open(c_file, _O_WRONLY | _O_CREAT | _O_TRUNC, 0644);
#else
  int fd = open(c_file, O_WRONLY | O_CREAT | O_TRUNC, 0644);
#endif
  if (fd == -1) {
    R_THROW_SYSTEM_ERROR("Cannot open new stderr file `%s`", c_file);
  }
  SEXP ret = processx_set_std(2, fd, 0);
  return ret;
}

SEXP processx_base64_encode(SEXP array);
SEXP processx_base64_decode(SEXP array);


#ifndef _WIN32

#include <string.h>
#include <signal.h>

void term_handler(int n) {
  // Need the cast and the +1 to ignore compiler warning about unused
  // return value.
  (void) (system("rm -rf \"$R_SESSION_TMPDIR\"") + 1);
  // Continue signal
  raise(SIGTERM);
}

void install_term_handler(void) {
  if (! getenv("PROCESSX_R_SIGTERM_CLEANUP")) {
    return;
  }

  struct sigaction sig = {{ 0 }};
  sig.sa_handler = term_handler;
  sig.sa_flags = SA_RESETHAND;
  sigaction(SIGTERM, &sig, NULL);
}

#endif // not _WIN32


static const R_CallMethodDef callMethods[]  = {
  { "processx_base64_encode", (DL_FUNC) &processx_base64_encode, 1 },
  { "processx_base64_decode", (DL_FUNC) &processx_base64_decode, 1 },
  { "processx_disable_inheritance", (DL_FUNC) &processx_disable_inheritance, 0 },
  { "processx_write", (DL_FUNC) &processx_write, 2 },
  { "processx_set_stdout", (DL_FUNC) &processx_set_stdout, 2 },
  { "processx_set_stderr", (DL_FUNC) &processx_set_stderr, 2 },
  { "processx_set_stdout_to_file", (DL_FUNC) &processx_set_stdout_to_file, 1 },
  { "processx_set_stderr_to_file", (DL_FUNC) &processx_set_stderr_to_file, 1 },
  { NULL, NULL, 0 }
};

void R_init_client(DllInfo *dll) {
  R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);

#ifndef _WIN32
  install_term_handler();
#endif
}
