
#include "../processx.h"

#include <termios.h>
#include <sys/ioctl.h>

char *processx__tmp_string(SEXP str, int i) {
  const char *ptr = CHAR(STRING_ELT(str, i));
  char *cstr = R_alloc(1, (int) strlen(ptr) + 1);
  strcpy(cstr, ptr);
  return cstr;
}

char **processx__tmp_character(SEXP chr) {
  size_t i, n = LENGTH(chr);
  char **cchr = (void*) R_alloc(n + 1, sizeof(char*));
  for (i = 0; i < n; i++) {
    cchr[i] = processx__tmp_string(chr, (int) i);
  }
  cchr[n] = 0;
  return cchr;
}

int processx__nonblock_fcntl(int fd, int set) {
  int flags;
  int r;

  do { r = fcntl(fd, F_GETFL); } while (r == -1 && errno == EINTR);
  if (r == -1) { return -errno; }

  /* Bail out now if already set/clear. */
  if (!!(r & O_NONBLOCK) == !!set) { return 0; }

  if (set) { flags = r | O_NONBLOCK; } else { flags = r & ~O_NONBLOCK; }

  do { r = fcntl(fd, F_SETFL, flags); } while (r == -1 && errno == EINTR);
  if (r) { return -errno; }

  return 0;
}

int processx__cloexec_fcntl(int fd, int set) {
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

SEXP processx_disable_crash_dialog(void) {
  struct sigaction action;
  memset(&action, 0, sizeof(action));
  action.sa_handler = SIG_DFL;
  sigaction(SIGSEGV, &action, /* oldact= */ NULL);
  sigaction(SIGILL,  &action, /* oldact= */ NULL);
#ifdef SIGBUS
  sigaction(SIGBUS,  &action, /* oldact= */ NULL);
#endif
  return R_NilValue;
}

SEXP processx__echo_on(void) {
  struct termios tp;

  if (tcgetattr(STDOUT_FILENO, &tp) == -1) {
    R_THROW_ERROR("Cannot turn terminal echo on");
  }

  tp.c_lflag |= ECHO;

  if (tcsetattr(STDOUT_FILENO, TCSAFLUSH, &tp) == -1) {
    R_THROW_ERROR("Cannot turn terminal echo on");
  }

  return R_NilValue;
}

SEXP processx__echo_off(void) {
  struct termios tp;

  if (tcgetattr(STDOUT_FILENO, &tp) == -1) {
    R_THROW_ERROR("Cannot turn terminal echo off");
  }

  tp.c_lflag &= ~ECHO;

  if (tcsetattr(STDOUT_FILENO, TCSAFLUSH, &tp) == -1) {
    R_THROW_ERROR("Cannot turn terminal echo off");
  }

  return R_NilValue;
}
