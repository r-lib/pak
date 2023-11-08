
#include "cli.h"
#include "errors.h"
#include "cleancall.h"

#ifdef WIN32
#include <windows.h>
#else
#include <termios.h>
#include <sys/ioctl.h>
#include <stdio.h>
#include <unistd.h>
#endif

SEXP clic_tty_size(void) {
  SEXP result = PROTECT(Rf_allocVector(INTSXP, 2));

#ifdef WIN32
  CONSOLE_SCREEN_BUFFER_INFO info;
  BOOL ok = GetConsoleScreenBufferInfo(
    GetStdHandle(STD_OUTPUT_HANDLE),
    &info
  );
  if (!ok) R_THROW_SYSTEM_ERROR("Cannot determine terminal size");
  INTEGER(result)[0] = info.srWindow.Right  - info.srWindow.Left + 1;
  INTEGER(result)[1] = info.srWindow.Bottom - info.srWindow.Top + 1;

#elif defined(TIOCGWINSZ)
  struct winsize w;
  int err = ioctl(STDOUT_FILENO, TIOCGWINSZ, &w);
  if (err == -1) R_THROW_SYSTEM_ERROR("Cannot determine terminal size");
  INTEGER(result)[0] = w.ws_col;
  INTEGER(result)[1] = w.ws_row;

#elif defined(TIOCGSIZE)
  struct ttysize ts;
  int err = ioctl(STDOUT_FILENO, TIOCGSIZE, &ts);
  if (err == -1) R_THROW_SYSTEM_ERROR("Cannot determine terminal size");
  INTEGER(result)[0] = ts.ts_cols;
  INTEGER(result)[1] = ts.ts_rows;

#else
  R_THROW_ERROR("Don't know how to determine terminal size");
#endif

  UNPROTECT(1);
  return result;
}
