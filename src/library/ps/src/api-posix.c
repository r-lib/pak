
#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif

#include <errno.h>
#include <sys/ioctl.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/statvfs.h>
#include <sys/resource.h>

#include "ps-internal.h"

SEXP psll_send_signal(SEXP p, SEXP sig) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  int csig = INTEGER(sig)[0];
  int ret;
  SEXP running;

  if (!handle) error("Process pointer cleaned up already");

  if (handle->pid == 0) {
    error("preventing sending signal to process with PID 0 as it "
	  "would affect every process in the process group of the "
	  "calling process (Sys.getpid()) instead of PID 0");
  }

  running = psll_is_running(p);
  if (!LOGICAL(running)[0]) {
    ps__no_such_process(handle->pid, 0);
    ps__throw_error();
  }

  /* TODO: this is still a race here. We would need to SIGSTOP the
     process first, then check the timestamp, and then send the signal
     (if not SIGSTOP), or send a SIGCONT. */

  ret = kill(handle->pid, csig);
  if (ret == -1) {
    if (errno == ESRCH) {
      ps__no_such_process(handle->pid, 0);
    } else if (errno == EPERM || errno == EACCES) {
      ps__access_denied("");
    } else {
      ps__set_error_from_errno();
    }
    ps__throw_error();
  }

  return R_NilValue;
}


SEXP psll_suspend(SEXP p) {
  SEXP res, s;
  PROTECT(s = ScalarInteger(SIGSTOP));
  PROTECT(res = psll_send_signal(p, s));
  UNPROTECT(2);
  return res;
}


SEXP psll_resume(SEXP p) {
  SEXP res, s;
  PROTECT(s = ScalarInteger(SIGCONT));
  PROTECT(res = psll_send_signal(p, s));
  UNPROTECT(2);
  return res;
}


SEXP psll_terminate(SEXP p) {
  SEXP res, s;
  PROTECT(s = ScalarInteger(SIGTERM));
  PROTECT(res = psll_send_signal(p, s));
  UNPROTECT(2);
  return res;
}


SEXP psll_kill(SEXP p) {
  SEXP res, s;
  PROTECT(s = ScalarInteger(SIGKILL));
  PROTECT(res = psll_send_signal(p, s));
  UNPROTECT(2);
  return res;
}

SEXP psll_interrupt(SEXP p, SEXP ctrlc, SEXP interrupt_path) {
  SEXP res, s;
  PROTECT(s = ScalarInteger(SIGINT));
  PROTECT(res = psll_send_signal(p, s));
  UNPROTECT(2);
  return res;
}

SEXP ps__tty_size(void) {
  struct winsize w;
  int err = ioctl(STDOUT_FILENO, TIOCGWINSZ, &w);
  if (err == -1) {
    ps__set_error_from_errno();
    ps__throw_error();
  }

  SEXP result = Rf_allocVector(INTSXP, 2);
  INTEGER(result)[0] = w.ws_col;
  INTEGER(result)[1] = w.ws_row;

  return result;
}

SEXP ps__disk_usage(SEXP paths) {
  struct statvfs stat;
  int i, n = Rf_length(paths);
  SEXP result = PROTECT(allocVector(VECSXP, n));

  for (i = 0; i < n; i++) {
    const char *cpath = CHAR(STRING_ELT(paths, i));
    int ret = statvfs(cpath, &stat);
    if (ret == -1) {
      ps__set_error_from_errno();
      ps__throw_error();
    }
    SET_VECTOR_ELT(
      result, i,
      ps__build_list("idddddd", (int) stat.f_frsize, (double) stat.f_files,
                     (double) stat.f_favail, (double) stat.f_ffree,
                     (double) stat.f_blocks, (double) stat.f_bavail,
                     (double) stat.f_bfree));
  }

  UNPROTECT(1);
  return result;
}

SEXP psll_get_nice(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  pid_t pid;
  int priority;
  errno = 0;

  if (!handle) error("Process pointer cleaned up already");

  pid = handle->pid;

#ifdef PS__MACOS
  priority = getpriority(PRIO_PROCESS, (id_t)pid);
#else
  priority = getpriority(PRIO_PROCESS, pid);
#endif

  if (errno != 0) {
    ps__check_for_zombie(handle, 1);
    ps__set_error_from_errno();
    ps__throw_error();
  } else {
    ps__check_for_zombie(handle, 0);
  }

  return ScalarInteger(priority);
}

SEXP psll_set_nice(SEXP p, SEXP value) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  pid_t pid;
  int priority = INTEGER(value)[0];
  int retval;

  if (!handle) error("Process pointer cleaned up already");

  pid = handle->pid;

#ifdef PSUTIL_OSX
  retval = setpriority(PRIO_PROCESS, (id_t)pid, priority);
#else
  retval = setpriority(PRIO_PROCESS, pid, priority);
#endif

  if (retval == -1) {
    ps__check_for_zombie(handle, 1);
    ps__set_error_from_errno();
    ps__throw_error();
  } else {
    ps__check_for_zombie(handle, 0);
  }

  return R_NilValue;
}
