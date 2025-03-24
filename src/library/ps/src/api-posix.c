
#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif

#include <errno.h>
#include <sys/ioctl.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/statvfs.h>
#include <sys/resource.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#ifdef __linux__
#include <sys/sysmacros.h>
#endif
#include <sys/param.h>
#include <libgen.h>
#include <stdlib.h>

#include "ps-internal.h"
#include "cleancall.h"

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


SEXP psll_kill(SEXP p, SEXP grace) {
  R_xlen_t i, num_handles = Rf_xlength(p);

  // check if handles are ok, and they are not pid 0
  for (i = 0; i < num_handles; i++) {
    ps_handle_t *handle = R_ExternalPtrAddr(VECTOR_ELT(p, i));
    if (!handle) Rf_error("Process pointer clean up already");
    if (handle->pid == 0) {
      Rf_error(
        "preventing sending KILL signal to process with PID 0 as it "
	      "would affect every process in the process group of the "
        "calling process (Sys.getpid()) instead of PID 0"
      );
    }
  }

  // OK, we can give it a go then
  SEXP res = PROTECT(Rf_allocVector(VECSXP, num_handles));
  SEXP ridx = PROTECT(Rf_allocVector(INTSXP, num_handles));
  int *idx = INTEGER(ridx);
  memset(idx, 0, sizeof(int) * num_handles);

  // Send out TERM signals
  int signals_sent = 0;
  for (i = 0; i < num_handles; i++) {
    if (!LOGICAL(psll_is_running(VECTOR_ELT(p, i)))[0]) {
      SET_VECTOR_ELT(res, i, Rf_mkString("dead"));
      continue;
    }
    ps_handle_t *handle = R_ExternalPtrAddr(VECTOR_ELT(p, i));
    int ret = kill(handle->pid, SIGTERM);
    if (ret == -1) {
      if (errno == ESRCH) {
        SET_VECTOR_ELT(res, i, Rf_mkString("dead"));
        continue;
      } else if (errno == EPERM || errno == EACCES) {
        ps__access_denied_pid(handle->pid, "");
      } else {
        ps__set_error_from_errno();
      }
      SET_VECTOR_ELT(res, i, Rf_duplicate(ps__last_error));
    } else {
      idx[signals_sent++] = i;
    }
  }

  // all done, exit early
  if (signals_sent == 0) {
    UNPROTECT(2);
    return res;
  }

  SEXP p2 = PROTECT(Rf_allocVector(VECSXP, signals_sent));
  for (i = 0; i < signals_sent; i++) {
    SET_VECTOR_ELT(p2, i, VECTOR_ELT(p, idx[i]));
  }
  SEXP waitres = PROTECT(psll_wait(p2, grace));
  for (i = 0; i < signals_sent; i++) {
    if (LOGICAL(waitres)[i]) {
      SET_VECTOR_ELT(res, idx[i], Rf_mkString("terminated"));
    } else {
      ps_handle_t *handle = R_ExternalPtrAddr(VECTOR_ELT(p2, i));
      int ret = kill(handle->pid, SIGKILL);
      if (ret == -1) {
        if (errno == ESRCH) {
          SET_VECTOR_ELT(res, idx[i], Rf_mkString("dead"));
          continue;
        } else if (errno == EPERM || errno == EACCES) {
          ps__access_denied_pid(handle->pid, "");
        } else {
          ps__set_error_from_errno();
        }
        SET_VECTOR_ELT(res, idx[i], Rf_duplicate(ps__last_error));
      } else {
        SET_VECTOR_ELT(res, idx[i], Rf_mkString("killed"));
      }
    }
  }

  UNPROTECT(4);
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

#define POSIXT(x) (((double) (x).tv_sec) + ((double) (x).tv_nsec) / 1000 / 1000 / 1000)

#ifdef __APPLE__
#define st_atim st_atimespec
#define st_mtim st_mtimespec
#define st_ctim st_ctimespec
#endif

SEXP ps__stat(SEXP paths, SEXP follow) {
  const char* nms[] = {
    "path",
    "dev_major",
    "dev_minor",
    "inode",
    "mode",
    "type",
    "permissions",
    "nlink",
    "uid",
    "gid",
    "rdev_major",
    "rdev_minor",
    "size",
    "block_size",
    "blocks",
    "access_time",
    "modification_time",
    "change_time",
    ""
  };

  R_xlen_t i, len = Rf_xlength(paths);
  SEXP res = PROTECT(Rf_mkNamed(VECSXP, nms));
  SET_VECTOR_ELT(res, 0, paths);
  SET_VECTOR_ELT(res, 1, Rf_allocVector(INTSXP, len));
  SET_VECTOR_ELT(res, 2, Rf_allocVector(INTSXP, len));
  SET_VECTOR_ELT(res, 3, Rf_allocVector(REALSXP, len));
  SET_VECTOR_ELT(res, 4, Rf_allocVector(REALSXP, len));
  SET_VECTOR_ELT(res, 5, Rf_allocVector(INTSXP, len));
  SET_VECTOR_ELT(res, 6, Rf_allocVector(INTSXP, len));
  SET_VECTOR_ELT(res, 7, Rf_allocVector(REALSXP, len));
  SET_VECTOR_ELT(res, 8, Rf_allocVector(REALSXP, len));
  SET_VECTOR_ELT(res, 9, Rf_allocVector(REALSXP, len));
  SET_VECTOR_ELT(res, 10, Rf_allocVector(INTSXP, len));
  SET_VECTOR_ELT(res, 11, Rf_allocVector(INTSXP, len));
  SET_VECTOR_ELT(res, 12, Rf_allocVector(REALSXP, len));
  SET_VECTOR_ELT(res, 13, Rf_allocVector(REALSXP, len));
  SET_VECTOR_ELT(res, 14, Rf_allocVector(REALSXP, len));
  SET_VECTOR_ELT(res, 15, Rf_allocVector(REALSXP, len));
  SET_VECTOR_ELT(res, 16, Rf_allocVector(REALSXP, len));
  SET_VECTOR_ELT(res, 17, Rf_allocVector(REALSXP, len));

  int lfollow = LOGICAL(follow)[0];
  for (i = 0; i < len; i++) {
    const char *cpath = CHAR(STRING_ELT(paths, i));
    struct stat st;
    int ret;

    if (lfollow) {
      ret = stat(cpath, &st);
    } else {
      ret = lstat(cpath, &st);
    }

    if (ret) {
      ps__set_error_from_errno();
      ps__throw_error();
    }

    INTEGER(VECTOR_ELT(res, 1))[i] = major(st.st_dev);
    INTEGER(VECTOR_ELT(res, 2))[i] = minor(st.st_dev);
    REAL(VECTOR_ELT(res, 3))[i] = st.st_ino;
    REAL(VECTOR_ELT(res, 4))[i] = st.st_mode;
    INTEGER(VECTOR_ELT(res, 5))[i] = NA_INTEGER;
    int *ctype = INTEGER(VECTOR_ELT(res, 5)) + i;
    switch (st.st_mode & S_IFMT) {
    case S_IFREG: *ctype = 1; break;
    case S_IFDIR: *ctype = 2; break;
    case S_IFCHR: *ctype = 3; break;
    case S_IFBLK: *ctype = 4; break;
    case S_IFIFO: *ctype = 5; break;
    case S_IFLNK: *ctype = 6; break;
    case S_IFSOCK: *ctype = 7; break;
    }
    INTEGER(VECTOR_ELT(res, 6))[i] = st.st_mode & 07777;
    REAL(VECTOR_ELT(res, 7))[i] = st.st_nlink;
    REAL(VECTOR_ELT(res, 8))[i] = st.st_uid;
    REAL(VECTOR_ELT(res, 9))[i] = st.st_gid;
    INTEGER(VECTOR_ELT(res, 10))[i] = NA_INTEGER;
    INTEGER(VECTOR_ELT(res, 11))[i] = NA_INTEGER;
    if (S_ISCHR(st.st_mode) || S_ISBLK(st.st_mode)) {
      INTEGER(VECTOR_ELT(res, 10))[i] = major(st.st_rdev);
      INTEGER(VECTOR_ELT(res, 11))[i] = minor(st.st_rdev);
    }
    REAL(VECTOR_ELT(res, 12))[i] = st.st_size;
    REAL(VECTOR_ELT(res, 13))[i] = st.st_blksize;
    REAL(VECTOR_ELT(res, 14))[i] = st.st_blocks;
    REAL(VECTOR_ELT(res, 15))[i] = POSIXT(st.st_atim);
    REAL(VECTOR_ELT(res, 16))[i] = POSIXT(st.st_mtim);
    REAL(VECTOR_ELT(res, 17))[i] = POSIXT(st.st_ctim);
  }

  UNPROTECT(1);
  return res;
}

void ps__mount_point_cleanup(void *data) {
  char *wd = (char*) data;
  int ret = chdir(wd);
  if (ret) {
    REprintf(
      "Could not restore current working directory "
      "in ps::ps_fs_mount_point()"
    );
  }
}

SEXP ps__mount_point(SEXP paths) {
  R_xlen_t i, len = Rf_xlength(paths);
  struct stat st, lastst;
  int ret;
  char wd[MAXPATHLEN + 1];

  // save wd
  if (!getcwd(wd, MAXPATHLEN)) {
    ps__set_error_from_errno();
    ps__throw_error();
  }

  r_call_on_early_exit(ps__mount_point_cleanup, wd);

  SEXP res = PROTECT(Rf_allocVector(STRSXP, len));

  for (i = 0; i < len; i++) {
    char buf[MAXPATHLEN + 1];
    const char *cpath = CHAR(STRING_ELT(paths, i));
    ret = stat(cpath, &st);
    if (ret) {
      ps__set_error_from_errno();
      ps__throw_error();
    }

    // go into the directory
    if (S_ISDIR(st.st_mode)) {
      ret = chdir(cpath);
      if (ret) {
        ps__set_error_from_errno();
        ps__throw_error();
      }
      lastst = st;

    } else {
      char *tmp = strdup(cpath);
      const char *dn = dirname(tmp);
      if (!dn) {
        free(tmp);
        ps__set_error_from_errno();
        ps__throw_error();
      }
      ret = chdir(dn);
      free(tmp);
      if (ret) {
        ps__set_error_from_errno();
        ps__throw_error();
      }
      ret = stat(".", &lastst);
      if (ret) {
        ps__set_error_from_errno();
        ps__throw_error();
      }
    }

    // walk up until we leave the device or arrive at /
    while (1) {
      ret = stat("..", &st);
      if (ret) {
        ps__set_error_from_errno();
        ps__throw_error();
      }
      if (st.st_dev != lastst.st_dev || st.st_ino == lastst.st_ino) {
        break;
      }
      ret = chdir("..");
      if (ret) {
        ps__set_error_from_errno();
        ps__throw_error();
      }
      lastst = st;
    }

    if (!getcwd(buf, MAXPATHLEN)) {
      ps__set_error_from_errno();
      ps__throw_error();
    }
    SET_STRING_ELT(res, i, Rf_mkChar(buf));
  }

  ret = chdir(wd);
  if (ret) {
    ps__set_error_from_errno();
    ps__throw_error();
  }
  UNPROTECT(1);
  return res;
}

SEXP psll_memory_maxrss(SEXP p) {
  struct rusage rusage;
  getrusage(RUSAGE_SELF, &rusage);
#if defined(__APPLE__)
  return Rf_ScalarReal(rusage.ru_maxrss);
#else
  return Rf_ScalarReal(rusage.ru_maxrss * 1024.0);
#endif
}
