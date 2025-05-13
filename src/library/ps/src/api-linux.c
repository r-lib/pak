#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif

#include "ps-internal.h"

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <dirent.h>
#include <utmp.h>
#include <mntent.h>
#include <sys/sysinfo.h>
#include <sched.h>
#include <sys/vfs.h>
#ifndef __linux__
#include <libgen.h>
#endif
#include <sys/syscall.h>
#include <sys/epoll.h>
#include <time.h>
#include <sys/inotify.h>
#include <poll.h>

#include <Rinternals.h>

#include "common.h"
#include "cleancall.h"

#ifndef SYS_pidfd_open
#define SYS_pidfd_open              434
#endif

double psll_linux_boot_time = 0;
double psll_linux_clock_period = 0;

typedef struct {
  char state;
  int ppid, pgrp, session, tty_nr, tpgid;
  unsigned int flags;
  unsigned long minflt, cminflt, majflt, cmajflt, utime, stime;
  long int cutime, cstime, priority, nice, num_threads, itrealvalue;
  unsigned long long starttime;
} psl_stat_t;

#define PS__TV2DOUBLE(t) ((t).tv_sec + (t).tv_usec / 1000000.0)

#define PS__CHECK_STAT(stat, handle)			\
  do {							\
    double starttime = psll_linux_boot_time +		\
      ((double)(stat.starttime) * psll_linux_clock_period);	\
    double diff = starttime - (handle)->create_time;	\
    if (fabs(diff) > psll_linux_clock_period) {		\
      ps__no_such_process((handle)->pid, 0);		\
      ps__throw_error();				\
    }							\
  } while (0)

#define PS__CHECK_HANDLE(handle)			\
  do {							\
    psl_stat_t stat;					\
    if (psll__parse_stat_file(handle->pid, &stat, 0)) {	\
      ps__wrap_linux_error(handle);			\
      ps__throw_error();				\
    }							\
    PS__CHECK_STAT(stat, handle);			\
  } while (0)

#define PS__GET_STATUS(stat, result, error)		\
  switch(stat) {					\
  case 'R': result = mkString("running");      break;	\
  case 'S': result = mkString("sleeping");     break;	\
  case 'D': result = mkString("disk_sleep");   break;	\
  case 'T': result = mkString("stopped");      break;	\
  case 't': result = mkString("tracing_stop"); break;	\
  case 'Z': result = mkString("zombie");       break;	\
  case 'X': result = mkString("dead");         break;	\
  case 'x': result = mkString("dead");         break;	\
  case 'K': result = mkString("wake_kill");    break;	\
  case 'W': result = mkString("waking");       break;	\
  default: error;					\
  }

int ps__read_file(const char *path, char **buffer, size_t buffer_size);

static void *ps__memmem(const void *haystack, size_t n1,
			const void *needle, size_t n2) {

  const unsigned char *p1 = haystack;
  const unsigned char *p2 = needle;
  const unsigned char *p3 = p1 + n1 - n2 + 1;
  const unsigned char *p;

  if (n2 == 0) return (void*)p1;
  if (n2 > n1) return NULL;

  for (p = p1; (p = memchr(p, *p2, p3 - p)) != NULL; p++) {
    if (!memcmp(p, p2, n2)) return (void*)p;
  }

  return NULL;
}

void psll_finalizer(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  if (handle) free(handle);
}

void ps__wrap_linux_error(ps_handle_t *handle) {
  char path[512];
  int ret;

  if (errno == ENOENT || errno == ESRCH) {
    /* no such file error; might be raised also if the */
    /* path actually exists for system processes with */
    /* low pids (about 0-20) */
    struct stat st;
    snprintf(path, sizeof(path), "/proc/%i", handle->pid);
    ret = lstat(path, &st);
    if (!ret) {
      /* process exists, other error */
      ps__set_error_from_errno();
    } else if (errno == ENOENT) {
      ps__no_such_process(handle->pid, 0);
    } else if (errno == EPERM || errno == EACCES)  {
      ps__access_denied("");
    } else {
      ps__set_error_from_errno();
    }
  } else {
    ps__set_error_from_errno();
  }

  ps__throw_error();
}

int psll__readlink(const char *path, char **linkname) {
  size_t size = 1024;
  ssize_t r;
  char *dpos;

  *linkname = R_alloc(size, 1);

  while (1) {

    r = readlink(path, *linkname, size - 1);

    if (r == (ssize_t) -1) {
      return -1;

    } else if (r < (ssize_t)1) {
      errno = ENOENT;
      return -1;

    } else if (r < (ssize_t)(size - 1)) {
      break;
    }

    *linkname = S_realloc(*linkname, size + 1024, size, 1);
    size += 1024;
  }

  (*linkname)[r] = '\0';

  /* readlink() might return paths containing null bytes ('\x00')
     resulting in "TypeError: must be encoded string without NULL
     bytes, not str" errors when the string is passed to other
     fs-related functions (os.*, open(), ...).
     Apparently everything after '\x00' is garbage (we can have
     ' (deleted)', 'new' and possibly others), see:
     https://github.com/giampaolo/psutil/issues/717

     For us this is not a problem, because mkString uses the string
     up to the first zero byte, anyway.

     Certain paths have ' (deleted)' appended. Usually this is
     bogus as the file actually exists. Even if it doesn't we
     don't care. */

  if ((dpos = strstr(*linkname,  " (deleted)")) != NULL &&
      !strcmp(dpos, " (deleted)")) {
    struct stat st;
    int ret = stat(*linkname, &st);
    if (!ret) *dpos = '\0';
  }

  return 0;
}

int psll__parse_stat_file(long pid, psl_stat_t *stat, char **name) {
  char path[512];
  int ret;
  char *buf;
  char *l, *r;

  ret = snprintf(path, sizeof(path), "/proc/%ld/stat", pid);
  if (ret >= sizeof(path)) {
    ps__set_error("Cannot read proc, path buffer too small");
    return -1;
  } else if (ret < 0) {
    return -1;
  }

  ret = ps__read_file(path, &buf, /* buffer= */ 2048);
  if (ret == -1) return -1;

  /* This removes the last character, but that's a \n anyway.
     At least we have a zero terminated string... */
  *(buf + ret - 1) = '\0';

  /* Find the first '(' and last ')', that's the end of the command */
  l = strchr(buf, '(');
  r = strrchr(buf, ')');
  if (!l || !r) {
    ps__set_error("Cannot parse stat file");
    ps__throw_error();
  }

  *r = '\0';
  if (name) *name = l + 1;

  ret = sscanf(r+2,
    "%c %d %d %d %d %d %u %lu %lu %lu %lu %lu %lu %ld %ld %ld %ld %ld %ld %llu",
    &stat->state, &stat->ppid, &stat->pgrp, &stat->session, &stat->tty_nr,
    &stat->tpgid, &stat->flags, &stat->minflt, &stat->cminflt,
    &stat->majflt, &stat->cmajflt, &stat->utime, &stat->stime,
    &stat->cutime, &stat->cstime, &stat->priority, &stat->nice,
    &stat->num_threads, &stat->itrealvalue, &stat->starttime);

  if (ret == -1) {
    return -1;
  } else if (ret != 20) {
    ps__set_error("Cannot parse stat file, parsed: %i/20 fields", ret);
    return -1;
  }

  return 0;
}

void ps__check_for_zombie(ps_handle_t *handle, int err) {
  psl_stat_t stat;
  double diff;

  if (!handle) error("Process pointer cleaned up already");

  if (psll__parse_stat_file(handle->pid, &stat, 0)) {
    ps__wrap_linux_error(handle);
    ps__throw_error();
  }

  diff = (psll_linux_boot_time +
           ((double)stat.starttime * psll_linux_clock_period)) -
         handle->create_time;
  if (fabs(diff) > psll_linux_clock_period) {
    ps__no_such_process(handle->pid, 0);
    err = 1;
  } else if (stat.state == 'Z') {
    ps__zombie_process(handle->pid);
    err = 1;
  } else {
    ps__set_error_from_errno();
  }

  if (err) ps__throw_error();
}

int psll_linux_get_boot_time(void) {
  int ret;
  char *buf;
  char *needle = "\nbtime ";
  size_t needle_len = strlen(needle);
  char *hit;
  unsigned long btime;

  ret = ps__read_file("/proc/stat", &buf, /* buffer= */ 2048);
  if (ret == -1) return -1;

  *(buf + ret - 1) = '\0';
  hit = ps__memmem(buf, ret, needle, needle_len);
  if (!hit) return -1;

  ret = sscanf(hit + needle_len, "%lu", &btime);
  if (ret != 1) return -1;
  psll_linux_boot_time = (double) btime;
  return 0;
}

int psll_linux_get_clock_period(void) {
  double psll_linux_clock_ticks = sysconf(_SC_CLK_TCK);
  if (psll_linux_clock_ticks == -1) {
    ps__set_error_from_errno();
    return -1;
  }
  psll_linux_clock_period = 1.0 / psll_linux_clock_ticks;
  return 0;
}

int psll_linux_ctime(long pid, double *ctime) {
  psl_stat_t stat;
  int ret = psll__parse_stat_file(pid, &stat, 0);
  if (ret) return ret;

  if (!psll_linux_boot_time) {
    ret = psll_linux_get_boot_time();
    if (ret) return ret;
  }

  if (!psll_linux_clock_period) {
    ret = psll_linux_get_clock_period();
    if (ret) {
      ps__throw_error();
    }
  }

  *ctime = psll_linux_boot_time +
    ((double)stat.starttime * psll_linux_clock_period);

  return 0;
}

SEXP psll_handle(SEXP pid, SEXP time) {
  pid_t cpid = isNull(pid) ? getpid() : INTEGER(pid)[0];
  double ctime;
  ps_handle_t *handle;
  SEXP res;

  if (!isNull(time))  {
    ctime = REAL(time)[0];
  } else {
    if (psll_linux_ctime(cpid, &ctime)) {
      ps__set_error_from_errno();
      ps__throw_error();
    }
  }

  handle = malloc(sizeof(ps_handle_t));

  if (!handle) {
    ps__no_memory("");
    ps__throw_error();
  }

  handle->pid = cpid;
  handle->create_time = ctime;
  handle->gone = 0;

  PROTECT(res = R_MakeExternalPtr(handle, R_NilValue, R_NilValue));
  R_RegisterCFinalizerEx(res, psll_finalizer, /* onexit */ 0);
  setAttrib(res, R_ClassSymbol, mkString("ps_handle"));

  UNPROTECT(1);
  return res;
}

SEXP psll_format(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  psl_stat_t stat;
  SEXP name, status, result;
  char *cname;

  if (!handle) error("Process pointer cleaned up already");

  if (psll__parse_stat_file(handle->pid, &stat, &cname)) {
    PROTECT(name = mkString("???"));
    PROTECT(status = mkString("terminated"));
  } else {
    PROTECT(name = ps__str_to_utf8(cname));
    PS__GET_STATUS(stat.state, status, status = mkString("unknown"));
    PROTECT(status);
  }

  PROTECT(result = ps__build_list("OldO", name, (long) handle->pid,
				  handle->create_time, status));

  /* We do not check that the pid is still valid here, because we want
     to be able to format & print processes that have finished already. */

  UNPROTECT(3);
  return result;
}

SEXP psll_parent(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  psl_stat_t stat;
  SEXP ppid, parent;

  if (!handle) error("Process pointer cleaned up already");

  if (psll__parse_stat_file(handle->pid, &stat, 0)) {
    ps__wrap_linux_error(handle);
    ps__throw_error();
  }
  PS__CHECK_STAT(stat, handle);

  /* TODO: this is a race condition, because the parent process might
     have just quit, so psll_handle() might fail. If this happens, then
     we should try to query the ppid again. */

  PROTECT(ppid = ScalarInteger(stat.ppid));
  PROTECT(parent = psll_handle(ppid, R_NilValue));

  UNPROTECT(2);
  return parent;
}

SEXP psll_ppid(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  psl_stat_t stat;

  if (!handle) error("Process pointer cleaned up already");

  if (psll__parse_stat_file(handle->pid, &stat, 0)) {
    ps__wrap_linux_error(handle);
    ps__throw_error();
  }
  PS__CHECK_STAT(stat, handle);

  return ScalarInteger(stat.ppid);
}

SEXP psll_is_running(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  double ctime;
  int ret;

  if (!handle) error("Process pointer cleaned up already");

  ret = psll_linux_ctime(handle->pid, &ctime);
  if (ret) return ScalarLogical(0);

  return ScalarLogical(ctime == handle->create_time);
}

SEXP psll_name(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  psl_stat_t stat;
  char *name;

  if (!handle) error("Process pointer cleaned up already");

  if (psll__parse_stat_file(handle->pid, &stat, &name)) {
    ps__wrap_linux_error(handle);
    ps__throw_error();
  }
  PS__CHECK_STAT(stat, handle);

  return ps__str_to_utf8(name);
}

SEXP psll_exe(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  char path[512];
  int ret;
  char *linkname;

  if (!handle) error("Process pointer cleaned up already");

  ret = snprintf(path, sizeof(path), "/proc/%i/exe", handle->pid);
  if (ret < 0) ps__throw_error();

  ret = psll__readlink(path, &linkname);
  if (ret) {
    if (errno == ENOENT || errno == ESRCH) {
      /* no such file error; might be raised also if the */
      /* path actually exists for system processes with */
      /* low pids (about 0-20) */
      struct stat st;
      snprintf(path, sizeof(path), "/proc/%i", handle->pid);
      ret = lstat(path, &st);
      if (!ret) {
	/* process exists, but can't get exe */
	ps__check_for_zombie(handle, 0);
	return ScalarString(NA_STRING);

      } else if (errno == ENOENT) {
	ps__no_such_process(handle->pid, 0);
	ps__throw_error();
      }
    }
    ps__check_for_zombie(handle, 1);
  }

  PS__CHECK_HANDLE(handle);
  return ps__str_to_utf8(linkname);
}

SEXP psll_cmdline(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  char path[512];
  int ret;
  char *buf, *ptr, *end, *prev;
  char sep = '\0';
  int nstr = 0;
  SEXP result;

  if (!handle) error("Process pointer cleaned up already");

  ret = snprintf(path, sizeof(path), "/proc/%d/cmdline", handle->pid);
  if (ret >= sizeof(path)) {
    ps__set_error("Cannot read proc, path buffer too small");
    ps__throw_error();
  } else if (ret < 0) {
    ps__set_error_from_errno();
    ps__throw_error();
  }

  ret = ps__read_file(path, &buf, 1024);
  if (ret <= 0) {
    ps__check_for_zombie(handle, 1);
  }

  PS__CHECK_HANDLE(handle);

  /* 'man proc' states that args are separated by null bytes '\0' */
  /* and last char is supposed to be a null byte. Nevertheless */
  /* some processes may change their cmdline after being started */
  /* (via setproctitle() or similar), they are usually not */
  /* compliant with this rule and use spaces instead. Google */
  /* Chrome process is an example. See: */
  /* https://github.com/giampaolo/psutil/issues/1179 */

  if (buf[ret - 1] != '\0') sep = ' ';

  /* Count number of vars first, then convert to strings */
  for (ptr = buf, end = buf + ret; ptr < end; ptr++) {
    if (*ptr == sep) nstr++;
  }

  PROTECT(result = allocVector(STRSXP, nstr));
  for (ptr = prev = buf, nstr = 0; ptr < end; ptr++) {
    if (!*ptr) {
      SET_STRING_ELT(result, nstr++, mkCharLen(prev, ptr - prev));
      prev = ptr + 1;
    }
  }

  UNPROTECT(1);
  return result;
}

SEXP psll_status(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  psl_stat_t stat;
  SEXP result;

  if (!handle) error("Process pointer cleaned up already");

  if (psll__parse_stat_file(handle->pid, &stat, 0)) {
    ps__wrap_linux_error(handle);
    ps__throw_error();
  }

  PS__CHECK_STAT(stat, handle);

  PS__GET_STATUS(stat.state, result, error("Unknown process status"));

  return  result;
}

SEXP psll_username(SEXP p) {
  SEXP ids, ruid, pw, result;

  PROTECT(ids = psll_uids(p));
  PROTECT(ruid = ScalarInteger(INTEGER(ids)[0]));
  PROTECT(pw = ps__get_pw_uid(ruid));
  PROTECT(result = VECTOR_ELT(pw, 0));

  UNPROTECT(4);
  return result;
}

SEXP psll_cwd(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  char path[512];
  int ret;
  char *linkname;

  if (!handle) error("Process pointer cleaned up already");

  ret = snprintf(path, sizeof(path), "/proc/%d/cwd", handle->pid);
  if (ret >= sizeof(path)) {
    ps__set_error("Cannot read proc, path buffer too small");
    ps__throw_error();
  } else if (ret < 0) {
    ps__set_error_from_errno();
    ps__throw_error();
  }

  ret = psll__readlink(path, &linkname);
  if (ret) {
    ps__check_for_zombie(handle, 1);
  }

  PS__CHECK_HANDLE(handle);

  return ps__str_to_utf8(linkname);
}

SEXP psll__ids(SEXP p, const char *needle) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  char path[512];
  int ret;
  char *buf;
  size_t needle_len  = strlen(needle);
  char *hit;
  unsigned long real, eff, saved;
  SEXP result, names;

  if (!handle) error("Process pointer cleaned up already");

  ret = snprintf(path, sizeof(path), "/proc/%i/status", handle->pid);
  if (ret >= sizeof(path)) {
    ps__set_error("Cannot read proc, path buffer too small");
    ps__throw_error();
  } else if (ret < 0) {
    ps__set_error_from_errno();
    ps__throw_error();
  }

  ret = ps__read_file(path, &buf, /* buffer= */ 2048);
  if (ret == -1) ps__check_for_zombie(handle, 1);

  *(buf + ret - 1) = '\0';
  hit = ps__memmem(buf, ret, needle, needle_len);
  if (!hit) {
    ps__set_error("Cannot read process status file");
    ps__throw_error();
  }

  ret = sscanf(hit + needle_len, " %lu %lu %lu", &real, &eff, &saved);
  if (ret != 3) {
    ps__set_error("Cannot read process status file");
    ps__throw_error();
  }

  PS__CHECK_HANDLE(handle);

  PROTECT(result = allocVector(INTSXP, 3));
  INTEGER(result)[0] = real;
  INTEGER(result)[1] = eff;
  INTEGER(result)[2] = saved;
  PROTECT(names = ps__build_string("real", "effective", "saved", NULL));
  setAttrib(result, R_NamesSymbol, names);

  UNPROTECT(2);
  return result;
}

SEXP psll_uids(SEXP p) {
  return psll__ids(p, "\nUid:");
}

SEXP psll_gids(SEXP p) {
  return psll__ids(p, "\nGid:");
}

SEXP psll_terminal(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  psl_stat_t stat;

  if (!handle) error("Process pointer cleaned up already");

  if (psll__parse_stat_file(handle->pid, &stat, 0)) {
    ps__wrap_linux_error(handle);
    ps__throw_error();
  }
  PS__CHECK_STAT(stat, handle);

  if (stat.tty_nr != 0) {
    return ScalarInteger(stat.tty_nr);
  }

  if (handle->pid != getpid()) {
    return ScalarInteger(NA_INTEGER);
  }

  // It is us, try ttyname. This is a workaround for qemu, where
  // /proc/self/stat is messed up
  char const *tty = ttyname (STDIN_FILENO);
  if (! tty) {
    return ScalarInteger(NA_INTEGER);
  }
  return mkString(tty);
}

SEXP psll_environ(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  char path[512];
  int ret;
  char *buf, *ptr, *end, *prev;
  SEXP result = R_NilValue;
  int nstr = 0;

  if (!handle) error("Process pointer cleaned up already");

  ret = snprintf(path, sizeof(path), "/proc/%d/environ", handle->pid);
  if (ret >= sizeof(path)) {
    ps__set_error("Cannot read proc, path buffer too small");
    ps__throw_error();
  } else if (ret < 0) {
    ps__set_error_from_errno();
    ps__throw_error();
  }

  ret = ps__read_file(path, &buf, /* buffer= */ 1024 * 32);
  if (ret <= 0) {
    ps__check_for_zombie(handle, 1);
  } else {
    PS__CHECK_HANDLE(handle);
  }

  *(buf + ret - 1) = '\0';

  /* Count number of vars first, then convert to strings */
  for (ptr = buf, end = buf + ret; ptr < end; ptr++) if (!*ptr) nstr++;

  PROTECT(result = allocVector(STRSXP, nstr));
  for (ptr = prev = buf, nstr = 0; ptr < end; ptr++) {
    if (!*ptr) {
      SET_STRING_ELT(result, nstr++, mkCharLen(prev, ptr - prev));
      prev = ptr + 1;
    }
  }

  UNPROTECT(1);
  return result;
}

SEXP psll_num_threads(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  psl_stat_t stat;
  int ret;

  if (!handle) error("Process pointer cleaned up already");

  ret = psll__parse_stat_file(handle->pid, &stat, 0);
  ps__check_for_zombie(handle, ret < 0);

  PS__CHECK_STAT(stat, handle);

  return ScalarInteger(stat.num_threads);
}

SEXP psll_cpu_times(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  psl_stat_t stat;
  SEXP result, names;
  int ret;

  if (!handle) error("Process pointer cleaned up already");

  ret = psll__parse_stat_file(handle->pid, &stat, 0);
  ps__check_for_zombie(handle, ret < 0);

  PS__CHECK_STAT(stat, handle);

  PROTECT(result = allocVector(REALSXP, 4));
  REAL(result)[0] = stat.utime * psll_linux_clock_period;
  REAL(result)[1] = stat.stime * psll_linux_clock_period;
  REAL(result)[2] = stat.cutime * psll_linux_clock_period;
  REAL(result)[3] = stat.cstime * psll_linux_clock_period;
  PROTECT(names = ps__build_string("user", "system", "children_user",
				   "children_system", NULL));
  setAttrib(result, R_NamesSymbol, names);

  UNPROTECT(2);
  return result;
}

SEXP psll_memory_info(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  unsigned long rss, vms, shared, text, lib, data, dirty;
  char path[512];
  char *buf;
  int ret;
  SEXP result, names;

  if (!handle) error("Process pointer cleaned up already");

  ret = snprintf(path, sizeof(path), "/proc/%d/statm", handle->pid);
  if (ret >= sizeof(path)) {
    ps__set_error("Cannot read statm, path buffer too small");
    ps__throw_error();
  } else if (ret < 0) {
    ps__set_error_from_errno();
    ps__throw_error();
  }

  ret = ps__read_file(path, &buf, /* buffer= */ 1024);
  ps__check_for_zombie(handle, ret <= 0);

  *(buf + ret - 1) = '\0';

  ret = sscanf(buf, "%lu %lu %lu %lu %lu %lu %lu", &vms, &rss, &shared,
	       &text, &lib, &data, &dirty);
  if (ret != 7) {
    ps__set_error_from_errno();
    ps__throw_error();
  }

  PS__CHECK_HANDLE(handle);

#ifdef _SC_PAGESIZE
    // recommended POSIX
    int page_size = sysconf(_SC_PAGESIZE);
#elif _SC_PAGE_SIZE
    // alias
    int page_size = sysconf(_SC_PAGE_SIZE);
#endif

  PROTECT(result = allocVector(REALSXP, 7));
  REAL(result)[0] = rss * page_size;
  REAL(result)[1] = vms * page_size;
  REAL(result)[2] = shared * page_size;
  REAL(result)[3] = text * page_size;
  REAL(result)[4] = lib * page_size;
  REAL(result)[5] = data * page_size;
  REAL(result)[6] = dirty * page_size;
  PROTECT(names = ps__build_string("rss", "vms", "shared", "text", "lib",
				   "data", "dirty", NULL));
  setAttrib(result, R_NamesSymbol, names);

  UNPROTECT(2);
  return result;
}

SEXP ps__boot_time(void) {
  if (psll_linux_boot_time == 0) {
    if (psll_linux_get_boot_time()) {
      ps__set_error_from_errno();
      ps__throw_error();
    }
  }
  return ScalarReal(psll_linux_boot_time);
}

SEXP ps__cpu_count_logical(void) {
  int n = sysconf(_SC_NPROCESSORS_ONLN);
  if (n >= 1) return ScalarInteger(n);
  return(ScalarInteger(NA_INTEGER));
}

/* This is not used, because it is much easier to parse this file from R */

SEXP ps__cpu_count_physical(void) {
  return R_NilValue;
}

static int psl__linux_match_environ(SEXP r_marker, SEXP r_pid) {
  const char *marker = CHAR(STRING_ELT(r_marker, 0));
  pid_t pid = INTEGER(r_pid)[0];
  char path[512];
  int ret;
  char *buf;

  ret = snprintf(path, sizeof(path), "/proc/%d/environ", (int) pid);
  if (ret >= sizeof(path)) {
    ps__set_error("Cannot read proc, path buffer too small");
    return -1;
  } else if (ret < 0) {
    ps__set_error_from_errno();
    return -1;
  }

  ret = ps__read_file(path, &buf, /* buffer= */ 1024 * 32);
  if (ret == -1) {
    ps__set_error_from_errno();
    return -1;
  }

  return ps__memmem(buf, ret, marker, strlen(marker)) != NULL;
}

SEXP ps__find_if_env(SEXP r_marker, SEXP r_after, SEXP r_pid) {
  SEXP phandle;
  int match;
  ps_handle_t *handle;

  PROTECT(phandle = psll_handle(r_pid,  R_NilValue));
  handle = R_ExternalPtrAddr(phandle);

  match = psl__linux_match_environ(r_marker, r_pid);
  if (match == -1) ps__throw_error();

  if (match) {
    PS__CHECK_HANDLE(handle);
    UNPROTECT(1);
    return phandle;
  }

  UNPROTECT(1);
  return R_NilValue;
}

SEXP psll_num_fds(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  char path[512];
  DIR *dirs;
  int ret;
  int num = 0;

  if (!handle) error("Process pointer cleaned up already");

  ret = snprintf(path, sizeof(path), "/proc/%i/fd", handle->pid);
  if (ret < 0) ps__throw_error();

  dirs = opendir(path);
  if (!dirs) ps__check_for_zombie(handle, 1);

  do {
    errno = 0;
    struct dirent *entry = readdir(dirs);
    if (!entry) {
      closedir(dirs);
      if (!errno) break;
      ps__check_for_zombie(handle, 1);
    }
    if (strncmp(".", entry->d_name, 2) &&
	strncmp("..", entry->d_name, 3)) num++;
  } while (1);

  /* OSX throws on zombies, so for consistency we do the same here*/
  ps__check_for_zombie(handle, 0);

  PS__CHECK_HANDLE(handle);

  return ScalarInteger(num);
}

SEXP psll_open_files(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  char path[512];
  DIR *dirs;
  int ret;
  int len = 10, num = 0;
  char *linkname;
  int fd, dfd;

  PROTECT_INDEX pidx;
  SEXP result;

  if (!handle) error("Process pointer cleaned up already");

  ret = snprintf(path, sizeof(path), "/proc/%i/fd", handle->pid);
  if (ret < 0) ps__throw_error();

  dirs = opendir(path);
  if (!dirs) ps__check_for_zombie(handle, 1);

  dfd = dirfd(dirs);
  PROTECT_WITH_INDEX(result = allocVector(VECSXP, len), &pidx);

  do {
    errno = 0;
    struct dirent *entry = readdir(dirs);
    if (entry == NULL) {
      closedir(dirs);
      if (errno == 0) break;
      ps__check_for_zombie(handle, 1);
    }

    if (!strncmp(".", entry->d_name, 2) ||
	!strncmp("..", entry->d_name, 3)) continue;

    ret = snprintf(path, sizeof(path), "/proc/%i/fd/%s", handle->pid,
		   entry->d_name);
    if (ret < 0) {
      closedir(dirs);
      ps__throw_error();
    }

    ret = psll__readlink(path, &linkname);
    if (ret) {
      closedir(dirs);
      if (errno == ENOENT || errno == ESRCH || errno == EINVAL) continue;
      ps__check_for_zombie(handle, 1);
    }

    if (strncmp("socket:", linkname, 7) == 0) continue;

    fd = strtol(entry->d_name, NULL, 10);
    if (fd == dfd) continue;
    if (++num == len) {
      len *= 2;
      REPROTECT(result = Rf_lengthgets(result, len), pidx);
    }
    SET_VECTOR_ELT(result, num, ps__build_list("si", linkname, fd));
  } while (1);

  /* OSX throws on zombies, so for consistency we do the same here*/
  ps__check_for_zombie(handle, 0);

  PS__CHECK_HANDLE(handle);

  UNPROTECT(1);
  return result;
}

SEXP psll_connections(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  char path[512];
  DIR *dirs;
  int ret;
  char *linkname;
  size_t l;
  SEXP result;
  PROTECT_INDEX pidx;
  int len = 10, num = 0;

  PROTECT_WITH_INDEX(result = allocVector(VECSXP, len), &pidx);

  if (!handle) error("Process pointer cleaned up already");

  ret = snprintf(path, sizeof(path), "/proc/%d/fd", handle->pid);
  if (ret < 0) ps__throw_error();

  dirs = opendir(path);
  if (!dirs) ps__check_for_zombie(handle, 1);

  do {
    errno = 0;
    struct dirent *entry = readdir(dirs);
    if (!entry) {
      closedir(dirs);
      if (!errno) break;
      ps__check_for_zombie(handle, 1);
    }

    if (!strncmp(".", entry->d_name, 2) ||
	!strncmp("..", entry->d_name, 3)) continue;

    ret = snprintf(path, sizeof(path), "/proc/%i/fd/%s", handle->pid,
		   entry->d_name);
    if (ret < 0) {
      closedir(dirs);
      ps__throw_error();
    }

    ret = psll__readlink(path, &linkname);
    if (ret) {
      if (errno == ENOENT || errno == ESRCH || errno == EINVAL) continue;
      closedir(dirs);
      ps__check_for_zombie(handle, 1);
    }

    l = strlen(linkname);
    if (l < 10) continue;

    linkname[7] = '\0';
    if (strcmp(linkname, "socket:")) continue;

    if (++num == len) {
      len *= 2;
      REPROTECT(result = Rf_lengthgets(result, len), pidx);
    }

    linkname[l - 1] = '\0';
    SET_VECTOR_ELT(
      result, num,
      ps__build_list("ss", entry->d_name, linkname + 8));

  } while (1);

  /* OSX throws on zombies, so for consistency we do the same here*/
  ps__check_for_zombie(handle, 0);

  PS__CHECK_HANDLE(handle);

  UNPROTECT(1);
  return result;
}

SEXP ps__memory_maps(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  char path[512];
  int ret;
  char *buf;

  if (!handle) error("Process pointer cleaned up already");

  ret = snprintf(path, sizeof(path), "/proc/%i/smaps", handle->pid);
  if (ret >= sizeof(path)) {
    ps__set_error("Cannot read proc, path buffer too small");
    ps__throw_error();
  } else if (ret < 0) {
    ps__set_error_from_errno();
    ps__throw_error();
  }

  ret = ps__read_file(path, &buf, /* buffer_size= */ 2048);
  if (ret == -1) {
    ps__wrap_linux_error(handle);
    ps__throw_error();
  }

  ps__check_for_zombie(handle, 0);

  SEXP result = PROTECT(allocVector(STRSXP, 1));
  SET_STRING_ELT(result, 0, Rf_mkCharLenCE(buf, ret, CE_UTF8));
  UNPROTECT(1);
  return result;
}

static const int NCPUS_START = sizeof(unsigned long) * CHAR_BIT;

SEXP psll_get_cpu_aff(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  int cpu, ncpus, count, cpucount_s;
  pid_t pid;
  size_t setsize;
  cpu_set_t *mask = NULL;

  if (!handle) error("Process pointer cleaned up already");

  PS__CHECK_HANDLE(handle);

  pid = handle->pid;

  ncpus = NCPUS_START;
  while (1) {
    setsize = CPU_ALLOC_SIZE(ncpus);
    mask = CPU_ALLOC(ncpus);
    if (mask == NULL) {
      ps__no_memory("");
      goto error;
    }
    if (sched_getaffinity(pid, setsize, mask) == 0) {
      break;
    }
    CPU_FREE(mask);
    mask = NULL;
    if (errno != EINVAL) {
      ps__set_error_from_errno();
      goto error;
    }
    if (ncpus > INT_MAX / 2) {
      ps__set_error("could not allocate a large enough CPU set");
      goto error;
    }
    ncpus = ncpus * 2;
  }

  cpucount_s = CPU_COUNT_S(setsize, mask);

  /* This is unsafe, in that in case of an R exception, CPU_FREE will
     not be called. But it is also very hard to fix without cleancall. */
  PROTECT_INDEX pidx;
  SEXP result;
  PROTECT_WITH_INDEX(result = allocVector(INTSXP, cpucount_s), &pidx);
  int pp = 0;

  for (cpu = 0, count = cpucount_s; count; cpu++) {
    if (CPU_ISSET_S(cpu, setsize, mask)) {
      INTEGER(result)[pp++] = cpu;
      --count;
    }
  }
  CPU_FREE(mask);

  REPROTECT(result = Rf_lengthgets(result, pp), pidx);
  UNPROTECT(1);
  return result;

error:
  if (mask) CPU_FREE(mask);
  ps__throw_error();
  return R_NilValue;
}

SEXP psll_set_cpu_aff(SEXP p, SEXP affinity) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  cpu_set_t cpu_set;
  pid_t pid;
  int i, seq_len = LENGTH(affinity);

  if (!handle) error("Process pointer cleaned up already");

  pid = handle->pid;

  CPU_ZERO(&cpu_set);
  for (i = 0; i < seq_len; i++) {
    int value = INTEGER(affinity)[i];
    CPU_SET(value, &cpu_set);
  }

  PS__CHECK_HANDLE(handle);

  if (sched_setaffinity(pid, sizeof(cpu_set), &cpu_set)) {
    ps__set_error_from_errno();
    goto error;
  }

  return R_NilValue;

error:
  ps__throw_error();
  return R_NilValue;
}

// timeout = 0 special case, no need to poll, just check if running
SEXP psll_wait0(SEXP pps) {
  R_xlen_t i, num_handles = Rf_xlength(pps);
  SEXP res = PROTECT(Rf_allocVector(LGLSXP, num_handles));
  for (i = 0; i < num_handles; i++) {
    ps_handle_t *handle = R_ExternalPtrAddr(VECTOR_ELT(pps, i));
    if (!handle) Rf_error("Process pointer #%d cleaned up already", (int) i);
    LOGICAL(res)[i] = ! LOGICAL(psll_is_running(VECTOR_ELT(pps, i)))[0];
  }

  UNPROTECT(1);
  return res;
}

// Add time limit to current time.
// clock_gettime does not fail, as long as the struct timespec
// pointer is ok. So no need to check the return value.
static void add_time(struct timespec *due, int ms) {
  clock_gettime(CLOCK_MONOTONIC, due);
  due->tv_sec = due->tv_sec + ms / 1000;
  due->tv_nsec = due->tv_nsec + (ms % 1000) * 1000000;
  if (due->tv_nsec >= 1000000000) {
    due->tv_nsec -= 1000000000;
    due->tv_sec++;
  }
}

static inline int time_left(struct timespec *due) {
  struct timespec now;
  clock_gettime(CLOCK_MONOTONIC, &now);
  return
    (due->tv_sec - now.tv_sec) * 1000 +
    (due->tv_nsec - now.tv_nsec) / 1000 / 1000;
}

struct psll__wait_inotify_cleanup_data {
  int inotfd;
};

void psll__wait_inotify_cleanup(void *data) {
  struct psll__wait_inotify_cleanup_data *cdata =
    (struct psll__wait_inotify_cleanup_data*) data;
  if (cdata->inotfd != -1) {
    close(cdata->inotfd);
  }
}

SEXP psll_wait_inotify(SEXP pps, SEXP timeout) {
  int ctimeout = INTEGER(timeout)[0];
  int forever = ctimeout < 0;
  R_xlen_t i, num_handles = Rf_xlength(pps);

  struct psll__wait_inotify_cleanup_data cdata = { -1 };
  r_call_on_early_exit(psll__wait_inotify_cleanup, &cdata);

  cdata.inotfd = inotify_init1(IN_NONBLOCK);
  if (cdata.inotfd == -1) {
    ps__set_error_from_errno();
    ps__throw_error();
  }

  SEXP res = PROTECT(Rf_allocVector(LGLSXP, num_handles));
  memset(LOGICAL(res), 0, sizeof(int) * num_handles);
  SEXP rwatches = PROTECT(Rf_allocVector(INTSXP, num_handles));
  int *watches = INTEGER(rwatches);

  char path[128];
  R_xlen_t topoll = 0;
  for (i = 0; i < num_handles; i++) {
    ps_handle_t *handle = R_ExternalPtrAddr(VECTOR_ELT(pps, i));
    if (!handle) Rf_error("Process pointer #%d cleaned up already", (int) i);
    if (!LOGICAL(psll_is_running(VECTOR_ELT(pps, i)))[0]) {
      // already done
      LOGICAL(res)[i] = 1;
    } else {
      snprintf(path, sizeof(path)-1, "/proc/%d/exe", handle->pid);
      watches[i] = inotify_add_watch(cdata.inotfd, path, IN_CLOSE_NOWRITE);
      if (watches[i] == -1) {
        if (errno == ENOENT) {
          // just finished
          LOGICAL(res)[i] = 0;
        } else {
          ps__set_error_from_errno();
          ps__throw_error();
        }
      } else {
        topoll++;
      }
    }
  }

  // early exit if nothing to do
  if (topoll == 0) {
    psll__wait_inotify_cleanup(&cdata);
    UNPROTECT(2);
    return res;
  }

  // first timeout is the smaller of PROCESSX_INTERRUPT_INTERVAL & timeout
  int ts;
  if (forever || ctimeout > PROCESSX_INTERRUPT_INTERVAL) {
    ts = PROCESSX_INTERRUPT_INTERVAL;
  } else {
    ts = ctimeout;
  }

  // this is the ultimate time limit, unless we poll forever
  struct timespec due;
  if (!forever) {
    add_time(&due, ctimeout);
  }

  struct pollfd pfd = { cdata.inotfd, POLLIN, 0 };
  int ret;

  do {
    do {
      ret = poll(&pfd, 1, ts);
    } while (ret == -1 && errno == EINTR);

    if (ret == -1) {
      ps__set_error_from_errno();
      ps__throw_error();
    }

    if (ret > 0) {
      struct inotify_event event;
      ret = read(cdata.inotfd, &event, sizeof(event));
      if (ret == -1) {
        ps__set_error_from_errno();
        ps__throw_error();
      }
      // this should not happen
      if (event.len > 0) {
        Rf_error("Invalid inotify event in ps_wait.");
      }

      // now we need to see if the event means that any processes have quit
      for (i = 0; i < num_handles; i++) {
        // we know that it finished
        if (LOGICAL(res)[i]) continue;
        if (watches[i] == event.wd) {
          if (!LOGICAL(psll_is_running(VECTOR_ELT(pps, i)))[0]) {
            LOGICAL(res)[i] = 1;
            topoll--;
          } else {
            // Still running, but we got an event. Maybe it called execve()
            // We need to start watching the new exe, if any. We might be
            // still watching the old exe, as this might be needed for
            // other processes.
            ps_handle_t *handle = R_ExternalPtrAddr(VECTOR_ELT(pps, i));
            snprintf(path, sizeof(path)-1, "/proc/%d/exe", handle->pid);
            watches[i] = inotify_add_watch(cdata.inotfd, path, IN_CLOSE_NOWRITE);
            if (watches[i] == -1) {
              if (errno == ENOENT) {
                // just finished
                LOGICAL(res)[i] = 1;
                topoll--;
              } else {
                ps__set_error_from_errno();
                ps__throw_error();
              }
            }
          }
        }
      }

      // are we done?
      if (topoll == 0) {
        break;
      }
    }

    // is the time limit over? Do we need to update the poll timeout
    if (!forever) {
      int tl = time_left(&due);
      if (tl < 0) {
        break;
      }
      if (tl < PROCESSX_INTERRUPT_INTERVAL) {
        ts = tl;
      }
    }

    R_CheckUserInterrupt();

  } while (1);

  psll__wait_inotify_cleanup(&cdata);
  UNPROTECT(2);
  return res;
}

struct psll__wait_cleanup_data {
  int epfd;
  R_xlen_t num_handles;
  int *pfds;
};

static void psll__wait_cleanup(void *data) {
  struct psll__wait_cleanup_data *cdata =
    (struct psll__wait_cleanup_data*) data;
  if (cdata->epfd != -1) {
    close(cdata->epfd);
    cdata->epfd = -1;
  }
  R_xlen_t i;
  if (cdata->pfds) {
    for (i = 0; i < cdata->num_handles; i++) {
      if (cdata->pfds[i] != -1) {
        close(cdata->pfds[i]);
      }
    }
  }
}

SEXP psll_wait(SEXP pps, SEXP timeout) {
  int ctimeout = INTEGER(timeout)[0];
  // this is much simpler, no need to poll at all
  if (ctimeout == 0) {
    return psll_wait0(pps);
  }

  // for testing
  if (getenv("PS_WAIT_FORCE_INOTIFY")) {
    return psll_wait_inotify(pps, timeout);
  }

  // Check if the kernel has pidfd_open() support
  if (ps_pidfd_open_support == PS_MAYBE) {
    int testfd = syscall(SYS_pidfd_open, getpid(), /* flags= */ 0);
    if (testfd == -1) {
      if (errno == ENOSYS || errno == ENODEV) {
        ps_pidfd_open_support = PS_NOPE;
      } else {
        // this is a real error, e.g. EMFILE or ENFILE, but nevertheless
        // we have pidfd_open support, so we march on, and error later, if
        ps_pidfd_open_support = PS_YEAH;
      }
    } else {
      ps_pidfd_open_support = PS_YEAH;
      close(testfd);
    }
  }
  if (ps_pidfd_open_support == PS_NOPE) {
    return psll_wait_inotify(pps, timeout);
  }

  int forever = ctimeout < 0;
  R_xlen_t i, num_handles = Rf_xlength(pps);

  struct psll__wait_cleanup_data cdata = {
    /* epfd=        */ -1,
    /* num_handles= */ num_handles,
    /* pfds= */        NULL
  };
  r_call_on_early_exit(psll__wait_cleanup, &cdata);
  cdata.epfd = epoll_create(num_handles);
  if (cdata.epfd == -1) {
    ps__set_error_from_errno();
    ps__throw_error();
  }

  SEXP res = PROTECT(Rf_allocVector(LGLSXP, num_handles));
  SEXP pfds = PROTECT(Rf_allocVector(INTSXP, num_handles));
  cdata.pfds = INTEGER(pfds);

  R_xlen_t topoll = 0;
  for (i = 0; i < num_handles; i++) {
    ps_handle_t *handle = R_ExternalPtrAddr(VECTOR_ELT(pps, i));
    if (!handle) Rf_error("Process pointer #%d cleaned up already", (int) i);
    if (!LOGICAL(psll_is_running(VECTOR_ELT(pps, i)))[0]) {
      // already done
      LOGICAL(res)[i] = 1;
      cdata.pfds[i] = -1;
    } else {
      cdata.pfds[i] = syscall(SYS_pidfd_open, handle->pid, /* flags= */ 0);
      if (cdata.pfds[i] == -1) {
        if (errno == ESRCH) {
          LOGICAL(res)[i] = 1;
          cdata.pfds[i] = -1;
        } else {
          ps__set_error_from_errno();
          ps__throw_error();
        }
      } else {
        topoll++;
        LOGICAL(res)[i] = 0;
        struct epoll_event ev;
        ev.events = EPOLLIN;
        ev.data.u64 = i;
        epoll_ctl(cdata.epfd, EPOLL_CTL_ADD, cdata.pfds[i], &ev);
      }
    }
  }

  // early exit if nothing to do
  if (topoll == 0) {
    psll__wait_cleanup(&cdata);
    UNPROTECT(2);
    return res;
  }

  int ret;
  SEXP revents = PROTECT(Rf_allocVector(RAWSXP, sizeof(struct epoll_event) * topoll));
  struct epoll_event *events = (struct epoll_event*) RAW(revents);

  // first timeout is the smaller of PROCESSX_INTERRUPT_INTERVAL & timeout
  int ts;
  if (forever || ctimeout > PROCESSX_INTERRUPT_INTERVAL) {
    ts = PROCESSX_INTERRUPT_INTERVAL;
  } else {
    ts = ctimeout;
  }

  // this is the ultimate time limit, unless we poll forever
  struct timespec due;
  if (!forever) {
    add_time(&due, ctimeout);
  }

  do {
    do {
      ret = epoll_wait(cdata.epfd, events, topoll, ts);
    } while (ret == -1 && errno == EINTR);

    if (ret == -1) {
      ps__set_error_from_errno();
      ps__throw_error();
    }

    for (i = 0; i < ret; i++) {
      uint64_t id = events[i].data.u64;
      LOGICAL(res)[id] = 1;
      close(cdata.pfds[id]);
      cdata.pfds[id] = -1;
      topoll--;
    }

    // are we done?
    if (topoll == 0) {
      break;
    }

    // is the time limit over? Do we need to update the poll timeout
    if (!forever) {
      int tl = time_left(&due);
      if (tl < 0) {
        break;
      }
      if (tl < PROCESSX_INTERRUPT_INTERVAL) {
        ts = tl;
      }
    }

    R_CheckUserInterrupt();

  } while (1);

  psll__wait_cleanup(&cdata);
  UNPROTECT(3);
  return res;
}

SEXP ps__users(void) {
  struct utmp *ut;
  SEXP result;
  PROTECT_INDEX pidx;
  int len = 10, num = 0;

  PROTECT_WITH_INDEX(result = allocVector(VECSXP, len), &pidx);

  setutent();

  while ((ut = getutent()) != NULL) {

    if (ut->ut_type != USER_PROCESS) continue;

    if (++num == len) {
      len *= 2;
      REPROTECT(result = Rf_lengthgets(result, len), pidx);
    }
    SET_VECTOR_ELT(
      result, num,
      ps__build_list("sssdi", ut->ut_user, ut->ut_line, ut->ut_host,
		     (double) PS__TV2DOUBLE(ut->ut_tv), ut->ut_pid));
  }

  endutent();
  UNPROTECT(1);
  return result;
}

SEXP ps__disk_partitions(SEXP all) {
  FILE *file = NULL;
  struct mntent *entry;
  SEXP result;
  PROTECT_INDEX pidx;
  int len = 30, num = -1;

  file = setmntent("/etc/mtab", "r");
  if ((file == 0) || (file == NULL)) {
    ps__set_error_from_errno();
    goto error;
  }

  PROTECT_WITH_INDEX(result = allocVector(VECSXP, len), &pidx);

  while ((entry = getmntent(file))) {
    if (entry == NULL) {
      ps__set_error_from_errno();
      goto error;
    }

    if (++num == len) {
      len *= 2;
      REPROTECT(result = Rf_lengthgets(result, len), pidx);
    }
    SET_VECTOR_ELT(
      result, num,
      ps__build_list("ssss", entry->mnt_fsname, entry->mnt_dir,
                     entry->mnt_type, entry->mnt_opts));
  }

  endmntent(file);

  UNPROTECT(1);
  return result;

error:
  if (file != NULL) endmntent(file);
  ps__throw_error();
  /* These are never called, but R CMD check and rchk cannot handle this */
  error("nah");
  return R_NilValue;
}

// These are from include/linux/statfs.h in Linux

#define ST_RDONLY	0x0001	/* mount read-only */
#define ST_NOSUID	0x0002	/* ignore suid and sgid bits */
#define ST_NODEV	0x0004	/* disallow access to device special files */
#define ST_NOEXEC	0x0008	/* disallow program execution */
#define ST_SYNCHRONOUS	0x0010	/* writes are synced at once */
#define ST_VALID	0x0020	/* f_flags support is implemented */
#define ST_MANDLOCK	0x0040	/* allow mandatory locks on an FS */
/* 0x0080 used for ST_WRITE in glibc */
/* 0x0100 used for ST_APPEND in glibc */
/* 0x0200 used for ST_IMMUTABLE in glibc */
#define ST_NOATIME	0x0400	/* do not update access times */
#define ST_NODIRATIME	0x0800	/* do not update directory access times */
#define ST_RELATIME	0x1000	/* update atime relative to mtime/ctime */
#define ST_NOSYMFOLLOW	0x2000	/* do not follow symlinks */

SEXP ps__fs_info(SEXP path, SEXP abspath, SEXP mps) {
  struct statfs sfs;
  R_xlen_t i, j, len = Rf_xlength(path);
  int ret;

  // Need to query all partitions to get the device name and fs type.
  SEXP partitions = PROTECT(ps__disk_partitions(Rf_ScalarLogical(1)));
  R_xlen_t num_parts = Rf_xlength(partitions);

  const char *nms[] = {
    "path",
    "mountpoint",
    "name",
    "type",
    "block_size",
    "transfer_block_size",
    "total_data_blocks",
    "free_blocks",
    "free_blocks_non_superuser",
    "total_nodes",
    "free_nodes",
    "id",
    "owner",
    "type_code",
    "subtype_code",

    "MANDLOCK",
    "NOATIME",
    "NODEV",
    "NODIRATIME",
    "NOEXEC",
    "NOSUID",
    "RDONLY",
    "RELATIME",
    "SYNCHRONOUS",
    "NOSYMFOLLOW",
    ""
  };

  SEXP res = PROTECT(Rf_mkNamed(VECSXP, nms));
  SET_VECTOR_ELT(res, 0, path);
  SET_VECTOR_ELT(res, 1, Rf_allocVector(STRSXP, len));
  SET_VECTOR_ELT(res, 2, Rf_allocVector(STRSXP, len));
  SET_VECTOR_ELT(res, 3, Rf_allocVector(STRSXP, len));
  SET_VECTOR_ELT(res, 4, Rf_allocVector(REALSXP, len));
  SET_VECTOR_ELT(res, 5, Rf_allocVector(REALSXP, len));
  SET_VECTOR_ELT(res, 6, Rf_allocVector(REALSXP, len));
  SET_VECTOR_ELT(res, 7, Rf_allocVector(REALSXP, len));
  SET_VECTOR_ELT(res, 8, Rf_allocVector(REALSXP, len));
  SET_VECTOR_ELT(res, 9, Rf_allocVector(REALSXP, len));
  SET_VECTOR_ELT(res, 10, Rf_allocVector(REALSXP, len));
  SET_VECTOR_ELT(res, 11, Rf_allocVector(VECSXP, len));
  SET_VECTOR_ELT(res, 12, Rf_allocVector(REALSXP, len));
  SET_VECTOR_ELT(res, 13, Rf_allocVector(REALSXP, len));
  SET_VECTOR_ELT(res, 14, Rf_allocVector(REALSXP, len));

  SET_VECTOR_ELT(res, 15, Rf_allocVector(LGLSXP, len));
  SET_VECTOR_ELT(res, 16, Rf_allocVector(LGLSXP, len));
  SET_VECTOR_ELT(res, 17, Rf_allocVector(LGLSXP, len));
  SET_VECTOR_ELT(res, 18, Rf_allocVector(LGLSXP, len));
  SET_VECTOR_ELT(res, 19, Rf_allocVector(LGLSXP, len));
  SET_VECTOR_ELT(res, 20, Rf_allocVector(LGLSXP, len));
  SET_VECTOR_ELT(res, 21, Rf_allocVector(LGLSXP, len));
  SET_VECTOR_ELT(res, 22, Rf_allocVector(LGLSXP, len));
  SET_VECTOR_ELT(res, 23, Rf_allocVector(LGLSXP, len));
  SET_VECTOR_ELT(res, 24, Rf_allocVector(LGLSXP, len));

  for (i = 0; i < len; i++) {
    ret = statfs(CHAR(STRING_ELT(abspath, i)), &sfs);
    if (ret != 0) {
      ps__set_error(
        "statfs %s: %d %s",
        CHAR(STRING_ELT(abspath, i)), errno, strerror(errno)
      );
      ps__throw_error();
    }

    SET_STRING_ELT(VECTOR_ELT(res, 1), i, NA_STRING);
    SET_STRING_ELT(VECTOR_ELT(res, 2), i, NA_STRING);
    SET_STRING_ELT(VECTOR_ELT(res, 3), i, NA_STRING);

    // match the mount point to the partitions
    const char *mpi = CHAR(STRING_ELT(mps, i));
    for (j = 0; j < num_parts; j++) {
      const char *mpp = CHAR(STRING_ELT(VECTOR_ELT(VECTOR_ELT(partitions, j), 1), 0));
      if (0 == strcmp(mpi, mpp)) {
        SET_STRING_ELT(
          VECTOR_ELT(res, 1), i,
          STRING_ELT(VECTOR_ELT(VECTOR_ELT(partitions, j), 1), 0));
        SET_STRING_ELT(
          VECTOR_ELT(res, 2), i,
          STRING_ELT(VECTOR_ELT(VECTOR_ELT(partitions, j), 0), 0));
        SET_STRING_ELT(
          VECTOR_ELT(res, 3), i,
          STRING_ELT(VECTOR_ELT(VECTOR_ELT(partitions, j), 2), 0));
        break;
      }
    }

    REAL(VECTOR_ELT(res, 4))[i] = sfs.f_bsize;
    REAL(VECTOR_ELT(res, 5))[i] = sfs.f_bsize;
    REAL(VECTOR_ELT(res, 6))[i] = sfs.f_blocks;
    REAL(VECTOR_ELT(res, 7))[i] = sfs.f_bfree;
    REAL(VECTOR_ELT(res, 8))[i] = sfs.f_bavail;
    REAL(VECTOR_ELT(res, 9))[i] = sfs.f_files;
    REAL(VECTOR_ELT(res, 10))[i] = sfs.f_ffree;
    SET_VECTOR_ELT(VECTOR_ELT(res, 11), i, Rf_allocVector(RAWSXP, sizeof(fsid_t)));
    memcpy(RAW(VECTOR_ELT(VECTOR_ELT(res, 11), i)), &sfs.f_fsid, sizeof(fsid_t));
    REAL(VECTOR_ELT(res, 12))[i] = NA_REAL;
    REAL(VECTOR_ELT(res, 13))[i] = sfs.f_type;
    REAL(VECTOR_ELT(res, 14))[i] = NA_REAL;

    LOGICAL(VECTOR_ELT(res, 15))[i] = sfs.f_flags & ST_MANDLOCK;
    LOGICAL(VECTOR_ELT(res, 16))[i] = sfs.f_flags & ST_NOATIME;
    LOGICAL(VECTOR_ELT(res, 17))[i] = sfs.f_flags & ST_NODEV;
    LOGICAL(VECTOR_ELT(res, 18))[i] = sfs.f_flags & ST_NODIRATIME;
    LOGICAL(VECTOR_ELT(res, 19))[i] = sfs.f_flags & ST_NOEXEC;
    LOGICAL(VECTOR_ELT(res, 20))[i] = sfs.f_flags & ST_NOSUID;
    LOGICAL(VECTOR_ELT(res, 21))[i] = sfs.f_flags & ST_RDONLY;
    LOGICAL(VECTOR_ELT(res, 22))[i] = sfs.f_flags & ST_RELATIME;
    LOGICAL(VECTOR_ELT(res, 23))[i] = sfs.f_flags & ST_SYNCHRONOUS;
    LOGICAL(VECTOR_ELT(res, 24))[i] = sfs.f_flags & ST_NOSYMFOLLOW;
  }

  UNPROTECT(2);
  return res;
}

SEXP ps__loadavg(SEXP counter_name) {
  /* Try /proc first, if fails we try sysinfo() */
  struct sysinfo info;
  char *buf;
  int ret = ps__read_file("/proc/loadavg", &buf, 128);
  SEXP avg = PROTECT(allocVector(REALSXP, 3));
  if (ret == -1) goto sysinfo;

  if (sscanf(buf, "%lf %lf %lf", REAL(avg), REAL(avg) + 1, REAL(avg) + 2) == 3) {
    UNPROTECT(1);
    return avg;
  }

 sysinfo:
  if (sysinfo(&info) < 0) {
    ps__set_error_from_errno();
    ps__throw_error();
  }

  REAL(avg)[0] = (double) info.loads[0] / 65536.0;
  REAL(avg)[1] = (double) info.loads[1] / 65536.0;
  REAL(avg)[2] = (double) info.loads[2] / 65536.0;

  UNPROTECT(1);
  return avg;
}

SEXP ps__system_memory(void) {
  // This is implemented in R on Linux
  ps__throw_error();
  return R_NilValue;
}

SEXP ps__system_swap(void) {
  // TODO
  return R_NilValue;
}

SEXP ps__system_cpu_times(void) {
  // This is implemented in R on Linux
  ps__throw_error();
  return R_NilValue;
}
