
#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif

#include <Rinternals.h>

#include "processx.h"

#ifdef _WIN32

#include <windows.h>

/* FILETIME origin is January 1, 1601 (UTC).
   See https://msdn.microsoft.com/en-us/9baf8a0e-59e3-4fbd-9616-2ec9161520d1
   Unix origin is January 1, 1970.
   The difference is 11644473600 seconds.
   FILETIME is in 100ns units, so we need to multiple this by 10^7. */

double processx__create_time(HANDLE process) {
  long long   ll, secs, nsecs;
  FILETIME    ftCreate, ftExit, ftKernel, ftUser;

  if (! GetProcessTimes(process, &ftCreate, &ftExit, &ftKernel, &ftUser)) {
    if (GetLastError() == ERROR_ACCESS_DENIED) {
      // usually means the process has died
      return 0.0;
    } else {
      return 0.0;
    }
  }

  ll = ((LONGLONG) ftCreate.dwHighDateTime) << 32;
  ll += ftCreate.dwLowDateTime - 116444736000000000LL;
  secs = ll / 10000000;
  nsecs = ll % 10000000;
  return (double) secs + ((double) nsecs) / 10000000;
}

#endif

/* This is defined on all OSes, but only really needed (and used)
 * on Linux. */

static double processx__linux_boot_time = 0.0;

SEXP processx__set_boot_time(SEXP bt) {
  processx__linux_boot_time = REAL(bt)[0];
  return R_NilValue;
}

#ifdef __linux__

#include <sys/sysinfo.h>
#include <unistd.h>

int processx__read_file(const char *path, char **buffer, size_t buffer_size) {
  int fd = -1;
  ssize_t ret;
  char *ptr;
  size_t rem_size = buffer_size;

  *buffer = 0;

  fd = open(path, O_RDONLY);
  if (fd == -1) goto error;

  ptr = *buffer = R_alloc(buffer_size, 1);
  if (!*buffer) goto error;

  do {
    if (rem_size == 0) {
      *buffer = S_realloc(*buffer, buffer_size * 2, buffer_size, 1);
      if (!*buffer) goto error;
      ptr = *buffer + buffer_size;
      rem_size = buffer_size;
      buffer_size *= 2;
    }

    ret = read(fd, ptr, rem_size);
    if (ret == -1) goto error;

    ptr += ret;
    rem_size -= ret;
  } while (ret > 0);

  close(fd);
  return buffer_size - rem_size;

 error:
  if (fd >= 0) close(fd);
  if (*buffer) free(*buffer);
  *buffer = 0;
  return -1;
}

double processx__create_time_since_boot(long pid) {
  char path[512];
  int ret;
  char *buf;
  char *l, *r;

  char state[2] = { 0, 0 };
  int ppid, pgrp, session, tty_nr, tpgid;
  unsigned int flags;
  unsigned long minflt, cminflt, majflt, cmajflt, utime, stime;
  long int cutime, cstime, priority, nice, num_threads, itrealvalue;
  unsigned long long starttime;

  ret = snprintf(path, sizeof(path), "/proc/%d/stat", (int) pid);
  if (ret >= sizeof(path)) {
    warning("Cannot parse stat file, buffer too small: %s", strerror(errno));
    return 0.0;
  } else if (ret < 0) {
    warning("Cannot parse stat file, buffer error: %s", strerror(errno));
    return 0.0;
  }

  ret = processx__read_file(path, &buf, /* buffer= */ 2048);
  if (ret == -1) {
    warning("Cannot parse stat file, cannot read file: %s", strerror(errno));
    return 0.0;
  }
  /* This removed the last character, but that's a \n anyway.
     At least we have a zero terminated string... */
  *(buf + ret - 1) = '\0';

  /* Find the first '(' and last ')', that's the end of the command */
  l = strchr(buf, '(');
  r = strrchr(buf, ')');
  if (!l || !r) {
    return 0.0;
  }

  *r = '\0';

  ret = sscanf(r+2,
    "%c %d %d %d %d %d %u %lu %lu %lu %lu %lu %lu %ld %ld %ld %ld %ld %ld %llu",
    state, &ppid, &pgrp, &session, &tty_nr, &tpgid, &flags, &minflt,
    &cminflt, &majflt, &cmajflt, &utime, &stime, &cutime, &cstime, &priority,
    &nice, &num_threads, &itrealvalue, &starttime);

  if (ret == -1) {
    warning("Cannot parse stat file, parse error: %s", strerror(errno));
    return 0.0;
  } else if (ret != 20) {
    warning("Cannot parse stat file, unknown parse error.", strerror(errno));
    return 0.0;
  }

  return starttime;
}

void *processx__memmem(const void *haystack, size_t n1,
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

double processx__boot_time(void) {
  return processx__linux_boot_time;
}

static double processx__linux_clock_period = 0.0;

double processx__create_time(long pid) {
  double ct;
  double bt;
  double clock;

  ct = processx__create_time_since_boot(pid);
  if (ct == 0) return 0.0;

  bt = processx__boot_time();
  if (bt == 0) return 0.0;

  /* Query if not yet queried */
  if (processx__linux_clock_period == 0) {
    clock = sysconf(_SC_CLK_TCK);
    if (clock == -1) return 0.0;
    processx__linux_clock_period = 1.0 / clock;
  }

  return bt + ct * processx__linux_clock_period;
}

#endif

#ifdef __APPLE__

#include <signal.h>
#include <sys/types.h>
#include <sys/sysctl.h>

#define PROCESSX__TV2DOUBLE(t) ((t).tv_sec + (t).tv_usec / 1000000.0)

double processx__create_time(long pid) {
  struct kinfo_proc kp;
  int mib[4];
  size_t len;
  mib[0] = CTL_KERN;
  mib[1] = KERN_PROC;
  mib[2] = KERN_PROC_PID;
  mib[3] = (pid_t) pid;

  len = sizeof(struct kinfo_proc);

  if (sysctl(mib, 4, &kp, &len, NULL, 0) == -1) return 0.0;

  /* Happens if process is gone already */
  if (len == 0) return 0.0;

  return PROCESSX__TV2DOUBLE(kp.kp_proc.p_starttime);
}

#endif

#ifndef _WIN32
#ifndef __linux__
#ifndef __APPLE__

double processx__create_time(long pid) {
  return 0;
}

#endif
#endif
#endif

SEXP processx_create_time(SEXP r_pid) {
  long pid = INTEGER(r_pid)[0];
#ifdef _WIN32
  DWORD dwDesiredAccess = PROCESS_QUERY_INFORMATION | PROCESS_VM_READ;
  HANDLE process = OpenProcess(dwDesiredAccess, FALSE, pid);
  double ct = processx__create_time(process);
  CloseHandle(process);
  return ScalarReal(ct);
#else
  return ScalarReal(processx__create_time(pid));
#endif
}

SEXP processx__proc_start_time(SEXP status) {
  processx_handle_t *handle = R_ExternalPtrAddr(status);

  if (!handle) {
    R_THROW_ERROR("Internal processx error, handle already removed");
  }

  return ScalarReal(handle->create_time);
}
