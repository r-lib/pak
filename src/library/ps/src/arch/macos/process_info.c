
#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif

#include <assert.h>
#include <errno.h>
#include <limits.h>  // for INT_MAX
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <sys/sysctl.h>
#include <libproc.h>
#include <string.h>

#include "process_info.h"
#include "../../common.h"
#include "../../posix.h"

/*
 * Returns a list of all BSD processes on the system.  This routine
 * allocates the list and puts it in *procList and a count of the
 * number of entries in *procCount.  You are responsible for freeing
 * this list (use "free" from System framework).
 * On success, the function returns 0.
 * On error, the function returns a BSD errno value.
 */
int ps__get_proc_list(kinfo_proc **procList, size_t *procCount) {
  int mib3[3] = { CTL_KERN, KERN_PROC, KERN_PROC_ALL };
  size_t size, size2;
  void *ptr;
  int err;
  int lim = 8;  // some limit

  assert( procList != NULL);
  assert(*procList == NULL);
  assert(procCount != NULL);

  *procCount = 0;

  /*
   * We start by calling sysctl with ptr == NULL and size == 0.
   * That will succeed, and set size to the appropriate length.
   * We then allocate a buffer of at least that size and call
   * sysctl with that buffer.  If that succeeds, we're done.
   * If that call fails with ENOMEM, we throw the buffer away
   * and try again.
   * Note that the loop calls sysctl with NULL again.  This is
   * is necessary because the ENOMEM failure case sets size to
   * the amount of data returned, not the amount of data that
   * could have been returned.
   */
  while (lim-- > 0) {
    size = 0;
    if (sysctl((int *)mib3, 3, NULL, &size, NULL, 0) == -1)
      return errno;
    size2 = size + (size >> 3);  // add some
    if (size2 > size) {
      ptr = malloc(size2);
      if (ptr == NULL)
	ptr = malloc(size);
      else
	size = size2;
    }
    else {
      ptr = malloc(size);
    }
    if (ptr == NULL)
      return ENOMEM;

    if (sysctl((int *)mib3, 3, ptr, &size, NULL, 0) == -1) {
      err = errno;
      free(ptr);
      if (err != ENOMEM)
	return err;
    }
    else {
      *procList = (kinfo_proc *)ptr;
      *procCount = size / sizeof(kinfo_proc);
      return 0;
    }
  }
  return ENOMEM;
}


// Read the maximum argument size for processes
int ps__get_argmax(void) {
  int argmax;
  int mib[] = { CTL_KERN, KERN_ARGMAX };
  size_t size = sizeof(argmax);

  if (sysctl(mib, 2, &argmax, &size, NULL, 0) == 0)
    return argmax;
  return 0;
}

// return process args as a character vector
SEXP ps__get_cmdline(long pid) {
  int mib[3];
  int nargs;
  int idx;
  size_t len;
  char *procargs = NULL;
  char *arg_ptr;
  char *arg_end;
  char *curr_arg;
  size_t argmax;

  SEXP arg = R_NilValue;
  SEXP retlist = R_NilValue;

  // special case for PID 0 (kernel_task) where cmdline cannot be fetched
  if (pid == 0) {
    ps__access_denied("");
    return R_NilValue;
  }

  // read argmax and allocate memory for argument space.
  argmax = ps__get_argmax();
  if (! argmax) {
    ps__set_error_from_errno();
    return R_NilValue;
  }

  procargs = (char *) malloc(argmax);
  if (NULL == procargs) {
    ps__no_memory("");
    return R_NilValue;
  }

  PROTECT_PTR(procargs);

  // read argument space
  mib[0] = CTL_KERN;
  mib[1] = KERN_PROCARGS2;
  mib[2] = (pid_t)pid;
  if (sysctl(mib, 3, procargs, &argmax, NULL, 0) < 0) {
    // In case of zombie process we'll get EINVAL, we fix this.
    if (errno == EINVAL) errno = ESRCH;
    UNPROTECT(1);
    ps__set_error_from_errno();
    return R_NilValue;
  }

  arg_end = &procargs[argmax];
  // copy the number of arguments to nargs
  memcpy(&nargs, procargs, sizeof(nargs));

  arg_ptr = procargs + sizeof(nargs);
  len = strlen(arg_ptr);
  arg_ptr += len + 1;

  if (arg_ptr == arg_end) {
    UNPROTECT(1);
    return allocVector(STRSXP, 0);
  }

  // skip ahead to the first argument
  for (; arg_ptr < arg_end; arg_ptr++) {
    if (*arg_ptr != '\0')
      break;
  }

  // iterate through arguments
  curr_arg = arg_ptr;
  idx = 0;
  PROTECT(retlist = allocVector(STRSXP, nargs));
  while (arg_ptr < arg_end && nargs > 0) {
    if (*arg_ptr++ == '\0') {
      PROTECT(arg = ps__str_to_utf8(curr_arg));
      SET_STRING_ELT(retlist, idx++, STRING_ELT(arg, 0));
      UNPROTECT(1);
      // iterate to next arg and decrement # of args
      curr_arg = arg_ptr;
      nargs--;
    }
  }

  UNPROTECT(2);
  return retlist;
}

// return process environment as a character vector
SEXP ps__get_environ(long pid) {
  int mib[3];
  int nargs, nenv;
  char *procargs = NULL;
  char *arg_ptr;
  char *arg_end;
  char *env_start;
  size_t argmax;
  SEXP ret = NULL;

  // special case for PID 0 (kernel_task) where cmdline cannot be fetched
  if (pid == 0) {
    ps__access_denied("");
    goto ret;
  }

  // read argmax and allocate memory for argument space.
  argmax = ps__get_argmax();
  if (! argmax) {
    ps__set_error_from_errno();
    return R_NilValue;
  }

  procargs = (char *) malloc(argmax);
  if (NULL == procargs) {
    ps__no_memory("");
    return R_NilValue;
  }

  PROTECT_PTR(procargs);

  // read argument space
  mib[0] = CTL_KERN;
  mib[1] = KERN_PROCARGS2;
  mib[2] = (pid_t)pid;
  if (sysctl(mib, 3, procargs, &argmax, NULL, 0) < 0) {
    // In case of zombie process we'll get EINVAL, fix this.
    if (errno == EINVAL) errno = ESRCH;
    UNPROTECT(1);
    ps__set_error_from_errno();
    return R_NilValue;
  }

  arg_end = &procargs[argmax];
  // copy the number of arguments to nargs
  memcpy(&nargs, procargs, sizeof(nargs));

  // skip executable path
  arg_ptr = procargs + sizeof(nargs);
  arg_ptr = memchr(arg_ptr, '\0', arg_end - arg_ptr);

  if (arg_ptr == NULL || arg_ptr == arg_end)
    goto empty;

  // skip ahead to the first argument
  for (; arg_ptr < arg_end; arg_ptr++) {
    if (*arg_ptr != '\0')
      break;
  }

  // iterate through arguments
  while (arg_ptr < arg_end && nargs > 0) {
    if (*arg_ptr++ == '\0')
      nargs--;
  }

  // build an environment variable block
  env_start = arg_ptr;

  /* Count the number of env vars first */
  nenv = 0;
  while (*arg_ptr != '\0' && arg_ptr < arg_end) {
    char *s = memchr(arg_ptr + 1, '\0', arg_end - arg_ptr);

    if (s == NULL)
      break;

    nenv++;
    arg_ptr = s + 1;
  }

  PROTECT(ret = allocVector(STRSXP, nenv));
  arg_ptr = env_start;
  nenv = 0;
  while (*arg_ptr != '\0' && arg_ptr < arg_end) {
    char *s = memchr(arg_ptr + 1, '\0', arg_end - arg_ptr);

    if (s == NULL)
      break;

    SET_STRING_ELT(ret, nenv++, Rf_mkCharLen(arg_ptr, (int)(s - arg_ptr)));

    arg_ptr = s + 1;
  }

  UNPROTECT(2);
  return ret;

 empty:
  UNPROTECT(1);

 ret:
  return allocVector(STRSXP, 0);
}


int ps__get_kinfo_proc(long pid, struct kinfo_proc *kp) {
  int mib[4];
  size_t len;
  mib[0] = CTL_KERN;
  mib[1] = KERN_PROC;
  mib[2] = KERN_PROC_PID;
  mib[3] = (pid_t) pid;

  // fetch the info with sysctl()
  len = sizeof(struct kinfo_proc);

  // now read the data from sysctl
  if (sysctl(mib, 4, kp, &len, NULL, 0) == -1) {
    // raise an exception and throw errno as the error
    ps__set_error_from_errno();
    return -1;
  }

  // sysctl succeeds but len is zero, happens when process has gone away
  if (len == 0) {
    ps__no_such_process(pid, 0);
    return -1;
  }
  return 0;
}


/*
 * A wrapper around proc_pidinfo().
 * Returns 0 on failure (and error gets already set).
 */
int ps__proc_pidinfo(long pid, int flavor, uint64_t arg, void *pti, int size) {
  errno = 0;
  int ret = proc_pidinfo((int)pid, flavor, arg, pti, size);
  if ((ret <= 0) || ((unsigned long)ret < sizeof(pti))) {
    ps__raise_for_pid(pid, "proc_pidinfo()");
    return 0;
  }
  return ret;
}
