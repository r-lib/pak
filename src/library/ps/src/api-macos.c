
#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif

#include <stdlib.h>
#include <unistd.h>
#include <sys/sysctl.h>
#include <sys/proc_info.h>
#include <sys/types.h>
#include <libproc.h>
#include <errno.h>
#include <sys/errno.h>
#include <string.h>
#include <utmpx.h>
#include <arpa/inet.h>
#include <sys/param.h>
#include <sys/mount.h>

#include <mach/mach.h>
#include <mach/mach_vm.h>
#include <mach/shared_region.h>
#include <mach/mach_time.h>

#include <sys/event.h>
#include <sys/time.h>

#include "ps-internal.h"
#include "arch/macos/process_info.h"
#include "cleancall.h"

#include <stdbool.h>

#if defined(__i386__) || defined(__ppc__)
#define statfs statfs64
#define getfsstat getfsstat64
#endif

struct mach_timebase_info PS_MACH_TIMEBASE_INFO;

#define PS__TV2DOUBLE(t) ((t).tv_sec + (t).tv_usec / 1000000.0)

#define PS__CHECK_KINFO(kp, handle)				      \
  if (PS__TV2DOUBLE(kp.kp_proc.p_starttime) != handle->create_time) { \
    ps__no_such_process(handle->pid, 0);			      \
    ps__throw_error();						      \
  }

#define PS__CHECK_HANDLE(handle)			\
  do {							\
    struct kinfo_proc kp;				\
    if (ps__get_kinfo_proc(handle->pid, &kp) == -1) {	\
      ps__set_error_from_errno();			\
      ps__throw_error();				\
    }							\
    PS__CHECK_KINFO(kp, handle);			\
  } while (0)

#define PS__GET_STATUS(pid, stat, result, error)		\
  switch(stat) {					\
  case SIDL:   result = mkString("idle");     break;	\
  case SRUN:   result = ps__get_status(pid);  break;	\
  case SSLEEP: result = mkString("sleeping"); break;	\
  case SSTOP:  result = mkString("stopped");  break;	\
  case SZOMB:  result = mkString("zombie");   break;	\
  default:     error;					\
  }

// this is a non-API alternative for task_for_pid(). ps uses this:
// https://github.com/apple-oss-distributions/adv_cmds/blob/8744084ea0ff41ca4bb96b0f9c22407d0e48e9b7/ps/tasks.c#L28
// but it seems that it does not actually change anything, and we still
// cannot get the tesk for most processes, so no need to use it right now
// extern kern_return_t task_read_for_pid(
// 	mach_port_name_t target_tport,
// 	int pid,
// 	mach_port_name_t *t);

SEXP ps__get_status(long pid) {
  /*
   * Scan threads for process state information.
   * Based on:
   * http://stackoverflow.com/questions/6788274/ios-mac-cpu-usage-for-thread and
   * https://github.com/max-horvath/htop-osx/blob/e86692e869e30b0bc7264b3675d2a4014866ef46/ProcessList.c
   */

  kern_return_t ret;
  task_t port;
  ret = task_for_pid(mach_task_self(), pid, &port);
  if (ret != KERN_SUCCESS) {
    return Rf_ScalarString(NA_STRING);
  }

  task_info_data_t tinfo;
  mach_msg_type_number_t task_info_count = TASK_INFO_MAX;
  ret = task_info(
    port,
    TASK_BASIC_INFO,
    (task_info_t) tinfo,
    &task_info_count
  );
  if (ret != KERN_SUCCESS) {
    return Rf_ScalarString(NA_STRING);
  }

  thread_array_t thread_list;
  mach_msg_type_number_t thread_count;
  ret = task_threads(port, &thread_list, &thread_count);
  if (ret != KERN_SUCCESS) {
    mach_port_deallocate(mach_task_self(), port);
    return Rf_ScalarString(NA_STRING);
  }

  integer_t run_state = 999;
  for (unsigned int i = 0; i < thread_count; i++) {
    thread_info_data_t thinfo;
    mach_msg_type_number_t thread_info_count = THREAD_BASIC_INFO_COUNT;
    ret = thread_info(
      thread_list[i],
      THREAD_BASIC_INFO,
      (thread_info_t) thinfo,
      &thread_info_count
    );
    if (ret == KERN_SUCCESS) {
      thread_basic_info_t basic_info_th = (thread_basic_info_t)thinfo;
      if (basic_info_th->run_state < run_state) {
        run_state = basic_info_th->run_state;
      }
      mach_port_deallocate(mach_task_self(), thread_list[i]);
    }
  }
  vm_deallocate(
    mach_task_self(),
    (vm_address_t)thread_list,
    sizeof(thread_port_array_t) * thread_count
  );
  mach_port_deallocate(mach_task_self(), port);

  switch (run_state) {
  case TH_STATE_RUNNING:
    return mkString("running");
  case TH_STATE_STOPPED:
    return mkString("stopped");
  case TH_STATE_WAITING:
    return mkString("sleeping");
  case TH_STATE_UNINTERRUPTIBLE:
    return mkString("uninterruptible");
  case TH_STATE_HALTED:
    return mkString("halted");
  default:
    return Rf_ScalarString(NA_STRING);
  }
}

void ps__check_for_zombie(ps_handle_t *handle, int err) {
  struct kinfo_proc kp;
  int ret;

  if (handle->pid == 0) {
    ps__access_denied("");
    err = 1;

  } else if (errno == 0 || errno == ESRCH) {

    ret = ps__get_kinfo_proc(handle->pid, &kp);
    if ((ret == -1) ||
	(PS__TV2DOUBLE(kp.kp_proc.p_starttime) != handle->create_time)) {
      ps__no_such_process(handle->pid, 0);
      err = 1;
    } else if (kp.kp_proc.p_stat == SZOMB) {
      ps__zombie_process(handle->pid);
      err = 1;
    } else {
      ps__access_denied("");
    }

  } else {
    ps__set_error_from_errno();
  }

  if (err) ps__throw_error();
}

void psll_finalizer(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  if (handle) free(handle);
}

SEXP psll_handle(SEXP pid, SEXP time) {
  pid_t cpid = isNull(pid) ? getpid() : INTEGER(pid)[0];
  double ctime;
  ps_handle_t *handle;
  SEXP res;

  if (!isNull(time)) {
    ctime = REAL(time)[0];
  } else {
    struct kinfo_proc kp;
    if (ps__get_kinfo_proc(cpid, &kp) == -1) ps__throw_error();
    ctime = (double) PS__TV2DOUBLE(kp.kp_proc.p_starttime);
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
  struct kinfo_proc kp;
  SEXP name, status, result;

  if (!handle) error("Process pointer cleaned up already");

  if (ps__get_kinfo_proc(handle->pid, &kp) == -1) {
    PROTECT(name = mkString("???"));
    PROTECT(status = mkString("terminated"));
  } else {
    PROTECT(name = ps__str_to_utf8(kp.kp_proc.p_comm));
    PS__GET_STATUS(handle->pid, kp.kp_proc.p_stat, status, status = mkString("unknown"));
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
  struct kinfo_proc kp;
  SEXP ppid, parent;

  if (!handle) error("Process pointer cleaned up already");

  if (ps__get_kinfo_proc(handle->pid, &kp) == -1) ps__throw_error();
  PS__CHECK_KINFO(kp, handle);

  /* TODO: this is a race condition, because the parent process might
     have just quit, so psll_handle() might fail. If this happens, then
     we should try to query the ppid again. */

  PROTECT(ppid = ScalarInteger(kp.kp_eproc.e_ppid));
  PROTECT(parent = psll_handle(ppid, R_NilValue));

  UNPROTECT(2);
  return parent;
}


SEXP psll_ppid(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  struct kinfo_proc kp;

  if (!handle) error("Process pointer cleaned up already");

  if (ps__get_kinfo_proc(handle->pid, &kp) == -1) ps__throw_error();
  PS__CHECK_KINFO(kp, handle);

  return ScalarInteger(kp.kp_eproc.e_ppid);
}


SEXP psll_is_running(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  struct kinfo_proc kp;
  double ctime;

  if (!handle) error("Process pointer cleaned up already");

  if (handle->gone) return ScalarLogical(0);
  if (ps__get_kinfo_proc(handle->pid, &kp) == -1) return ScalarLogical(0);

  ctime = (double) PS__TV2DOUBLE(kp.kp_proc.p_starttime);
  return ScalarLogical(ctime == handle->create_time);
}


SEXP psll_name(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  struct kinfo_proc kp;

  if (!handle) error("Process pointer cleaned up already");

  if (ps__get_kinfo_proc(handle->pid, &kp) == -1) ps__throw_error();
  PS__CHECK_KINFO(kp, handle);
  return ps__str_to_utf8(kp.kp_proc.p_comm);
}


SEXP psll_exe(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  int ret;
  char buf[PROC_PIDPATHINFO_MAXSIZE];

  if (!handle) error("Process pointer cleaned up already");

  ret = proc_pidpath(handle->pid, &buf, sizeof(buf));

  if (ret == 0) ps__check_for_zombie(handle, 1);

  PS__CHECK_HANDLE(handle);

  return ps__str_to_utf8(buf);
}

SEXP psll_cmdline(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  SEXP result;

  if (!handle) error("Process pointer cleaned up already");

  result = ps__get_cmdline(handle->pid);

  if (isNull(result)) ps__check_for_zombie(handle, 1);

  PROTECT(result);
  PS__CHECK_HANDLE(handle);
  UNPROTECT(1);
  return result;
}


SEXP psll_status(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  struct kinfo_proc kp;
  SEXP result;

  if (!handle) error("Process pointer cleaned up already");

  if (ps__get_kinfo_proc(handle->pid, &kp) == -1) {
    handle->gone = 1;
    ps__no_such_process(handle->pid, 0);
    ps__throw_error();
  }

  PS__CHECK_KINFO(kp, handle);

  PS__GET_STATUS(handle->pid, kp.kp_proc.p_stat, result, error("Unknown process status"));

  return result;
}


SEXP psll_username(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  struct kinfo_proc kp;
  SEXP ruid, pw, result;

  if (!handle) error("Process pointer cleaned up already");

  if (ps__get_kinfo_proc(handle->pid, &kp) == -1) ps__throw_error();

  PS__CHECK_KINFO(kp, handle);

  PROTECT(ruid = ScalarInteger(kp.kp_eproc.e_pcred.p_ruid));
  PROTECT(pw = ps__get_pw_uid(ruid));
  PROTECT(result = VECTOR_ELT(pw, 0));

  UNPROTECT(3);
  return result;
}


SEXP psll_cwd(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);

  if (!handle) error("Process pointer cleaned up already");

  struct proc_vnodepathinfo pathinfo;

  if (ps__proc_pidinfo(handle->pid, PROC_PIDVNODEPATHINFO, 0, &pathinfo,
		       sizeof(pathinfo)) <= 0) {
    ps__check_for_zombie(handle, 1);
  }

  PS__CHECK_HANDLE(handle);
  return ps__str_to_utf8(pathinfo.pvi_cdir.vip_path);
}


SEXP psll_uids(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  struct kinfo_proc kp;
  SEXP result, names;

  if (!handle) error("Process pointer cleaned up already");

  if (ps__get_kinfo_proc(handle->pid, &kp) == -1) ps__throw_error();

  PS__CHECK_KINFO(kp, handle);

  PROTECT(result = allocVector(INTSXP, 3));
  INTEGER(result)[0] = kp.kp_eproc.e_pcred.p_ruid;
  INTEGER(result)[1] = kp.kp_eproc.e_ucred.cr_uid;
  INTEGER(result)[2] = kp.kp_eproc.e_pcred.p_svuid;
  PROTECT(names = ps__build_string("real", "effective", "saved", NULL));
  setAttrib(result, R_NamesSymbol, names);

  UNPROTECT(2);
  return result;
}


SEXP psll_gids(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  struct kinfo_proc kp;
  SEXP result, names;

  if (!handle) error("Process pointer cleaned up already");

  if (ps__get_kinfo_proc(handle->pid, &kp) == -1) ps__throw_error();

  PS__CHECK_KINFO(kp, handle);

  PROTECT(result = allocVector(INTSXP, 3));
  INTEGER(result)[0] = kp.kp_eproc.e_pcred.p_rgid;
  INTEGER(result)[1] = kp.kp_eproc.e_ucred.cr_groups[0];
  INTEGER(result)[2] = kp.kp_eproc.e_pcred.p_svgid;
  PROTECT(names = ps__build_string("real", "effective", "saved", NULL));
  setAttrib(result, R_NamesSymbol, names);

  UNPROTECT(2);
  return result;
}


SEXP psll_terminal(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  struct kinfo_proc kp;

  if (!handle) error("Process pointer cleaned up already");

  if (ps__get_kinfo_proc(handle->pid, &kp) == -1) ps__throw_error();

  PS__CHECK_KINFO(kp, handle);

  if (kp.kp_eproc.e_tdev != -1) {
    return ScalarInteger(kp.kp_eproc.e_tdev);
  } else {
    return ScalarInteger(NA_INTEGER);
  }
}


SEXP psll_environ(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  SEXP result;

  if (!handle) error("Process pointer cleaned up already");

  result = ps__get_environ(handle->pid);

  if (isNull(result)) ps__check_for_zombie(handle, 1);

  PROTECT(result);
  PS__CHECK_HANDLE(handle);
  UNPROTECT(1);
  return result;
}



SEXP psll_num_threads(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  struct proc_taskinfo pti;

  if (!handle) error("Process pointer cleaned up already");

  if (ps__proc_pidinfo(handle->pid, PROC_PIDTASKINFO, 0, &pti,
		       sizeof(pti)) <= 0) {
    ps__check_for_zombie(handle, 1);
  }

  PS__CHECK_HANDLE(handle);

  return ScalarInteger(pti.pti_threadnum);
}


SEXP psll_cpu_times(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  struct proc_taskinfo pti;
  SEXP result, names;
  uint64_t total_user;
  uint64_t total_system;

  if (!handle) error("Process pointer cleaned up already");

  if (ps__proc_pidinfo(handle->pid, PROC_PIDTASKINFO, 0, &pti,
		       sizeof(pti)) <= 0) {
    ps__check_for_zombie(handle, 1);
  }

  PS__CHECK_HANDLE(handle);

  total_user = pti.pti_total_user * PS_MACH_TIMEBASE_INFO.numer;
  total_user /= PS_MACH_TIMEBASE_INFO.denom;
  total_system = pti.pti_total_system * PS_MACH_TIMEBASE_INFO.numer;
  total_system /= PS_MACH_TIMEBASE_INFO.denom;

  PROTECT(result = allocVector(REALSXP, 4));
  REAL(result)[0] = (double) total_user / 1000000000.0;
  REAL(result)[1] = (double) total_system / 1000000000.0;
  REAL(result)[2] = REAL(result)[3] = NA_REAL;
  PROTECT(names = ps__build_string("user", "system", "children_user",
				   "children_system", NULL));
  setAttrib(result, R_NamesSymbol, names);

  UNPROTECT(2);
  return result;
}


SEXP psll_memory_info(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  struct proc_taskinfo pti;
  SEXP result, names;

  if (!handle) error("Process pointer cleaned up already");

  if (ps__proc_pidinfo(handle->pid, PROC_PIDTASKINFO, 0, &pti,
		       sizeof(pti)) <= 0) {
    ps__check_for_zombie(handle, 1);
  }

  PS__CHECK_HANDLE(handle);

  PROTECT(result = allocVector(REALSXP, 4));
  REAL(result)[0] = (double) pti.pti_resident_size;
  REAL(result)[1] = (double) pti.pti_virtual_size;
  REAL(result)[2] = (double) pti.pti_faults;
  REAL(result)[3] = (double) pti.pti_pageins;
  PROTECT(names = ps__build_string("rss", "vms", "pfaults", "pageins", NULL));
  setAttrib(result, R_NamesSymbol, names);

  UNPROTECT(2);
  return result;
}

SEXP ps__boot_time(void) {
#define MIB_SIZE 2
  int mib[MIB_SIZE];
  size_t size;
  struct timeval boottime;
  double unixtime = 0.0;

  mib[0] = CTL_KERN;
  mib[1] = KERN_BOOTTIME;
  size = sizeof(boottime);
  if (sysctl(mib, MIB_SIZE, &boottime, &size, NULL, 0) != -1)  {
    unixtime = boottime.tv_sec + boottime.tv_usec / 1.e6;
  } else {
    ps__set_error_from_errno();
    ps__throw_error();
  }

  return ScalarReal(unixtime);
}

SEXP ps__cpu_count_logical(void) {
  int num = 0;
  size_t size = sizeof(int);

  if (sysctlbyname("hw.logicalcpu", &num, &size, NULL, 2))
    return ScalarInteger(NA_INTEGER);
  else
    return ScalarInteger(num);
}

SEXP ps__cpu_count_physical(void) {
  int num = 0;
  size_t size = sizeof(int);

  if (sysctlbyname("hw.physicalcpu", &num, &size, NULL, 0))
    return ScalarInteger(NA_INTEGER);
  else
    return ScalarInteger(num);
}

SEXP ps__find_if_env(SEXP marker, SEXP after, SEXP pid) {
  const char *cmarker = CHAR(STRING_ELT(marker, 0));
  pid_t cpid = INTEGER(pid)[0];
  SEXP env;
  size_t i, len;
  SEXP phandle;
  ps_handle_t *handle;

  PROTECT(phandle = psll_handle(pid, R_NilValue));
  handle = R_ExternalPtrAddr(phandle);

  PROTECT(env = ps__get_environ(cpid));
  if (isNull(env)) {
    ps__set_error_from_errno();
    ps__throw_error();
  }

  len = LENGTH(env);

  for (i = 0; i < len; i++) {
    if (strstr(CHAR(STRING_ELT(env, i)), cmarker)) {
      UNPROTECT(2);
      PS__CHECK_HANDLE(handle);
      return phandle;
    }
  }

  UNPROTECT(2);
  return R_NilValue;
}

SEXP psll_num_fds(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  struct proc_fdinfo *fds_pointer;
  int pidinfo_result;
  int num;
  pid_t pid;

  if (!handle) error("Process pointer cleaned up already");

  pid = handle->pid;

  pidinfo_result = proc_pidinfo(pid, PROC_PIDLISTFDS, 0, NULL, 0);
  if (pidinfo_result <= 0) ps__check_for_zombie(handle, 1);

  fds_pointer = malloc(pidinfo_result);
  if (fds_pointer == NULL) {
    ps__no_memory("");
    ps__throw_error();
  }

  pidinfo_result = proc_pidinfo(pid, PROC_PIDLISTFDS, 0, fds_pointer,
				pidinfo_result);

  if (pidinfo_result <= 0) {
    free(fds_pointer);
    ps__check_for_zombie(handle, 1);
  }

  num = (pidinfo_result / PROC_PIDLISTFD_SIZE);
  free(fds_pointer);

  PS__CHECK_HANDLE(handle);

  return ScalarInteger(num);
}

SEXP psll_open_files(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);

  long pid;
  int pidinfo_result;
  int iterations;
  int i;
  unsigned long nb;

  struct proc_fdinfo *fds_pointer = NULL;
  struct proc_fdinfo *fdp_pointer;
  struct vnode_fdinfowithpath vi;

  SEXP result;

  if (!handle) error("Process pointer cleaned up already");

  pid = handle->pid;

  pidinfo_result = ps__proc_pidinfo(pid, PROC_PIDLISTFDS, 0, NULL, 0);
  if (pidinfo_result <= 0) goto error;

  fds_pointer = malloc(pidinfo_result);
  if (fds_pointer == NULL) {
    ps__no_memory("");
    goto error;
  }
  pidinfo_result = ps__proc_pidinfo(
    pid, PROC_PIDLISTFDS, 0, fds_pointer, pidinfo_result);

  if (pidinfo_result <= 0) goto error;

  iterations = (pidinfo_result / PROC_PIDLISTFD_SIZE);

  PROTECT(result = allocVector(VECSXP, iterations));

  for (i = 0; i < iterations; i++) {
    fdp_pointer = &fds_pointer[i];

    if (fdp_pointer->proc_fdtype == PROX_FDTYPE_VNODE) {
      errno = 0;
      nb = proc_pidfdinfo((pid_t)pid,
			  fdp_pointer->proc_fd,
			  PROC_PIDFDVNODEPATHINFO,
			  &vi,
			  sizeof(vi));

      // --- errors checking
      if ((nb <= 0) || nb < sizeof(vi)) {
	if ((errno == ENOENT) || (errno == EBADF)) {
	  // no such file or directory or bad file descriptor;
	  // let's assume the file has been closed or removed
	  continue;
	} else {
	  ps__set_error(
	    "proc_pidinfo(PROC_PIDFDVNODEPATHINFO) failed for %d", (int) pid);
	  goto error;
	}
      }
      // --- /errors checking

      SET_VECTOR_ELT(
	result, i,
	ps__build_list("si", vi.pvip.vip_path, (int) fdp_pointer->proc_fd));
    }
  }

  free(fds_pointer);

  PS__CHECK_HANDLE(handle);

  UNPROTECT(1);
  return result;

 error:
  if (fds_pointer != NULL) free(fds_pointer);
  ps__check_for_zombie(handle, 1);
  return R_NilValue;
}

SEXP psll_connections(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  long pid;
  int pidinfo_result;
  int iterations;
  int i;
  unsigned long nb;

  struct proc_fdinfo *fds_pointer = NULL;
  struct proc_fdinfo *fdp_pointer;
  struct socket_fdinfo si;

  SEXP result;

  if (!handle) error("Process pointer cleaned up already");

  pid = handle->pid;

  if (pid == 0) return allocVector(VECSXP, 0);

  pidinfo_result = ps__proc_pidinfo(pid, PROC_PIDLISTFDS, 0, NULL, 0);
  if (pidinfo_result <= 0) goto error;

  fds_pointer = malloc(pidinfo_result);
  if (fds_pointer == NULL) {
    ps__no_memory("");
    ps__throw_error();
  }

  pidinfo_result = ps__proc_pidinfo(pid, PROC_PIDLISTFDS, 0, fds_pointer,
				    pidinfo_result);
  if (pidinfo_result <= 0) goto error;

  iterations = (pidinfo_result / PROC_PIDLISTFD_SIZE);
  PROTECT(result = allocVector(VECSXP, iterations));
  for (i = 0; i < iterations; i++) {
    fdp_pointer = &fds_pointer[i];

    if (fdp_pointer->proc_fdtype == PROX_FDTYPE_SOCKET) {
      errno = 0;
      nb = proc_pidfdinfo((pid_t)pid, fdp_pointer->proc_fd,
			  PROC_PIDFDSOCKETINFO, &si, sizeof(si));

      // --- errors checking
      if ((nb <= 0) || (nb < sizeof(si))) {
	if (errno == EBADF) {
	  // let's assume socket has been closed
	  continue;
	} else {
	  ps__set_error("proc_pidinfo(PROC_PIDFDSOCKETINFO) failed for %d",
			(int) pid);
	  goto error;
	}
      }
      // --- /errors checking

      //
      int fd, family, type, lport, rport, state;
      char lip[512], rip[512];

      SEXP tuple;

      fd = (int)fdp_pointer->proc_fd;
      family = si.psi.soi_family;
      type = si.psi.soi_type;

      if ((family == AF_INET) || (family == AF_INET6)) {
	if (family == AF_INET) {
	  inet_ntop(AF_INET,
		    &si.psi.soi_proto.pri_tcp.tcpsi_ini.	\
		    insi_laddr.ina_46.i46a_addr4,
		    lip,
		    sizeof(lip));
	  inet_ntop(AF_INET,
		    &si.psi.soi_proto.pri_tcp.tcpsi_ini.insi_faddr.	\
		    ina_46.i46a_addr4,
		    rip,
		    sizeof(rip));
	} else {
	  inet_ntop(AF_INET6,
		    &si.psi.soi_proto.pri_tcp.tcpsi_ini.	\
		    insi_laddr.ina_6,
		    lip, sizeof(lip));
	  inet_ntop(AF_INET6,
		    &si.psi.soi_proto.pri_tcp.tcpsi_ini.	\
		    insi_faddr.ina_6,
		    rip, sizeof(rip));
	}

	// check for inet_ntop failures
	if (errno != 0) {
	  ps__set_error_from_errno();
	  goto error;
	}

	lport = ntohs(si.psi.soi_proto.pri_tcp.tcpsi_ini.insi_lport);
	rport = ntohs(si.psi.soi_proto.pri_tcp.tcpsi_ini.insi_fport);
	if (type == SOCK_STREAM)
	  state = (int)si.psi.soi_proto.pri_tcp.tcpsi_state;
	else
	  state = NA_INTEGER;

	// construct the python list
	PROTECT(tuple = ps__build_list("iiisisii", fd, family, type,
				       lip, lport, rip, rport,state));
	SET_VECTOR_ELT(result, i, tuple);
	UNPROTECT(1);

      } else if (family == AF_UNIX) {
	SEXP laddr, raddr;
	PROTECT(laddr = ps__str_to_utf8(si.psi.soi_proto.pri_un.unsi_addr.ua_sun.sun_path));
	PROTECT(raddr = ps__str_to_utf8(si.psi.soi_proto.pri_un.unsi_caddr.ua_sun.sun_path));

	// construct the python list
	PROTECT(tuple = ps__build_list("iiiOiOii", fd, family, type,
				       laddr, 0, raddr, 0, NA_INTEGER));

	SET_VECTOR_ELT(result, i, tuple);
	UNPROTECT(3);
      }
    }
  }

  free(fds_pointer);

  PS__CHECK_HANDLE(handle);

  UNPROTECT(1);
  return result;

 error:
  if (fds_pointer) free(fds_pointer);
  ps__check_for_zombie(handle, 1);
  return R_NilValue;
}

/*
 * Indicates if the given virtual address on the given architecture is in the
 * shared VM region.
 */
static bool ps_in_shared_region(mach_vm_address_t addr, cpu_type_t type) {
  mach_vm_address_t base;
  mach_vm_address_t size;

  switch (type) {
  case CPU_TYPE_ARM:
    base = SHARED_REGION_BASE_ARM;
    size = SHARED_REGION_SIZE_ARM;
    break;
  case CPU_TYPE_I386:
    base = SHARED_REGION_BASE_I386;
    size = SHARED_REGION_SIZE_I386;
    break;
  case CPU_TYPE_X86_64:
    base = SHARED_REGION_BASE_X86_64;
    size = SHARED_REGION_SIZE_X86_64;
    break;
  case CPU_TYPE_POWERPC:
    base = SHARED_REGION_BASE_PPC;
    size = SHARED_REGION_SIZE_PPC;
    break;
  case CPU_TYPE_POWERPC64:
    base = SHARED_REGION_BASE_PPC64;
    size = SHARED_REGION_SIZE_PPC64;
    break;
  default:
    return false;
  }

  return base <= addr && addr < (base + size);
}

/*
 * Returns the USS (unique set size) of the process. Reference:
 * https://dxr.mozilla.org/mozilla-central/source/xpcom/base/
 *     nsMemoryReporterManager.cpp
 */

SEXP psll_memory_uss(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  long pid;
  size_t len;
  cpu_type_t cpu_type;
  size_t private_pages = 0;
  mach_vm_size_t size = 0;
  mach_msg_type_number_t info_count = VM_REGION_TOP_INFO_COUNT;
  kern_return_t kr;
  long pagesize = getpagesize();
  mach_vm_address_t addr = MACH_VM_MIN_ADDRESS;
  mach_port_t task = MACH_PORT_NULL;
  vm_region_top_info_data_t info;
  mach_port_t object_name;

  if (!handle) error("Process pointer cleaned up already");
  pid = handle->pid;

  kern_return_t err = KERN_SUCCESS;
  err = task_for_pid(mach_task_self(), pid, &task);
  if (err != KERN_SUCCESS) {
    ps__check_for_zombie(handle, 1);
    PS__CHECK_HANDLE(handle);
    ps__set_error("Access denied for task_for_pid() for %d", (int) pid);
    ps__throw_error();
  }

  len = sizeof(cpu_type);
  if (sysctlbyname("sysctl.proc_cputype", &cpu_type, &len, NULL, 0) != 0) {
    ps__set_error_from_errno();
    ps__throw_error();
  }

  // Roughly based on libtop_update_vm_regions in
  // http://www.opensource.apple.com/source/top/top-100.1.2/libtop.c
  for (addr = 0; ; addr += size) {
    kr = mach_vm_region(
      task, &addr, &size, VM_REGION_TOP_INFO, (vm_region_info_t)&info,
      &info_count, &object_name);

    if (kr == KERN_INVALID_ADDRESS) {
      // Done iterating VM regions.
      break;

    } else if (kr != KERN_SUCCESS) {
      ps__set_error(
        "mach_vm_region(VM_REGION_TOP_INFO) syscall failed for %d",
        (int) pid
      );
      ps__throw_error();
    }

    if (ps_in_shared_region(addr, cpu_type) &&
        info.share_mode != SM_PRIVATE) {
      continue;
    }

    switch (info.share_mode) {
#ifdef SM_LARGE_PAGE
    case SM_LARGE_PAGE:
      // NB: Large pages are not shareable and always resident.
#endif
    case SM_PRIVATE:
      private_pages += info.private_pages_resident;
      private_pages += info.shared_pages_resident;
      break;
    case SM_COW:
      private_pages += info.private_pages_resident;
      if (info.ref_count == 1) {
        // Treat copy-on-write pages as private if they only
        // have one reference.
        private_pages += info.shared_pages_resident;
      }
      break;
    case SM_SHARED:
    default:
      break;
    }
  }

  mach_port_deallocate(mach_task_self(), task);
  return Rf_ScalarInteger(private_pages * pagesize);
}

SEXP ps__users(void) {
  struct utmpx *utx;
  SEXP result;
  PROTECT_INDEX pidx;
  int len = 10, num = 0;

  PROTECT_WITH_INDEX(result = allocVector(VECSXP, len), &pidx);

  while ((utx = getutxent()) != NULL) {

    if (utx->ut_type != USER_PROCESS) continue;

    if (++num == len) {
      len *= 2;
      REPROTECT(result = Rf_lengthgets(result, len), pidx);
    }
    SET_VECTOR_ELT(
      result, num,
      ps__build_list("sssdi", utx->ut_user, utx->ut_line, utx->ut_host,
		     (double) PS__TV2DOUBLE(utx->ut_tv), utx->ut_pid));
  }

  endutxent();
  UNPROTECT(1);
  return result;
}

SEXP ps__disk_partitions(SEXP all) {
  int num;
  int i;
  int len;
  uint64_t flags;
  char opts[400];
  struct statfs *fs = NULL;
  SEXP result;

  // get the number of mount points
  num = getfsstat(NULL, 0, MNT_NOWAIT);
  if (num == -1) {
    ps__set_error_from_errno();
    goto error;
  }

  len = sizeof(*fs) * num;
  fs = malloc(len);
  if (fs == NULL) {
    ps__no_memory("");
    goto error;
  }

  num = getfsstat(fs, len, MNT_NOWAIT);
  if (num == -1) {
    ps__set_error_from_errno();
    goto error;
  }

  PROTECT(result = allocVector(VECSXP, num));

  for (i = 0; i < num; i++) {
    opts[0] = 0;
    flags = fs[i].f_flags;

    // see sys/mount.h
    if (flags & MNT_RDONLY)
      strlcat(opts, "ro", sizeof(opts));
    else
      strlcat(opts, "rw", sizeof(opts));
    if (flags & MNT_SYNCHRONOUS)
      strlcat(opts, ",sync", sizeof(opts));
    if (flags & MNT_NOEXEC)
      strlcat(opts, ",noexec", sizeof(opts));
    if (flags & MNT_NOSUID)
      strlcat(opts, ",nosuid", sizeof(opts));
    if (flags & MNT_UNION)
      strlcat(opts, ",union", sizeof(opts));
    if (flags & MNT_ASYNC)
      strlcat(opts, ",async", sizeof(opts));
    if (flags & MNT_EXPORTED)
      strlcat(opts, ",exported", sizeof(opts));
    if (flags & MNT_QUARANTINE)
      strlcat(opts, ",quarantine", sizeof(opts));
    if (flags & MNT_LOCAL)
      strlcat(opts, ",local", sizeof(opts));
    if (flags & MNT_QUOTA)
      strlcat(opts, ",quota", sizeof(opts));
    if (flags & MNT_ROOTFS)
      strlcat(opts, ",rootfs", sizeof(opts));
    if (flags & MNT_DOVOLFS)
      strlcat(opts, ",dovolfs", sizeof(opts));
    if (flags & MNT_DONTBROWSE)
      strlcat(opts, ",dontbrowse", sizeof(opts));
    if (flags & MNT_IGNORE_OWNERSHIP)
      strlcat(opts, ",ignore-ownership", sizeof(opts));
    if (flags & MNT_AUTOMOUNTED)
      strlcat(opts, ",automounted", sizeof(opts));
    if (flags & MNT_JOURNALED)
      strlcat(opts, ",journaled", sizeof(opts));
    if (flags & MNT_NOUSERXATTR)
      strlcat(opts, ",nouserxattr", sizeof(opts));
    if (flags & MNT_DEFWRITE)
      strlcat(opts, ",defwrite", sizeof(opts));
    if (flags & MNT_MULTILABEL)
      strlcat(opts, ",multilabel", sizeof(opts));
    if (flags & MNT_NOATIME)
      strlcat(opts, ",noatime", sizeof(opts));
    if (flags & MNT_UPDATE)
      strlcat(opts, ",update", sizeof(opts));
    if (flags & MNT_RELOAD)
      strlcat(opts, ",reload", sizeof(opts));
    if (flags & MNT_FORCE)
      strlcat(opts, ",force", sizeof(opts));
    if (flags & MNT_CMDFLAGS)
      strlcat(opts, ",cmdflags", sizeof(opts));

    SET_VECTOR_ELT(
      result, i,
      ps__build_list("ssss", fs[i].f_mntfromname, fs[i].f_mntonname,
                     fs[i].f_fstypename, opts));
  }

  free(fs);
  UNPROTECT(1);
  return result;

error:
  if (fs != NULL) free(fs);
  ps__throw_error();
  return R_NilValue;
}

SEXP ps__fs_info(SEXP path, SEXP abspath, SEXP mps) {
  struct statfs sfs;
  R_xlen_t i, len = Rf_xlength(path);

  const char *nms[] = {
    "path",                         // 0
    "mountpoint",                   // 1
    "name",                         // 2
    "type",                         // 3
    "block_size",                   // 4
    "transfer_block_size",          // 5
    "total_data_blocks",            // 6
    "free_blocks",                  // 7
    "free_blocks_non_superuser",    // 8
    "total_nodes",                  // 9
    "free_nodes",                   // 10
    "id",                           // 11
    "owner",                        // 12
    "type_code",                    // 13
    "subtype_code",                 // 14

    "RDONLY",
    "SYNCHRONOUS",
    "NOEXEC",
    "NOSUID",
    "NODEV",
    "UNION",
    "ASYNC",
    "EXPORTED",
    "LOCAL",
    "QUOTA",
    "ROOTFS",
    "DOVOLFS",
    "DONTBROWSE",
    "UNKNOWNPERMISSIONS",
    "AUTOMOUNTED",
    "JOURNALED",
    "DEFWRITE",
    "MULTILABEL",
    "CPROTECT",
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
  SET_VECTOR_ELT(res, 25, Rf_allocVector(LGLSXP, len));
  SET_VECTOR_ELT(res, 26, Rf_allocVector(LGLSXP, len));
  SET_VECTOR_ELT(res, 27, Rf_allocVector(LGLSXP, len));
  SET_VECTOR_ELT(res, 28, Rf_allocVector(LGLSXP, len));
  SET_VECTOR_ELT(res, 29, Rf_allocVector(LGLSXP, len));
  SET_VECTOR_ELT(res, 30, Rf_allocVector(LGLSXP, len));
  SET_VECTOR_ELT(res, 31, Rf_allocVector(LGLSXP, len));
  SET_VECTOR_ELT(res, 32, Rf_allocVector(LGLSXP, len));
  SET_VECTOR_ELT(res, 33, Rf_allocVector(LGLSXP, len));

  for (i = 0; i < len; i++) {
    int ret = statfs(CHAR(STRING_ELT(abspath, i)), &sfs);
    if (ret != 0) {
      ps__set_error(
        "statfs %s: %d %s",
        CHAR(STRING_ELT(abspath, i)), errno, strerror(errno)
      );
      ps__throw_error();
    }
    SET_STRING_ELT(VECTOR_ELT(res, 1), i, STRING_ELT(mps, i));
    SET_STRING_ELT(VECTOR_ELT(res, 2), i,
                   Rf_mkCharCE(sfs.f_mntfromname, CE_UTF8));
    SET_STRING_ELT(VECTOR_ELT(res, 3), i,
                   Rf_mkCharCE(sfs.f_fstypename, CE_UTF8));
    REAL(VECTOR_ELT(res, 4))[i] = sfs.f_bsize;
    REAL(VECTOR_ELT(res, 5))[i] = sfs.f_iosize;
    REAL(VECTOR_ELT(res, 6))[i] = sfs.f_blocks;
    REAL(VECTOR_ELT(res, 7))[i] = sfs.f_bfree;
    REAL(VECTOR_ELT(res, 8))[i] = sfs.f_bavail;
    REAL(VECTOR_ELT(res, 9))[i] = sfs.f_files;
    REAL(VECTOR_ELT(res, 10))[i] = sfs.f_ffree;
    SET_VECTOR_ELT(VECTOR_ELT(res, 11), i, Rf_allocVector(RAWSXP, sizeof(fsid_t)));
    memcpy(RAW(VECTOR_ELT(VECTOR_ELT(res, 11), i)), &sfs.f_fsid, sizeof(fsid_t));
    REAL(VECTOR_ELT(res, 12))[i] = sfs.f_owner;
    REAL(VECTOR_ELT(res, 13))[i] = sfs.f_type;
    REAL(VECTOR_ELT(res, 14))[i] = sfs.f_fssubtype;

    LOGICAL(VECTOR_ELT(res, 15))[i] = sfs.f_flags & MNT_RDONLY;
    LOGICAL(VECTOR_ELT(res, 16))[i] = sfs.f_flags & MNT_SYNCHRONOUS;
    LOGICAL(VECTOR_ELT(res, 17))[i] = sfs.f_flags & MNT_NOEXEC;
    LOGICAL(VECTOR_ELT(res, 18))[i] = sfs.f_flags & MNT_NOSUID;
    LOGICAL(VECTOR_ELT(res, 19))[i] = sfs.f_flags & MNT_NODEV;
    LOGICAL(VECTOR_ELT(res, 20))[i] = sfs.f_flags & MNT_UNION;
    LOGICAL(VECTOR_ELT(res, 21))[i] = sfs.f_flags & MNT_ASYNC;
    LOGICAL(VECTOR_ELT(res, 22))[i] = sfs.f_flags & MNT_EXPORTED;
    LOGICAL(VECTOR_ELT(res, 23))[i] = sfs.f_flags & MNT_LOCAL;
    LOGICAL(VECTOR_ELT(res, 24))[i] = sfs.f_flags & MNT_QUOTA;
    LOGICAL(VECTOR_ELT(res, 25))[i] = sfs.f_flags & MNT_ROOTFS;
    LOGICAL(VECTOR_ELT(res, 26))[i] = sfs.f_flags & MNT_DOVOLFS;
    LOGICAL(VECTOR_ELT(res, 27))[i] = sfs.f_flags & MNT_DONTBROWSE;
    LOGICAL(VECTOR_ELT(res, 28))[i] = sfs.f_flags & MNT_UNKNOWNPERMISSIONS;
    LOGICAL(VECTOR_ELT(res, 29))[i] = sfs.f_flags & MNT_AUTOMOUNTED;
    LOGICAL(VECTOR_ELT(res, 30))[i] = sfs.f_flags & MNT_JOURNALED;
    LOGICAL(VECTOR_ELT(res, 31))[i] = sfs.f_flags & MNT_DEFWRITE;
    LOGICAL(VECTOR_ELT(res, 32))[i] = sfs.f_flags & MNT_MULTILABEL;
/* Since sys/mount.h includes Availability.h, we use underscore-prefixed macro */
#if __MAC_OS_X_VERSION_MIN_REQUIRED >= 1090
    LOGICAL(VECTOR_ELT(res, 33))[i] = sfs.f_flags & MNT_CPROTECT;
#endif
  }

  UNPROTECT(1);
  return res;
}

int ps__sys_vminfo(vm_statistics_data_t *vmstat) {
  kern_return_t ret;
  mach_msg_type_number_t count = sizeof(*vmstat) / sizeof(integer_t);
  mach_port_t mport = mach_host_self();

  ret = host_statistics(mport, HOST_VM_INFO, (host_info_t)vmstat, &count);
  if (ret != KERN_SUCCESS) {
    ps__set_error(
      "host_statistics(HOST_VM_INFO) syscall failed: %s",
      mach_error_string(ret)
    );
    return 1;
  }
  mach_port_deallocate(mach_task_self(), mport);
  return 0;
}

SEXP ps__system_memory(void) {
  int mib[2];
  uint64_t total;
  size_t len = sizeof(total);
  vm_statistics_data_t vm;
  int pagesize = getpagesize();
  // physical mem
  mib[0] = CTL_HW;
  mib[1] = HW_MEMSIZE;

  // This is also available as sysctlbyname("hw.memsize").
  if (sysctl(mib, 2, &total, &len, NULL, 0)) {
    if (errno != 0) {
      ps__set_error_from_errno();
    } else {
      ps__set_error("sysctl(HW_MEMSIZE) syscall failed");
    }
    ps__throw_error();
  }

  // vm
  if (ps__sys_vminfo(&vm)) ps__throw_error();

  return ps__build_named_list(
    "dddddd",
    "total",       (double) total,
    "active",      (double) vm.active_count * pagesize,
    "inactive",    (double) vm.inactive_count * pagesize,
    "wired",       (double) vm.wire_count * pagesize,
    "free",        (double) vm.free_count * pagesize,
    "speculative", (double) vm.speculative_count * pagesize
  );
}

SEXP ps__system_swap(void) {
  int mib[2];
  size_t size;
  struct xsw_usage totals;
  vm_statistics_data_t vm;
  int pagesize = getpagesize();

  mib[0] = CTL_VM;
  mib[1] = VM_SWAPUSAGE;
  size = sizeof(totals);
  if (sysctl(mib, 2, &totals, &size, NULL, 0) == -1) {
  if (errno != 0) {
      ps__set_error_from_errno();
    } else {
      ps__set_error("sysctl(VM_SWAPUSAGE) syscall failed");
    }
    ps__throw_error();
  }

  // vm
  if (ps__sys_vminfo(&vm)) ps__throw_error();

  return ps__build_named_list(
    "ddddd",
    "total", (double) totals.xsu_total,
    "used",  (double) totals.xsu_used,
    "free",  (double) totals.xsu_avail,
    "sin",   (double) vm.pageins * pagesize,
    "sout",  (double) vm.pageouts * pagesize
  );
}

#define ARRAY_SIZE(a) (sizeof(a) / sizeof((a)[0]))

SEXP ps__loadavg(SEXP counter_name) {
  struct loadavg info;
  size_t size = sizeof(info);
  int which[] = {CTL_VM, VM_LOADAVG};

  if (sysctl(which, ARRAY_SIZE(which), &info, &size, NULL, 0) < 0) {
    ps__set_error_from_errno();
    ps__throw_error();
  }

  SEXP ret = PROTECT(allocVector(REALSXP, 3));
  REAL(ret)[0] = (double) info.ldavg[0] / info.fscale;
  REAL(ret)[1] = (double) info.ldavg[1] / info.fscale;
  REAL(ret)[2] = (double) info.ldavg[2] / info.fscale;

  UNPROTECT(1);
  return ret;
}

SEXP ps__system_cpu_times(void) {
  mach_msg_type_number_t count = HOST_CPU_LOAD_INFO_COUNT;
  kern_return_t error;
  host_cpu_load_info_data_t r_load;

  mach_port_t host_port = mach_host_self();
  error = host_statistics(host_port, HOST_CPU_LOAD_INFO,
                          (host_info_t)&r_load, &count);

  mach_port_deallocate(mach_task_self(), host_port);

  if (error != KERN_SUCCESS) {
    ps__set_error_from_errno();
    ps__throw_error();
  }

  const char *nms[] = { "user", "nice", "system", "idle", "" };
  SEXP ret = PROTECT(Rf_mkNamed(REALSXP, nms));

  REAL(ret)[0] = (double) r_load.cpu_ticks[CPU_STATE_USER]   / CLK_TCK;
  REAL(ret)[1] = (double) r_load.cpu_ticks[CPU_STATE_NICE]   / CLK_TCK;
  REAL(ret)[2] = (double) r_load.cpu_ticks[CPU_STATE_SYSTEM] / CLK_TCK;
  REAL(ret)[3] = (double) r_load.cpu_ticks[CPU_STATE_IDLE]   / CLK_TCK;

  UNPROTECT(1);
  return ret;
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

struct psll__wait_cleanup_data {
  int epfd;
};

static void psll__wait_cleanup(void *data) {
  struct psll__wait_cleanup_data *cdata =
    (struct psll__wait_cleanup_data*) data;
  if (cdata->epfd != -1) {
    close(cdata->epfd);
    cdata->epfd = -1;
  }
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

SEXP psll_wait(SEXP pps, SEXP timeout) {
  int ctimeout = INTEGER(timeout)[0];
  // this is much simpler, no need to poll at all
  if (ctimeout == 0) {
    return psll_wait0(pps);
  }

  int forever = ctimeout < 0;
  R_xlen_t i, num_handles = Rf_xlength(pps);
  struct psll__wait_cleanup_data cdata = { -1 };
  r_call_on_early_exit(psll__wait_cleanup, &cdata);
  cdata.epfd = kqueue();
  if (cdata.epfd == -1) {
    ps__set_error_from_errno();
    ps__throw_error();
  }

  SEXP res = PROTECT(Rf_allocVector(LGLSXP, num_handles));

  int ret;
  R_xlen_t topoll = 0;
  for (i = 0; i < num_handles; i++) {
    ps_handle_t *handle = R_ExternalPtrAddr(VECTOR_ELT(pps, i));
    if (!handle) Rf_error("Process pointer #%d cleaned up already", (int) i);
    if (!LOGICAL(psll_is_running(VECTOR_ELT(pps, i)))[0]) {
      // already done
      LOGICAL(res)[i] = 1;
    } else {
      struct kevent ev;
      EV_SET(
        &ev,
        handle->pid,
        EVFILT_PROC,
        EV_ADD | EV_ONESHOT,
        NOTE_EXIT,
        (intptr_t) NULL,
        LOGICAL(res) + i
      );
      ret = kevent(cdata.epfd, &ev, 1, NULL, 0, NULL);
      if (ret == -1) {
        if (errno == ESRCH) {
          LOGICAL(res)[i] = 1;
        } else {
          ps__set_error_from_errno();
          ps__throw_error();
        }
      } else {
        topoll++;
        LOGICAL(res)[i] = 0;
      }
    }
  }

  // early exit if nothing to do
  if (topoll == 0) {
    psll__wait_cleanup(&cdata);
    UNPROTECT(1);
    return res;
  }

  SEXP revents = PROTECT(Rf_allocVector(RAWSXP, sizeof(struct kevent) * topoll));
  struct kevent *events = (struct kevent*) RAW(revents);

  // first timeout is the smaller of PROCESSX_INTERRUPT_INTERVAL & timeout
  struct timespec ts = { 0, 0 };
  if (forever || ctimeout > PROCESSX_INTERRUPT_INTERVAL) {
    ts.tv_sec = PROCESSX_INTERRUPT_INTERVAL / 1000;
    ts.tv_nsec = (PROCESSX_INTERRUPT_INTERVAL % 1000) * 1000 * 1000;
  } else {
    ts.tv_sec = ctimeout / 1000;
    ts.tv_nsec = (ctimeout % 1000) * 1000 * 1000;
  }

  // this is the ultimate time limit, unless we poll forever
  struct timespec due;
  if (!forever) {
    add_time(&due, ctimeout);
  }

  do {
    do {
      ret = kevent(cdata.epfd, NULL, 0, events, topoll, &ts);
    } while (ret == -1 && errno == EINTR);

    if (ret == -1) {
      ps__set_error_from_errno();
      ps__throw_error();
    }

    for (i = 0; i < ret; i++) {
      int *ptr = (int*) events[i].udata;
      *ptr = 1;
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
        ts.tv_sec = tl / 1000;
        ts.tv_nsec = (tl % 1000) * 1000 * 1000;
      }
    }

    R_CheckUserInterrupt();

  } while (1);

  psll__wait_cleanup(&cdata);
  UNPROTECT(2);
  return res;
}
