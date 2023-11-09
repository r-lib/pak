
#include "common.h"
#include "windows.h"
#include "arch/windows/process_info.h"
#include "arch/windows/process_handles.h"

#include <tlhelp32.h>
#include <string.h>
#include <math.h>
#include <wtsapi32.h>

static void psll_finalizer(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  if (handle) free(handle);
}

static int ps__create_time_raw(DWORD pid, FILETIME *ftCreate) {
  HANDLE hProcess = ps__handle_from_pid(pid);
  FILETIME ftExit, ftKernel, ftUser;

  if (! hProcess) goto error;	/* error set already */

  if (! GetProcessTimes(hProcess, ftCreate, &ftExit, &ftKernel, &ftUser)) {
    if (GetLastError() == ERROR_ACCESS_DENIED) {
      // usually means the process has died so we throw a
      // NoSuchProcess here
      ps__no_such_process(pid, 0);
    } else {
      ps__set_error_from_windows_error(0);
    }
    goto error;
  }

  CloseHandle(hProcess);
  return 0;

 error:
  if (hProcess) CloseHandle(hProcess);
  return -1;
}

double ps__filetime_to_unix(FILETIME ft) {
  long long ll, secs, nsecs;
  ll = ((LONGLONG) ft.dwHighDateTime) << 32;
  ll += ft.dwLowDateTime - 116444736000000000LL;
  secs = ll / 10000000;
  nsecs = ll % 10000000;
  return (double) secs + ((double) nsecs) / 10000000;
}

static SEXP ps__is_running(ps_handle_t *handle) {
  FILETIME ftCreate;
  int ret = ps__create_time_raw(handle->pid, &ftCreate);

  if (ret) return ScalarLogical(0);

  if (handle->wtime.dwHighDateTime) {
    if (handle->wtime.dwHighDateTime != ftCreate.dwHighDateTime ||
	handle->wtime.dwLowDateTime != ftCreate.dwLowDateTime)  {
      return ScalarLogical(0);
    } else {
      return ScalarLogical(1);
    }
  } else {
    double unix_time = ps__filetime_to_unix(ftCreate);
    if (fabs(unix_time - handle->create_time) > 0.01) {
      return ScalarLogical(0);
    } else  {
      return ScalarLogical(1);
    }
  }

  /* Never reached */
  return R_NilValue;
}

void PS__CHECK_HANDLE(ps_handle_t *handle) {
  SEXP ret = ps__is_running(handle);
  if (!LOGICAL(ret)[0]) {
    ps__no_such_process(handle->pid, 0);
    ps__throw_error();
  }
}

static int ps__proc_is_suspended(DWORD pid) {
  ULONG i;
  PSYSTEM_PROCESS_INFORMATION process;
  PVOID buffer;

  if (! ps__get_proc_info(pid, &process, &buffer)) return -1;

  for (i = 0; i < process->NumberOfThreads; i++) {
    if (process->Threads[i].ThreadState != Waiting ||
	process->Threads[i].WaitReason != Suspended) {
      free(buffer);
      return 0;
    }
  }
  free(buffer);
  return 1;
}

static SEXP ps__status(DWORD pid) {
  int ret = ps__proc_is_suspended(pid);
  if (ret < 0) return R_NilValue;

  return ret ? mkString("stopped") : mkString("running");
}

SEXP psll_handle(SEXP pid, SEXP time) {
  DWORD cpid = isNull(pid) ? GetCurrentProcessId() : INTEGER(pid)[0];
  double ctime;
  ps_handle_t *handle;
  SEXP res;
  FILETIME ftCreate = { 0, 0 };

  if (!isNull(time)) {
    ctime = REAL(time)[0];

  } else {
    int ret = ps__create_time_raw(cpid, &ftCreate);
    if (ret) ps__throw_error();
    ctime = ps__filetime_to_unix(ftCreate);
  }

  handle = malloc(sizeof(ps_handle_t));

  if (!handle) {
    ps__no_memory("");
    ps__throw_error();
  }

  handle->pid = cpid;
  handle->create_time = ctime;
  handle->gone = 0;
  handle->wtime = ftCreate;

  PROTECT(res = R_MakeExternalPtr(handle, R_NilValue, R_NilValue));
  R_RegisterCFinalizerEx(res, psll_finalizer, /* onexit */ 0);
  setAttrib(res, R_ClassSymbol, mkString("ps_handle"));

  UNPROTECT(1);
  return res;
}

SEXP psll_format(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  SEXP name, status, result;

  if (!handle) error("Process pointer cleaned up already");

  name = ps__name(handle->pid);
  if (isNull(name)) name = mkString("???");
  PROTECT(name);

  status = ps__status(handle->pid);
  if (isNull(status)) status = mkString("terminated");
  PROTECT(status);

  PROTECT(result = ps__build_list("OldO", name, (long) handle->pid,
				  handle->create_time, status));

  /* We do not check that the pid is still valid here, because we want
     to be able to format & print processes that have finished already. */

  UNPROTECT(3);
  return result;
}

SEXP psll_parent(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  SEXP pid, parent, time;
  DWORD ppid;
  FILETIME pft;
  double pctime;
  int ret;

  if (!handle) error("Process pointer cleaned up already");

  PROTECT(pid = ps__ppid(handle->pid));
  if (isNull(pid)) ps__throw_error();
  PS__CHECK_HANDLE(handle);

  ppid = INTEGER(pid)[0];

  ret = ps__create_time_raw(ppid, &pft);
  if (ret) ps__throw_error();

  pctime = ps__filetime_to_unix(pft);
  if (pctime > handle->create_time) {
    ps__no_such_process(ppid, 0);
    ps__throw_error();
  }

  PROTECT(time = ScalarReal(pctime));
  PROTECT(parent = psll_handle(pid, time));

  UNPROTECT(3);
  return parent;
}

SEXP psll_ppid(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  SEXP ret;

  if (!handle) error("Process pointer cleaned up already");

  PROTECT(ret = ps__ppid(handle->pid));
  if (isNull(ret)) ps__throw_error();

  PS__CHECK_HANDLE(handle);

  UNPROTECT(1);
  return ret;
}


SEXP psll_is_running(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  if (!handle) error("Process pointer cleaned up already");
  return ps__is_running(handle);
}


SEXP psll_name(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  SEXP ret;

  if (!handle) error("Process pointer cleaned up already");

  PROTECT(ret = ps__name(handle->pid));
  if (isNull(ret)) ps__throw_error();

  PS__CHECK_HANDLE(handle);

  UNPROTECT(1);
  return ret;
}

SEXP psll_exe(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  SEXP result;

  if (!handle) error("Process pointer cleaned up already");
  result = ps__exe(handle->pid);
  if (isNull(result)) ps__throw_error();

  PS__CHECK_HANDLE(handle);

  return result;
}

SEXP psll_cmdline(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  SEXP result;

  if (!handle) error("Process pointer cleaned up already");

  result = ps__get_cmdline(handle->pid);
  if (isNull(result)) ps__throw_error();

  PS__CHECK_HANDLE(handle);

  return result;
}

SEXP psll_status(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  SEXP ret;

  if (!handle) error("Process pointer cleaned up already");
  ret = ps__status(handle->pid);
  if (isNull(ret)) ps__throw_error();

  PS__CHECK_HANDLE(handle);

  return ret;
}

SEXP psll_username(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  SEXP ret, result;
  size_t len1, len2;
  char *str;

  if (!handle) error("Process pointer cleaned up already");

  if (handle->pid == 0 || handle->pid == 4) {
    return mkString("NT AUTHORITY\\SYSTEM");
  }

  PROTECT(ret = ps__proc_username(handle->pid));
  if (isNull(ret)) ps__throw_error();

  PS__CHECK_HANDLE(handle);

  len1 = strlen(CHAR(STRING_ELT(ret, 0)));
  len2 = strlen(CHAR(STRING_ELT(ret, 1)));
  str = R_alloc(len1 + len2 + 2, sizeof(char));
  memcpy(str, CHAR(STRING_ELT(ret, 0)), len1);
  *(str + len1) = '\\';
  memcpy(str + len1 + 1, CHAR(STRING_ELT(ret,  1)), len2 + 1);

  PROTECT(result = mkString(str));
  UNPROTECT(2);

  return result;
}

SEXP psll_cwd(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  SEXP result;

  if (!handle) error("Process pointer cleaned up already");

  if (handle->pid == 0 || handle->pid == 4) {
    ps__access_denied("");
    ps__throw_error();
  }

  PROTECT(result = ps__get_cwd(handle->pid));
  if (isNull(result)) ps__throw_error();

  PS__CHECK_HANDLE(handle);

  UNPROTECT(1);
  return  result;
}

SEXP psll_uids(SEXP p) {
  ps__not_implemented("uids");
  ps__throw_error();
  return R_NilValue;
}

SEXP psll_gids(SEXP p) {
  ps__not_implemented("uids");
  ps__throw_error();
  return R_NilValue;
}

SEXP psll_terminal(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  if (!handle) error("Process pointer cleaned up already");
  PS__CHECK_HANDLE(handle);
  return ScalarString(NA_STRING);
}

SEXP psll_environ(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  SEXP result;

  if (!handle) error("Process pointer cleaned up already");

  if (handle->pid == 0 || handle->pid == 4) {
    ps__access_denied("");
    ps__throw_error();
  }

  PROTECT(result = ps__get_environ(handle->pid));
  if (isNull(result)) ps__throw_error();

  PS__CHECK_HANDLE(handle);

  UNPROTECT(1);
  return  result;
}

SEXP psll_num_threads(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  SEXP result;

  if (!handle) error("Process pointer cleaned up already");

  PROTECT(result = ps__proc_num_threads(handle->pid));
  if (isNull(result)) ps__throw_error();

  PS__CHECK_HANDLE(handle);

  UNPROTECT(1);
  return  result;
}

SEXP psll_cpu_times(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  SEXP result, result2, names;

  if (!handle) error("Process pointer cleaned up already");

  PROTECT(result = ps__proc_cpu_times(handle->pid));
  if (!isNull(result)) {
    PS__CHECK_HANDLE(handle);
    UNPROTECT(1);
    return result;
  }

  UNPROTECT(1);

  PROTECT(result2 = ps__proc_info(handle->pid));
  if (isNull(result2)) {
    UNPROTECT(1);
    ps__throw_error();
  }

  PS__CHECK_HANDLE(handle);

  PROTECT(result = allocVector(REALSXP, 4));
  REAL(result)[0] = REAL(VECTOR_ELT(result2, 2))[0];
  REAL(result)[1] = REAL(VECTOR_ELT(result2, 3))[0];
  REAL(result)[2] = REAL(result)[3] = NA_REAL;
  PROTECT(names = ps__build_string("user", "system", "children_user",
				   "children_system", NULL));
  setAttrib(result, R_NamesSymbol, names);

  UNPROTECT(3);
  return result;
}

SEXP psll_memory_info(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  SEXP result, result2, names;

  if (!handle) error("Process pointer cleaned up already");

  PROTECT(result2 = ps__proc_info(handle->pid));
  if (isNull(result2)) {
    UNPROTECT(1);
    ps__throw_error();
  }

  PS__CHECK_HANDLE(handle);

  PROTECT(result = allocVector(REALSXP, 12));
  REAL(result)[0] = REAL(VECTOR_ELT(result2, 12))[0];
  REAL(result)[1] = REAL(VECTOR_ELT(result2, 13))[0];
  REAL(result)[2] = REAL(VECTOR_ELT(result2, 14))[0];
  REAL(result)[3] = REAL(VECTOR_ELT(result2, 15))[0];
  REAL(result)[4] = REAL(VECTOR_ELT(result2, 16))[0];
  REAL(result)[5] = REAL(VECTOR_ELT(result2, 17))[0];
  REAL(result)[6] = REAL(VECTOR_ELT(result2, 18))[0];
  REAL(result)[7] = REAL(VECTOR_ELT(result2, 19))[0];
  REAL(result)[8] = REAL(VECTOR_ELT(result2, 20))[0];
  REAL(result)[9] = REAL(VECTOR_ELT(result2, 21))[0];
  REAL(result)[10] = REAL(VECTOR_ELT(result2, 14))[0];
  REAL(result)[11] = REAL(VECTOR_ELT(result2, 19))[0];
  PROTECT(names = ps__build_string(
    "num_page_faults", "peak_wset", "wset", "peak_paged_pool",
    "paged_pool", "peak_non_paged_pool", "non_paged_pool",
    "pagefile", "peak_pagefile", "mem_private", "rss", "vms", NULL));
  setAttrib(result, R_NamesSymbol, names);

  UNPROTECT(3);
  return result;
}

SEXP psll_send_signal(SEXP p, SEXP sig) {
  ps__not_implemented("uids");
  ps__throw_error();
  return R_NilValue;
}

SEXP psll_suspend(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  SEXP running, ret;

  if (!handle) error("Process pointer cleaned up already");

  HANDLE hProcess = ps__handle_from_pid(handle->pid);
  if (!hProcess) goto error;

  running = ps__is_running(handle);
  if (!LOGICAL(running)[0]) {
    ps__no_such_process(handle->pid, 0);
    goto error;
  }

  PROTECT(ret = ps__proc_suspend(handle->pid));
  if (isNull(ret)) goto error;

  UNPROTECT(1);
  return R_NilValue;

 error:
  if (hProcess) CloseHandle(hProcess);
  ps__throw_error();
  return  R_NilValue;
}

SEXP psll_resume(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  SEXP running, ret;

  if (!handle) error("Process pointer cleaned up already");

  HANDLE hProcess = ps__handle_from_pid(handle->pid);
  if (!hProcess) goto error;

  running = ps__is_running(handle);
  if (!LOGICAL(running)[0]) {
    ps__no_such_process(handle->pid, 0);
    goto error;
  }

  PROTECT(ret = ps__proc_resume(handle->pid));
  if (isNull(ret)) goto error;

  UNPROTECT(1);
  return R_NilValue;

 error:
  if (hProcess) CloseHandle(hProcess);
  ps__throw_error();
  return  R_NilValue;
}

SEXP psll_terminate(SEXP p) {
  ps__not_implemented("uids");
  ps__throw_error();
  return R_NilValue;
}

SEXP psll_kill(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  SEXP running, ret;

  if (!handle) error("Process pointer cleaned up already");

  HANDLE hProcess = ps__handle_from_pid(handle->pid);
  if (!hProcess) goto error;

  running = ps__is_running(handle);
  if (!LOGICAL(running)[0]) {
    ps__no_such_process(handle->pid, 0);
    goto error;
  }

  PROTECT(ret = ps__proc_kill(handle->pid));
  if (isNull(ret)) ps__throw_error();

  UNPROTECT(1);
  return R_NilValue;

 error:
  if (hProcess) CloseHandle(hProcess);
  ps__throw_error();
  return  R_NilValue;
}

static ULONGLONG (*ps__GetTickCount64)(void) = NULL;

/*
 * Return a double representing the system uptime expressed in seconds
 * since the epoch.
 */
SEXP ps__boot_time() {
  double now, uptime;
  FILETIME fileTime;
  HINSTANCE hKernel32;
  ps__GetTickCount64 = NULL;

  GetSystemTimeAsFileTime(&fileTime);

  now =  ps__filetime_to_unix(fileTime);

  // GetTickCount64() is Windows Vista+ only. Dinamically load
  // GetTickCount64() at runtime. We may have used
  // "#if (_WIN32_WINNT >= 0x0600)" pre-processor but that way
  // the produced exe/wheels cannot be used on Windows XP, see:
  // https://github.com/giampaolo/psutil/issues/811#issuecomment-230639178
  hKernel32 = GetModuleHandleW(L"KERNEL32");
  ps__GetTickCount64 = (void*)GetProcAddress(hKernel32, "GetTickCount64");
  if (ps__GetTickCount64 != NULL) {
    // Windows >= Vista
    uptime = ps__GetTickCount64() / (ULONGLONG)1000.00f;
    return ScalarReal(now - uptime);
  } else {
    // Windows XP.
    // GetTickCount() time will wrap around to zero if the
    // system is run continuously for 49.7 days.
    uptime = GetTickCount() / (LONGLONG)1000.00f;
    return ScalarReal(now - uptime);
  }
}

/*
 * Return the number of logical, active CPUs. Return 0 if undetermined.
 * See discussion at: https://bugs.python.org/issue33166#msg314631
 */

#ifndef ALL_PROCESSOR_GROUPS
#define ALL_PROCESSOR_GROUPS 0xffff
#endif

unsigned int ps__get_num_cpus(int fail_on_err) {
  unsigned int ncpus = 0;
  SYSTEM_INFO sysinfo;
  static DWORD(CALLBACK *_GetActiveProcessorCount)(WORD) = NULL;
  HINSTANCE hKernel32;

  // GetActiveProcessorCount is available only on 64 bit versions
  // of Windows from Windows 7 onward.
  // Windows Vista 64 bit and Windows XP doesn't have it.
  hKernel32 = GetModuleHandleW(L"KERNEL32");
  _GetActiveProcessorCount = (void*)GetProcAddress(
    hKernel32, "GetActiveProcessorCount");

  if (_GetActiveProcessorCount != NULL) {
    ncpus = _GetActiveProcessorCount(ALL_PROCESSOR_GROUPS);
    if ((ncpus == 0) && (fail_on_err == 1)) {
      ps__set_error_from_windows_error(0);
    }
  } else {
    ps__debug("GetActiveProcessorCount() not available; "
		 "using GetNativeSystemInfo()");
    GetNativeSystemInfo(&sysinfo);
    ncpus = (unsigned int) sysinfo.dwNumberOfProcessors;
    if ((ncpus == 0) && (fail_on_err == 1)) {
      ps__set_error("GetNativeSystemInfo() failed to retrieve CPU count");
      ps__throw_error();
    }
  }

  return ncpus;
}

SEXP ps__cpu_count_logical() {
  unsigned int ncpus;

  ncpus = ps__get_num_cpus(0);
  if (ncpus != 0) {
    return ScalarInteger(ncpus);
  } else {
    return ScalarInteger(NA_INTEGER);
  }
}

typedef BOOL (WINAPI *LPFN_GLPI)(
    PSYSTEM_LOGICAL_PROCESSOR_INFORMATION,
    PDWORD);

// Helper function to count set bits in the processor mask.
DWORD ps__count_set_bits(ULONG_PTR bitMask) {
  DWORD LSHIFT = sizeof(ULONG_PTR)*8 - 1;
  DWORD bitSetCount = 0;
  ULONG_PTR bitTest = (ULONG_PTR)1 << LSHIFT;
  DWORD i;

  for (i = 0; i <= LSHIFT; ++i) {
    bitSetCount += ((bitMask & bitTest)?1:0);
    bitTest/=2;
  }

  return bitSetCount;
}

SEXP ps__cpu_count_physical() {
  LPFN_GLPI glpi;
  BOOL done = FALSE;
  PSYSTEM_LOGICAL_PROCESSOR_INFORMATION buffer = NULL;
  PSYSTEM_LOGICAL_PROCESSOR_INFORMATION ptr = NULL;
  DWORD returnLength = 0;
  DWORD nproc = 0;
  DWORD byteOffset = 0;

  glpi = (LPFN_GLPI) GetProcAddress(
    GetModuleHandle(TEXT("kernel32")),
    "GetLogicalProcessorInformation");

  if (NULL == glpi) return ScalarInteger(NA_INTEGER);

  while (!done) {
    DWORD rc = glpi(buffer, &returnLength);

    if (FALSE == rc) {
      if (GetLastError() == ERROR_INSUFFICIENT_BUFFER) {
	if (buffer) free(buffer);
	buffer = (PSYSTEM_LOGICAL_PROCESSOR_INFORMATION)  malloc(returnLength);

	if (NULL == buffer) {
	  ps__no_memory("");
	  ps__throw_error();
	}
      } else {
	ps__set_error_from_windows_error(0);
	ps__throw_error();
      }
    } else {
      done = TRUE;
    }
  }

  ptr = buffer;

  while (byteOffset + sizeof(SYSTEM_LOGICAL_PROCESSOR_INFORMATION) <= returnLength) {

    switch (ptr->Relationship) {
    case RelationProcessorCore:
      // A hyperthreaded core supplies more than one logical processor.
      nproc += ps__count_set_bits(ptr->ProcessorMask);
      break;
    default:
      break;
    }

    byteOffset += sizeof(SYSTEM_LOGICAL_PROCESSOR_INFORMATION);
    ptr++;
  }

  free(buffer);

  if (nproc > 0) {
    return ScalarInteger(nproc);
  } else {
    return ScalarInteger(NA_INTEGER);
  }
}

SEXP ps__kill_if_env(SEXP marker, SEXP after, SEXP pid, SEXP sig) {
  const char *cmarker = CHAR(STRING_ELT(marker, 0));
  double cafter = REAL(after)[0];
  DWORD cpid = INTEGER(pid)[0];
  SEXP env;
  size_t i, len;
  double ctime = 0, ctime2 = 0;

  /* Filter on start time */
  FILETIME ftCreate;
  int ret = ps__create_time_raw(cpid, &ftCreate);
  if (ret) ps__throw_error();
  ctime = ps__filetime_to_unix(ftCreate);
  if (ctime < cafter - 1) return R_NilValue;

  PROTECT(env = ps__get_environ(cpid));
  if (isNull(env)) ps__throw_error();

  len = LENGTH(env);

  for (i = 0; i < len; i++) {
    if (strstr(CHAR(STRING_ELT(env, i)), cmarker)) {
      HANDLE hProcess = ps__handle_from_pid(cpid);
      FILETIME ftCreate;
      SEXP name, ret2;
      int ret = ps__create_time_raw(cpid, &ftCreate);
      if (ret) {
	CloseHandle(hProcess);
	ps__throw_error();
      }

      ctime2 = ps__filetime_to_unix(ftCreate);
      if (fabs(ctime - ctime2) < 0.01)  {
	PROTECT(name = ps__name(cpid));
	ret2 = ps__proc_kill(cpid);
	CloseHandle(hProcess);
	if (isNull(ret2)) ps__throw_error();
	if (isNull(name)) {
	  UNPROTECT(2);
	  return mkString("???");
	} else {
	  UNPROTECT(2);
	  return name;
	}
      } else  {
	CloseHandle(hProcess);
	UNPROTECT(1);
	return R_NilValue;
      }
    }
  }

  UNPROTECT(1);
  return R_NilValue;
}

SEXP ps__find_if_env(SEXP marker, SEXP after, SEXP pid) {
  const char *cmarker = CHAR(STRING_ELT(marker, 0));
  double cafter = REAL(after)[0];
  long cpid = INTEGER(pid)[0];
  SEXP env;
  size_t i, len;
  SEXP phandle;
  ps_handle_t *handle;
  double ctime = 0;
  FILETIME ftCreate;

  /* Filter on start time */
  int ret = ps__create_time_raw(cpid, &ftCreate);
  if (ret) ps__throw_error();
  ctime = ps__filetime_to_unix(ftCreate);
  if (ctime < cafter - 1) return R_NilValue;

  PROTECT(phandle = psll_handle(pid, R_NilValue));
  handle = R_ExternalPtrAddr(phandle);

  PROTECT(env = ps__get_environ(cpid));
  if (isNull(env)) ps__throw_error();

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
  HANDLE  hProcess = NULL;
  DWORD handleCount;

  if (!handle) error("Process pointer cleaned up already");

  hProcess = ps__handle_from_pid(handle->pid);
  if (hProcess != NULL) {
    if (GetProcessHandleCount(hProcess, &handleCount)) {
      CloseHandle(hProcess);
      PS__CHECK_HANDLE(handle);
      return ScalarInteger(handleCount);
    }
  }

  /* Cleanup on error */
  if (hProcess != NULL) CloseHandle(hProcess);
  PS__CHECK_HANDLE(handle);
  ps__set_error_from_windows_error(0);
  ps__throw_error();
  return R_NilValue;
}

SEXP psll_open_files(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  HANDLE processHandle = NULL;
  DWORD access = PROCESS_DUP_HANDLE | PROCESS_QUERY_INFORMATION;
  SEXP result;

  if (!handle) error("Process pointer cleaned up already");

  processHandle = ps__handle_from_pid_waccess(handle->pid, access);
  if (processHandle == NULL) {
    PS__CHECK_HANDLE(handle);
    ps__set_error_from_windows_error(0);
    ps__throw_error();
  }

  PROTECT(result = ps__get_open_files(handle->pid, processHandle));

  CloseHandle(processHandle);

  PS__CHECK_HANDLE(handle);

  if (isNull(result)) {
    ps__set_error_from_windows_error(0);
    ps__throw_error();
  }

  UNPROTECT(1);
  return result;
}

SEXP psll_dlls(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  HANDLE processHandle = NULL;
  DWORD access = PROCESS_QUERY_INFORMATION | PROCESS_VM_READ;
  SEXP result;

  if (!handle) error("Process pointer cleaned up already");

  processHandle = ps__handle_from_pid_waccess(handle->pid, access);
  if (processHandle == NULL) {
    PS__CHECK_HANDLE(handle);
    ps__set_error_from_windows_error(0);
    ps__throw_error();
  }

  PROTECT(result = ps__get_modules(processHandle));

  CloseHandle(processHandle);

  PS__CHECK_HANDLE(handle);

  if (isNull(result)) {
    ps__set_error_from_windows_error(0);
    ps__throw_error();
  }

  UNPROTECT(1);
  return result;
}

SEXP psll_interrupt(SEXP p, SEXP ctrlc, SEXP interrupt_path) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  const char *cinterrupt_path = CHAR(STRING_ELT(interrupt_path, 0));
  int cctrlc = LOGICAL(ctrlc)[0];
  WCHAR *wpath;
  SEXP running;
  int iret;
  STARTUPINFOW startup = { 0 };
  PROCESS_INFORMATION info = { 0 };
  HANDLE hProcess;
  char arguments[100];
  WCHAR *warguments;
  DWORD process_flags;

  if (!handle) error("Process pointer cleaned up already");

  hProcess = ps__handle_from_pid(handle->pid);
  if (!hProcess) goto error;

  running = ps__is_running(handle);
  if (!LOGICAL(running)[0]) {
    ps__no_such_process(handle->pid, 0);
    goto error;
  }

  iret = ps__utf8_to_utf16(cinterrupt_path, &wpath);
  if (iret) goto error;

  iret = snprintf(arguments, sizeof(arguments) - 1, "interrupt.exe %d %s", (int) handle->pid,
		  cctrlc ? "c" : "break");
  if  (iret < 0) goto error;

  iret = ps__utf8_to_utf16(arguments, &warguments);
  if (iret) goto error;

  startup.cb = sizeof(startup);
  startup.lpReserved = NULL;
  startup.lpDesktop = NULL;
  startup.lpTitle = NULL;
  startup.dwFlags = 0;

  startup.cbReserved2 = 0;
  startup.lpReserved2 = 0;

  process_flags = CREATE_UNICODE_ENVIRONMENT | CREATE_NO_WINDOW;

  iret = CreateProcessW(
    /* lpApplicationName =    */ wpath,
    /* lpCommandLine =        */ warguments,
    /* lpProcessAttributes =  */ NULL,
    /* lpThreadAttributes =   */ NULL,
    /* bInheritHandles =      */ 0,
    /* dwCreationFlags =      */ process_flags,
    /* lpEnvironment =        */ NULL,
    /* lpCurrentDirectory =   */ NULL,
    /* lpStartupInfo =        */ &startup,
    /* lpProcessInformation = */ &info);

  if (!iret) {
    ps__set_error_from_errno();
    goto error;
  }

  CloseHandle(info.hThread);
  CloseHandle(info.hProcess);

  return R_NilValue;

 error:
  if (hProcess) CloseHandle(hProcess);
  ps__throw_error();
  return R_NilValue;
}

static int ps_get_proc_wset_information(SEXP p, HANDLE hProcess,
					PMEMORY_WORKING_SET_INFORMATION *wSetInfo) {

  ps_handle_t *handle = R_ExternalPtrAddr(p);
  if (!handle) error("Process pointer cleaned up already");

  DWORD pid = handle->pid;
  NTSTATUS status;
  PVOID buffer;
  SIZE_T bufferSize;

  bufferSize = 0x8000;
  buffer = MALLOC_ZERO(bufferSize);
  if (! buffer) {
    ps__no_memory("get wset information");
    ps__throw_error();
  }

  while (1) {
    status = NtQueryVirtualMemory(
      hProcess,
      NULL,
      MemoryWorkingSetInformation,
      buffer,
      bufferSize,
      NULL
    );
    if (status != STATUS_INFO_LENGTH_MISMATCH) {
      break;
    }

    FREE(buffer);
    bufferSize *= 2;
    // Fail if we're resizing the buffer to something very large.
    if (bufferSize > 256 * 1024 * 1024) {
      ps__set_error("NtQueryVirtualMemory bufsize is too large");
      ps__throw_error();
    }
    buffer = MALLOC_ZERO(bufferSize);
    if (! buffer) {
      ps__no_memory("NtQueryVirtualMemory");
      ps__throw_error();
    }
  }

  if (!NT_SUCCESS(status)) {
    if (status == STATUS_ACCESS_DENIED) {
      ps__access_denied("NtQueryVirtualMemory -> STATUS_ACCESS_DENIED");
    } else if (!LOGICAL(ps__is_running(handle))[0]) {
      ps__no_such_process(pid, "NtQueryVirtualMemory");
    } else {
      ps__set_error_from_windows_error(0);
    }
    HeapFree(GetProcessHeap(), 0, buffer);
    ps__throw_error();
  }

  *wSetInfo = (PMEMORY_WORKING_SET_INFORMATION) buffer;
  return 0;
}

/*
 * Returns the USS of the process.
 * Reference:
 * https://dxr.mozilla.org/mozilla-central/source/xpcom/base/
 *     nsMemoryReporterManager.cpp
 */

SEXP psll_memory_uss(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  if (!handle) error("Process pointer cleaned up already");

  DWORD pid = handle->pid;
  HANDLE hProcess = ps__handle_from_pid(pid);
  PS__PROCESS_WS_COUNTERS wsCounters;
  PMEMORY_WORKING_SET_INFORMATION wsInfo;
  ULONG_PTR i;

  if (! hProcess) {
    ps__throw_error();
  }

  if (ps_get_proc_wset_information(p, hProcess, &wsInfo) != 0) {
    CloseHandle(hProcess);
    return NULL;
  }
  memset(&wsCounters, 0, sizeof(PS__PROCESS_WS_COUNTERS));

  for (i = 0; i < wsInfo->NumberOfEntries; i++) {
    // This is what ProcessHacker does.
    /*
      wsCounters.NumberOfPages++;
      if (wsInfo->WorkingSetInfo[i].ShareCount > 1)
      wsCounters.NumberOfSharedPages++;
      if (wsInfo->WorkingSetInfo[i].ShareCount == 0)
      wsCounters.NumberOfPrivatePages++;
      if (wsInfo->WorkingSetInfo[i].Shared)
      wsCounters.NumberOfShareablePages++;
    */

    // This is what we do: count shared pages that only one process
    // is using as private (USS).
    if (!wsInfo->WorkingSetInfo[i].Shared ||
	wsInfo->WorkingSetInfo[i].ShareCount <= 1) {
      wsCounters.NumberOfPrivatePages++;
    }
  }

  HeapFree(GetProcessHeap(), 0, wsInfo);
  CloseHandle(hProcess);

  SYSTEM_INFO sysinfo;
  GetNativeSystemInfo(&sysinfo);

  return Rf_ScalarInteger(wsCounters.NumberOfPrivatePages * sysinfo.dwPageSize);
}

SEXP ps__users() {
  HANDLE hServer = WTS_CURRENT_SERVER_HANDLE;
  WCHAR *buffer_user = NULL;
  LPTSTR buffer_addr = NULL;
  PWTS_SESSION_INFO sessions = NULL;
  DWORD count;
  DWORD i;
  DWORD sessionId;
  DWORD bytes;
  PWTS_CLIENT_ADDRESS address;
  char address_str[50];
  double unix_time;

  PWINSTATIONQUERYINFORMATIONW WinStationQueryInformationW;
  WINSTATION_INFO station_info;
  HINSTANCE hInstWinSta = NULL;
  ULONG returnLen;

  SEXP retlist, raddress, username;

  hInstWinSta = LoadLibraryA("winsta.dll");
  WinStationQueryInformationW = (PWINSTATIONQUERYINFORMATIONW) \
    GetProcAddress(hInstWinSta, "WinStationQueryInformationW");

  if (WTSEnumerateSessions(hServer, 0, 1, &sessions, &count) == 0) {
    ps__set_error_from_windows_error(0);
    goto error;
  }

  PROTECT(retlist = allocVector(VECSXP, count));

  for (i = 0; i < count; i++) {
    SET_VECTOR_ELT(retlist, i, R_NilValue);
    raddress = R_NilValue;
    sessionId = sessions[i].SessionId;
    if (buffer_user != NULL)
      WTSFreeMemory(buffer_user);
    if (buffer_addr != NULL)
      WTSFreeMemory(buffer_addr);

    buffer_user = NULL;
    buffer_addr = NULL;

    // username
    bytes = 0;
    if (WTSQuerySessionInformationW(hServer, sessionId, WTSUserName,
				    &buffer_user, &bytes) == 0) {
      ps__set_error_from_windows_error(0);
      goto error;
    }
    if (bytes <= 2) continue;

    // address
    bytes = 0;
    if (WTSQuerySessionInformation(hServer, sessionId, WTSClientAddress,
				   &buffer_addr, &bytes) == 0) {
      ps__set_error_from_windows_error(0);
      goto error;
    }

    address = (PWTS_CLIENT_ADDRESS) buffer_addr;
    if (address->AddressFamily == 0 &&  // AF_INET
	(address->Address[0] || address->Address[1] ||
	 address->Address[2] || address->Address[3])) {
      snprintf(address_str,
	       sizeof(address_str),
	       "%u.%u.%u.%u",
	       address->Address[0],
	       address->Address[1],
	       address->Address[2],
	       address->Address[3]);
      raddress = mkString(address_str);
    } else {
      raddress = mkString("");
    }
    PROTECT(raddress);

    // login time
    if (!WinStationQueryInformationW(hServer,
				     sessionId,
				     WinStationInformation,
				     &station_info,
				     sizeof(station_info),
				     &returnLen))  {
	goto error;
      }

    unix_time = ps__filetime_to_unix(station_info.ConnectTime);

    PROTECT(username = ps__utf16_to_strsxp(buffer_user, -1));

    SET_VECTOR_ELT(
      retlist, i,
      ps__build_list("OOOdi", username, ScalarString(NA_STRING), raddress,
		     unix_time, NA_INTEGER));
    UNPROTECT(2);
  }

  WTSFreeMemory(sessions);
  WTSFreeMemory(buffer_user);
  WTSFreeMemory(buffer_addr);
  FreeLibrary(hInstWinSta);

  UNPROTECT(1);
  return retlist;

 error:
  if (hInstWinSta != NULL) FreeLibrary(hInstWinSta);
  if (sessions != NULL) WTSFreeMemory(sessions);
  if (buffer_user != NULL) WTSFreeMemory(buffer_user);
  if (buffer_addr != NULL) WTSFreeMemory(buffer_addr);
  ps__throw_error();
  return R_NilValue;
}

SEXP ps__tty_size() {
  CONSOLE_SCREEN_BUFFER_INFO info;

  BOOL ok = GetConsoleScreenBufferInfo(
    GetStdHandle(STD_OUTPUT_HANDLE),
    &info
  );

  if (!ok) {
    ps__set_error_from_windows_error(0);
    ps__throw_error();
  }

  SEXP result = Rf_allocVector(INTSXP, 2);
  INTEGER(result)[0] = info.srWindow.Right  - info.srWindow.Left + 1;
  INTEGER(result)[1] = info.srWindow.Bottom - info.srWindow.Top + 1;

  return result;
}

static char *ps__get_drive_type(int type) {
  switch (type) {
  case DRIVE_FIXED:
    return "fixed";
  case DRIVE_CDROM:
    return "cdrom";
  case DRIVE_REMOVABLE:
    return "removable";
  case DRIVE_UNKNOWN:
    return "unknown";
  case DRIVE_NO_ROOT_DIR:
    return "unmounted";
  case DRIVE_REMOTE:
    return "remote";
  case DRIVE_RAMDISK:
    return "ramdisk";
  default:
    return "?";
  }
}



SEXP ps__disk_partitions(SEXP rall) {
  DWORD num_bytes;
  char drive_strings[255];
  char *drive_letter = drive_strings;
  char mp_buf[MAX_PATH];
  char mp_path[MAX_PATH];
  int all = LOGICAL(rall)[0];
  int type;
  int ret;
  unsigned int old_mode = 0;
  char opts[20];
  HANDLE mp_h;
  BOOL mp_flag= TRUE;
  LPTSTR fs_type[MAX_PATH + 1] = { 0 };
  DWORD pflags = 0;

  SEXP result;
  PROTECT_INDEX pidx;
  int len = 10, num = -1;
  PROTECT_WITH_INDEX(result = allocVector(VECSXP, len), &pidx);

  // avoid to visualize a message box in case something goes wrong
  // see https://github.com/giampaolo/psutil/issues/264
  old_mode = SetErrorMode(SEM_FAILCRITICALERRORS);

  num_bytes = GetLogicalDriveStrings(254, drive_letter);

  if (num_bytes == 0) {
    ps__set_error_from_errno();
    goto error;
  }

  while (*drive_letter != 0) {
    opts[0] = 0;
    fs_type[0] = 0;

    type = GetDriveType(drive_letter);

    // by default we only show hard drives and cd-roms
    if (all == 0) {
      if ((type == DRIVE_UNKNOWN) ||
          (type == DRIVE_NO_ROOT_DIR) ||
          (type == DRIVE_REMOTE) ||
          (type == DRIVE_RAMDISK)) {
        goto next;
      }
      // floppy disk: skip it by default as it introduces a
      // considerable slowdown.
      if ((type == DRIVE_REMOVABLE) &&
          (strcmp(drive_letter, "A:\\")  == 0)) {
        goto next;
      }
    }

    ret = GetVolumeInformation(
      (LPCTSTR)drive_letter, NULL, _ARRAYSIZE(drive_letter),
      NULL, NULL, &pflags, (LPTSTR)fs_type, _ARRAYSIZE(fs_type));
    if (ret == 0) {
      // We might get here in case of a floppy hard drive, in
      // which case the error is (21, "device not ready").
      // Let's pretend it didn't happen as we already have
      // the drive name and type ('removable').
      strcat_s(opts, _countof(opts), "");
      SetLastError(0);
    } else {
      if (pflags & FILE_READ_ONLY_VOLUME) {
        strcat_s(opts, _countof(opts), "ro");
      } else {
        strcat_s(opts, _countof(opts), "rw");
      }
      if (pflags & FILE_VOLUME_IS_COMPRESSED) {
        strcat_s(opts, _countof(opts), ",compressed");
      }

      // Check for mount points on this volume and add/get info
      // (checks first to know if we can even have mount points)
      if (pflags & FILE_SUPPORTS_REPARSE_POINTS) {
        mp_h = FindFirstVolumeMountPoint(
          drive_letter, mp_buf, MAX_PATH);
        if (mp_h != INVALID_HANDLE_VALUE) {
          while (mp_flag) {

            // Append full mount path with drive letter
            strcpy_s(mp_path, _countof(mp_path), drive_letter);
            strcat_s(mp_path, _countof(mp_path), mp_buf);

            SetErrorMode(old_mode);
            // TODO: should call FindVolumeMountPointClose on error
            if (++num == len) {
              len *= 2;
              REPROTECT(result = Rf_lengthgets(result, len), pidx);
            }
            SET_VECTOR_ELT(
              result, num,
              ps__build_list(
                "ssss", drive_letter, mp_path, fs_type, opts));
            SetErrorMode(SEM_FAILCRITICALERRORS);

            // Continue looking for more mount points
            mp_flag = FindNextVolumeMountPoint(
              mp_h, mp_buf, MAX_PATH);
          }
          FindVolumeMountPointClose(mp_h);
        }
      }
    }

    if (strlen(opts) > 0) {
      strcat_s(opts, _countof(opts), ",");
    }
    strcat_s(opts, _countof(opts), ps__get_drive_type(type));

    SetErrorMode(old_mode);

    if (++num == len) {
      len *= 2;
      REPROTECT(result = Rf_lengthgets(result, len), pidx);
    }
    SET_VECTOR_ELT(
      result, num,
      ps__build_list(
        "ssss", drive_letter, drive_letter, fs_type, opts));

    next:
      drive_letter = strchr(drive_letter, 0) + 1;
  }

  UNPROTECT(1);
  return result;

  error:
    SetErrorMode(old_mode);
    return R_NilValue;
}

SEXP ps__disk_usage(SEXP paths) {
  BOOL retval;
  ULARGE_INTEGER freeuser, total, free;
  int i, n = Rf_length(paths);
  SEXP result = PROTECT(allocVector(VECSXP, n));

  for (i = 0; i < n; i++) {
    const char *path = CHAR(STRING_ELT(paths, i));
    wchar_t *wpath;
    int iret = ps__utf8_to_utf16(path, &wpath);
    if (iret) goto error;
    retval = GetDiskFreeSpaceExW(wpath, &freeuser, &total, &free);
    if (!retval) {
      ps__set_error_from_windows_error(0);
      goto error;
    }
    SET_VECTOR_ELT(
      result, i,
      allocVector(REALSXP, 3));
    REAL(VECTOR_ELT(result, i))[0] = (double) total.QuadPart;
    REAL(VECTOR_ELT(result, i))[1] = (double) free.QuadPart;
    REAL(VECTOR_ELT(result, i))[2] = (double) freeuser.QuadPart;
  }

  UNPROTECT(1);
  return result;

error:
  ps__throw_error();
  return R_NilValue;
}

SEXP ps__system_memory() {
  MEMORYSTATUSEX memInfo;
  memInfo.dwLength = sizeof(MEMORYSTATUSEX);

  if (! GlobalMemoryStatusEx(&memInfo)) {
    ps__set_error_from_windows_error(0);
    ps__throw_error();
  }

  return ps__build_named_list(
    "dddddd",
    "total",      (double) memInfo.ullTotalPhys,
    "avail",      (double) memInfo.ullAvailPhys,
    "totpagef",   (double) memInfo.ullTotalPageFile,
    "availpagef", (double) memInfo.ullAvailPageFile,
    "totvirt",    (double) memInfo.ullTotalVirtual,
    "freevirt",   (double) memInfo.ullAvailVirtual);
}

SEXP ps__system_swap() {
  // We use ps__system_memory instead
  ps__throw_error();
  return R_NilValue;
}

SEXP psll_get_nice(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  HANDLE hProcess = NULL;
  DWORD priority;

  if (!handle) error("Process pointer cleaned up already");

  hProcess = ps__handle_from_pid(handle->pid);
  if (!hProcess) goto error;

  priority = GetPriorityClass(hProcess);
  if (priority == 0) {
    ps__set_error_from_windows_error(0);
    goto error;
  }

  CloseHandle(hProcess);

  switch(priority) {
  case REALTIME_PRIORITY_CLASS:
    return ScalarInteger(1);
  case HIGH_PRIORITY_CLASS:
    return ScalarInteger(2);
  case ABOVE_NORMAL_PRIORITY_CLASS:
    return ScalarInteger(3);
  case NORMAL_PRIORITY_CLASS:
    return ScalarInteger(4);
  case IDLE_PRIORITY_CLASS:
    return ScalarInteger(5);
  case BELOW_NORMAL_PRIORITY_CLASS:
    return ScalarInteger(6);
  default:
    error("Internal error, invalid priority class");
  }

error:
  if (hProcess) CloseHandle(hProcess);
  ps__throw_error();
  return R_NilValue;
}

SEXP psll_set_nice(SEXP p, SEXP value) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  HANDLE hProcess = NULL;
  DWORD priority = INTEGER(value)[0];
  DWORD classes[] = {
    REALTIME_PRIORITY_CLASS,
    HIGH_PRIORITY_CLASS,
    ABOVE_NORMAL_PRIORITY_CLASS,
    NORMAL_PRIORITY_CLASS,
    IDLE_PRIORITY_CLASS,
    BELOW_NORMAL_PRIORITY_CLASS
  };
  DWORD access = PROCESS_QUERY_INFORMATION | PROCESS_SET_INFORMATION;

  if (!handle) error("Process pointer cleaned up already");

  hProcess = ps__handle_from_pid_waccess(handle->pid, access);
  if (!hProcess) {
    PS__CHECK_HANDLE(handle);
    ps__set_error_from_windows_error(0);
    goto error;
  }

  BOOL ret = SetPriorityClass(hProcess, classes[priority - 1]);
  if (!ret) {
    ps__set_error_from_windows_error(0);
    goto error;
  }

  CloseHandle(hProcess);

  return R_NilValue;

error:
  if (hProcess) CloseHandle(hProcess);
  ps__throw_error();
  return R_NilValue;

}


SEXP psll_get_cpu_aff(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  HANDLE hProcess = NULL;
  DWORD access = PROCESS_QUERY_LIMITED_INFORMATION;
  DWORD_PTR proc_mask;
  DWORD_PTR system_mask;

  if (!handle) error("Process pointer cleaned up already");

  hProcess = ps__handle_from_pid_waccess(handle->pid, access);
  if (!hProcess) {
    PS__CHECK_HANDLE(handle);
    ps__set_error_from_windows_error(0);
    goto error;
  }

  BOOL ret = GetProcessAffinityMask(hProcess, &proc_mask, &system_mask);
  if (!ret) {
    ps__set_error_from_windows_error(0);
    goto error;
  }

  CloseHandle(hProcess);

  unsigned long long n = (unsigned long long) proc_mask;
  int count = 0, w = 0;
  while (n) {
    count += n & 1;
    n >>= 1;
  }
  SEXP result = PROTECT(Rf_allocVector(INTSXP, count));
  n = (unsigned long long) proc_mask;
  count = 0;
  while (n) {
    if (n & 1) {
      INTEGER(result)[count++] = w;
    }
    n >>= 1;
    w++;
  }

  UNPROTECT(1);
  return result;

error:
  if (hProcess) CloseHandle(hProcess);
  ps__throw_error();
  return R_NilValue;
}

SEXP psll_set_cpu_aff(SEXP p, SEXP affinity) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  HANDLE hProcess = NULL;
  DWORD access = PROCESS_QUERY_INFORMATION | PROCESS_SET_INFORMATION;
  DWORD_PTR mask = 0;

  if (!handle) error("Process pointer cleaned up already");

  hProcess = ps__handle_from_pid_waccess(handle->pid, access);
  if (!hProcess) {
    PS__CHECK_HANDLE(handle);
    ps__set_error_from_windows_error(0);
    goto error;
  }

  int *caff = INTEGER(affinity);
  int i, len = LENGTH(affinity);
  for (i = 0; i < len; i++) {
    int m = 1 << caff[i];
    mask |= m;
  }

  BOOL ret = SetProcessAffinityMask(hProcess, mask);
  if (!ret) {
    PS__CHECK_HANDLE(handle);
    ps__set_error_from_windows_error(0);
    goto error;
  }

  CloseHandle(hProcess);

  return R_NilValue;

error:
  if (hProcess) CloseHandle(hProcess);
  ps__throw_error();
  return R_NilValue;
}

SEXP ps__loadavg(SEXP counter_name) {
  SEXP ret = PROTECT(allocVector(REALSXP, 3));
  ps__get_loadavg(REAL(ret), counter_name);
  UNPROTECT(1);
  return ret;
}

#define LO_T 1e-7
#define HI_T 429.4967296

SEXP ps__system_cpu_times() {
  double idle, kernel, user, system;
  FILETIME idle_time, kernel_time, user_time;

  if (!GetSystemTimes(&idle_time, &kernel_time, &user_time)) {
    ps__set_error_from_windows_error(0);
    ps__throw_error();
  }

  idle = (double)((HI_T * idle_time.dwHighDateTime) +
		  (LO_T * idle_time.dwLowDateTime));
  user = (double)((HI_T * user_time.dwHighDateTime) +
		  (LO_T * user_time.dwLowDateTime));
  kernel = (double)((HI_T * kernel_time.dwHighDateTime) +
		    (LO_T * kernel_time.dwLowDateTime));

  // Kernel time includes idle time.
  // We return only busy kernel time subtracting idle time from
  // kernel time.
  system = (kernel - idle);

  const char *nms[] = { "user", "system", "idle", "" };
  SEXP ret = PROTECT(Rf_mkNamed(REALSXP, nms));

  REAL(ret)[0] = (double) user;
  REAL(ret)[1] = (double) system;
  REAL(ret)[2] = (double) idle;

  UNPROTECT(1);
  return ret;
}
