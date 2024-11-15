
#include "common.h"
#include "windows.h"
#include "arch/windows/process_info.h"
#include "arch/windows/process_handles.h"
#include "cleancall.h"

#include <tlhelp32.h>
#include <string.h>
#include <math.h>
#include <wtsapi32.h>
#include <time.h>

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

SEXP psll_kill(SEXP p, SEXP grace) {
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

#ifndef FILE_RETURNS_CLEANUP_RESULT_INFO
#define FILE_RETURNS_CLEANUP_RESULT_INFO 0x00000200
#endif
#ifndef FILE_SUPPORTS_POSIX_UNLINK_RENAME
#define FILE_SUPPORTS_POSIX_UNLINK_RENAME 0x00000400
#endif
#ifndef FILE_SUPPORTS_BLOCK_REFCOUNTING
#define FILE_SUPPORTS_BLOCK_REFCOUNTING 0x08000000
#endif
#ifndef FILE_SUPPORTS_SPARSE_VDL
#define FILE_SUPPORTS_SPARSE_VDL 0x10000000
#endif
#ifndef FILE_DAX_VOLUME
#define FILE_DAX_VOLUME 0x20000000
#endif
#ifndef FILE_SUPPORTS_GHOSTING
#define FILE_SUPPORTS_GHOSTING 0x40000000
#endif

SEXP ps__fs_info(SEXP path, SEXP abspath, SEXP mps) {
  R_xlen_t i, len = Rf_xlength(path);

  const char *nms[] = {
    "path",                           // 0
    "mountpoint",                    // 1
    "name",                           // 2
    "type",                           // 3
    "block_size",                     // 4
    "transfer_block_size",            // 5
    "total_data_blocks",              // 6
    "free_blocks",                    // 7
    "free_blocks_non_superuser",      // 8
    "total_nodes",                    // 9
    "free_nodes",                     // 10
    "id",                             // 11
    "owner",                          // 12
    "type_code",                      // 13
    "subtype_code",                   // 14

    "CASE_SENSITIVE_SEARCH",
    "CASE_PRESERVED_NAMES",
    "UNICODE_ON_DISK",
    "PERSISTENT_ACLS",
    "FILE_COMPRESSION",
    "VOLUME_QUOTAS",
    "SUPPORTS_SPARSE_FILES",
    "SUPPORTS_REPARSE_POINTS",
    "SUPPORTS_REMOTE_STORAGE",
    "RETURNS_CLEANUP_RESULT_INFO",
    "SUPPORTS_POSIX_UNLINK_RENAME",
    "VOLUME_IS_COMPRESSED",
    "SUPPORTS_OBJECT_IDS",
    "SUPPORTS_ENCRYPTION",
    "NAMED_STREAMS",
    "READ_ONLY_VOLUME",
    "SEQUENTIAL_WRITE_ONCE",
    "SUPPORTS_TRANSACTIONS",
    "SUPPORTS_HARD_LINKS",
    "SUPPORTS_EXTENDED_ATTRIBUTES",
    "SUPPORTS_OPEN_BY_FILE_ID",
    "SUPPORTS_USN_JOURNAL",
    "SUPPORTS_INTEGRITY_STREAMS",
    "SUPPORTS_BLOCK_REFCOUNTING",
    "SUPPORTS_SPARSE_VDL",
    "DAX_VOLUME",
    "SUPPORTS_GHOSTING",
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
  SET_VECTOR_ELT(res, 34, Rf_allocVector(LGLSXP, len));
  SET_VECTOR_ELT(res, 35, Rf_allocVector(LGLSXP, len));
  SET_VECTOR_ELT(res, 36, Rf_allocVector(LGLSXP, len));
  SET_VECTOR_ELT(res, 37, Rf_allocVector(LGLSXP, len));
  SET_VECTOR_ELT(res, 38, Rf_allocVector(LGLSXP, len));
  SET_VECTOR_ELT(res, 39, Rf_allocVector(LGLSXP, len));
  SET_VECTOR_ELT(res, 40, Rf_allocVector(LGLSXP, len));
  SET_VECTOR_ELT(res, 41, Rf_allocVector(LGLSXP, len));

  for (i = 0; i < len; i++) {
    const char *cpath = CHAR(STRING_ELT(abspath, i));
    wchar_t *wpath;
    int iret = ps__utf8_to_utf16(cpath, &wpath);
    if (iret) {
      ps__throw_error();
    }

    // we already have the mount point, convert to UTF-16
    wchar_t *wmp;
    iret = ps__utf8_to_utf16(CHAR(STRING_ELT(mps, i)), &wmp);
    if (iret) {
      ps__throw_error();
    }
    SET_STRING_ELT(VECTOR_ELT(res, 1), i, STRING_ELT(mps, i));

    // name of the volume
    wchar_t volname[1024];
    BOOL ok = GetVolumeNameForVolumeMountPointW(
      wmp,
      volname,
      sizeof(volname)/sizeof(wchar_t) - 1
    );
    if (!ok) {
      ps__set_error_from_windows_error(0);
      ps__throw_error();
    }
    SET_STRING_ELT(
      VECTOR_ELT(res, 2), i,
      ps__utf16_to_charsxp(volname, -1)
    );

    DWORD sn, mcl, flags;
    wchar_t type[MAX_PATH + 1];
    ok = GetVolumeInformationW(
      wmp, NULL, 0, &sn, &mcl, &flags, type,
      sizeof(type)/sizeof(wchar_t) - 1);
    if (!ok) {
      ps__set_error_from_windows_error(0);
      ps__throw_error();
    }
    // type
    SET_STRING_ELT(
      VECTOR_ELT(res, 3), i,
      ps__utf16_to_charsxp(type, -1)
    );

    DWORD spc, bps, freec, totalc;
    ok = GetDiskFreeSpaceW(wmp, &spc, &bps, &freec, &totalc);
    if (!ok) {
      ps__set_error_from_windows_error(0);
      ps__throw_error();
    }
    REAL(VECTOR_ELT(res, 4))[i] = bps;
    REAL(VECTOR_ELT(res, 5))[i] = bps * spc;

    ULARGE_INTEGER freeuser, total, freeroot;
    ok = GetDiskFreeSpaceExW(wmp, &freeuser, &total, &freeroot);
    if (!ok) {
      ps__set_error_from_windows_error(0);
      ps__throw_error();
    }
    REAL(VECTOR_ELT(res, 6))[i] = total.QuadPart / bps;
    REAL(VECTOR_ELT(res, 7))[i] = freeroot.QuadPart / bps;
    REAL(VECTOR_ELT(res, 8))[i] = freeuser.QuadPart / bps;
    REAL(VECTOR_ELT(res, 9))[i] = NA_REAL;
    REAL(VECTOR_ELT(res, 10))[i] = NA_REAL;
    SET_VECTOR_ELT(VECTOR_ELT(res, 11), i, R_NilValue);
    REAL(VECTOR_ELT(res, 12))[i] = NA_REAL;
    REAL(VECTOR_ELT(res, 13))[i] = NA_REAL;
    REAL(VECTOR_ELT(res, 14))[i] = NA_REAL;

    LOGICAL(VECTOR_ELT(res, 15))[i] = flags & FILE_CASE_SENSITIVE_SEARCH;
    LOGICAL(VECTOR_ELT(res, 16))[i] = flags & FILE_CASE_PRESERVED_NAMES;
    LOGICAL(VECTOR_ELT(res, 17))[i] = flags & FILE_UNICODE_ON_DISK;
    LOGICAL(VECTOR_ELT(res, 18))[i] = flags & FILE_PERSISTENT_ACLS;
    LOGICAL(VECTOR_ELT(res, 19))[i] = flags & FILE_FILE_COMPRESSION;
    LOGICAL(VECTOR_ELT(res, 20))[i] = flags & FILE_VOLUME_QUOTAS;
    LOGICAL(VECTOR_ELT(res, 21))[i] = flags & FILE_SUPPORTS_SPARSE_FILES;
    LOGICAL(VECTOR_ELT(res, 22))[i] = flags & FILE_SUPPORTS_REPARSE_POINTS;
    LOGICAL(VECTOR_ELT(res, 23))[i] = flags & FILE_SUPPORTS_REMOTE_STORAGE;
    LOGICAL(VECTOR_ELT(res, 24))[i] = flags & FILE_RETURNS_CLEANUP_RESULT_INFO;
    LOGICAL(VECTOR_ELT(res, 25))[i] = flags & FILE_SUPPORTS_POSIX_UNLINK_RENAME;
    LOGICAL(VECTOR_ELT(res, 26))[i] = flags & FILE_VOLUME_IS_COMPRESSED;
    LOGICAL(VECTOR_ELT(res, 27))[i] = flags & FILE_SUPPORTS_OBJECT_IDS;
    LOGICAL(VECTOR_ELT(res, 28))[i] = flags & FILE_SUPPORTS_ENCRYPTION;
    LOGICAL(VECTOR_ELT(res, 29))[i] = flags & FILE_NAMED_STREAMS;
    LOGICAL(VECTOR_ELT(res, 30))[i] = flags & FILE_READ_ONLY_VOLUME;
    LOGICAL(VECTOR_ELT(res, 31))[i] = flags & FILE_SEQUENTIAL_WRITE_ONCE;
    LOGICAL(VECTOR_ELT(res, 32))[i] = flags & FILE_SUPPORTS_TRANSACTIONS;
    LOGICAL(VECTOR_ELT(res, 33))[i] = flags & FILE_SUPPORTS_HARD_LINKS;
    LOGICAL(VECTOR_ELT(res, 34))[i] = flags & FILE_SUPPORTS_EXTENDED_ATTRIBUTES;
    LOGICAL(VECTOR_ELT(res, 35))[i] = flags & FILE_SUPPORTS_OPEN_BY_FILE_ID;
    LOGICAL(VECTOR_ELT(res, 36))[i] = flags & FILE_SUPPORTS_USN_JOURNAL;
    LOGICAL(VECTOR_ELT(res, 37))[i] = flags & FILE_SUPPORTS_INTEGRITY_STREAMS;
    LOGICAL(VECTOR_ELT(res, 38))[i] = flags & FILE_SUPPORTS_BLOCK_REFCOUNTING;
    LOGICAL(VECTOR_ELT(res, 39))[i] = flags & FILE_SUPPORTS_SPARSE_VDL;
    LOGICAL(VECTOR_ELT(res, 40))[i] = flags & FILE_DAX_VOLUME;
    LOGICAL(VECTOR_ELT(res, 41))[i] = flags & FILE_SUPPORTS_GHOSTING;
  }

  UNPROTECT(1);
  return res;
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

SEXP ps__disk_io_counters() {
    // Based on the implementation from psutil
    DISK_PERFORMANCE diskPerformance;
    DWORD dwSize;
    HANDLE hDevice = NULL;
    char szDevice[MAX_PATH];
    char szDeviceDisplay[MAX_PATH];
    int devNum;
    int i;
    DWORD ioctrlSize;
    BOOL ret;

    SEXP result = PROTECT(allocVector(VECSXP, 7));
    SEXP device = PROTECT(allocVector(STRSXP, 32));
    SEXP read_count = PROTECT(allocVector(REALSXP, 32));
    SEXP read_bytes = PROTECT(allocVector(REALSXP, 32));
    SEXP read_time = PROTECT(allocVector(REALSXP, 32));
    SEXP write_count = PROTECT(allocVector(REALSXP, 32));
    SEXP write_bytes = PROTECT(allocVector(REALSXP, 32));
    SEXP write_time = PROTECT(allocVector(REALSXP, 32));

    // Apparently there's no way to figure out how many times we have
    // to iterate in order to find valid drives.
    // Let's assume 32, which is higher than 26, the number of letters
    // in the alphabet (from A:\ to Z:\).
    for (devNum = 0; devNum <= 32; ++devNum) {
        sprintf_s(szDevice, MAX_PATH, "\\\\.\\PhysicalDrive%d", devNum);
        hDevice = CreateFile(szDevice, 0, FILE_SHARE_READ | FILE_SHARE_WRITE,
                             NULL, OPEN_EXISTING, 0, NULL);
        if (hDevice == INVALID_HANDLE_VALUE)
            continue;

        // DeviceIoControl() sucks!
        i = 0;
        ioctrlSize = sizeof(diskPerformance);
        while (1) {
            i += 1;
            ret = DeviceIoControl(
                hDevice, IOCTL_DISK_PERFORMANCE, NULL, 0, &diskPerformance,
                ioctrlSize, &dwSize, NULL);
            if (ret != 0)
                break;  // OK!
            if (GetLastError() == ERROR_INSUFFICIENT_BUFFER) {
                // Retry with a bigger buffer (+ limit for retries).
                if (i <= 1024) {
                    ioctrlSize *= 2;
                    continue;
                }
            }
            else if (GetLastError() == ERROR_INVALID_FUNCTION) {
                // This happens on AppVeyor:
                // https://ci.appveyor.com/project/giampaolo/psutil/build/
                //      1364/job/ascpdi271b06jle3
                // Assume it means we're dealing with some exotic disk
                // and go on.
                ps__debug("DeviceIoControl -> ERROR_INVALID_FUNCTION; "
                             "ignore PhysicalDrive%i", devNum);
                goto next;
            }
            else if (GetLastError() == ERROR_NOT_SUPPORTED) {
                // Again, let's assume we're dealing with some exotic disk.
                ps__debug("DeviceIoControl -> ERROR_NOT_SUPPORTED; "
                             "ignore PhysicalDrive%i", devNum);
                goto next;
            }
            // XXX: it seems we should also catch ERROR_INVALID_PARAMETER:
            // https://sites.ualberta.ca/dept/aict/uts/software/openbsd/
            //     ports/4.1/i386/openafs/w-openafs-1.4.14-transarc/
            //     openafs-1.4.14/src/usd/usd_nt.c

            // XXX: we can also bump into ERROR_MORE_DATA in which case
            // (quoting doc) we're supposed to retry with a bigger buffer
            // and specify  a new "starting point", whatever it means.
            ps__set_error_from_windows_error(0);
            goto error;
        }

        sprintf_s(szDeviceDisplay, MAX_PATH, "PhysicalDrive%i", devNum);
        SET_STRING_ELT(device, devNum, mkChar(szDeviceDisplay));
        REAL(read_count)[devNum] = diskPerformance.ReadCount;
        REAL(read_bytes)[devNum] =  diskPerformance.BytesRead.QuadPart;
        REAL(read_time)[devNum] = (unsigned long long) (diskPerformance.ReadTime.QuadPart) / 10000000;
        REAL(write_count)[devNum] = diskPerformance.WriteCount;
        REAL(write_bytes)[devNum] = diskPerformance.BytesWritten.QuadPart;
        REAL(write_time)[devNum] = (unsigned long long) (diskPerformance.WriteTime.QuadPart) / 10000000;

next:
        CloseHandle(hDevice);
    }

    SET_VECTOR_ELT(result, 0, device);
    SET_VECTOR_ELT(result, 1, read_count);
    SET_VECTOR_ELT(result, 2, read_bytes);
    SET_VECTOR_ELT(result, 3, read_time);
    SET_VECTOR_ELT(result, 4, write_count);
    SET_VECTOR_ELT(result, 5, write_bytes);
    SET_VECTOR_ELT(result, 6, write_time);

    UNPROTECT(8);

    return result;

error:
    if (hDevice != NULL)
        CloseHandle(hDevice);
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
  R_xlen_t num_handles;
  HANDLE *pfds;
};

static void psll__wait_cleanup(void *data) {
  struct psll__wait_cleanup_data *cdata =
    (struct psll__wait_cleanup_data*) data;
  R_xlen_t i;
  if (cdata->pfds) {
    for (i = 0; i < cdata->num_handles; i++) {
      if (cdata->pfds[i] != INVALID_HANDLE_VALUE) {
	CloseHandle(cdata->pfds[i]);
      }
    }
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

  struct psll__wait_cleanup_data cdata = {
    /* num_handles= */ num_handles,
    /* pfds= */        NULL
  };
  r_call_on_early_exit(psll__wait_cleanup, &cdata);

  SEXP res = PROTECT(Rf_allocVector(LGLSXP, num_handles));
  SEXP rhandles = PROTECT(Rf_allocVector(RAWSXP, sizeof(HANDLE) * num_handles));
  cdata.pfds = (HANDLE*) RAW(rhandles);

  R_xlen_t topoll = 0;
  for (i = 0; i < num_handles; i++) {
    ps_handle_t *handle = R_ExternalPtrAddr(VECTOR_ELT(pps, i));
    if (!handle) Rf_error("Process pointer #%d cleaned up already", (int) i);
    if (!LOGICAL(psll_is_running(VECTOR_ELT(pps, i)))[0]) {
      // already done
      LOGICAL(res)[i] = 1;
      cdata.pfds[i] = INVALID_HANDLE_VALUE;
    } else {
      cdata.pfds[i] = OpenProcess(SYNCHRONIZE, FALSE, handle->pid);
      if (cdata.pfds[i] == NULL) {
	if (GetLastError() == 87) {
	  LOGICAL(res)[i] = 1;
	  cdata.pfds[i] = INVALID_HANDLE_VALUE;
	} else {
	  ps__set_error_from_windows_error(0);
	  ps__throw_error();
	}
      } else {
	cdata.pfds[topoll] = cdata.pfds[i];
	topoll++;
	LOGICAL(res)[i] = 0;
      }
    }
  }

  // early exit if nothing to do
  if (topoll == 0) {
    psll__wait_cleanup(&cdata);
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

  // WaitForMultipleObjects can wait on at most 64 processes. If we have
  // more than that, we poll the first 64, and then if all of them are
  // done within the timeout, we poll the next 64, etc.
  int first = 0;
  topoll = num_handles;
  DWORD ret;
  do {
    topoll = topoll > 64 ? 64 : topoll;
    ret = WaitForMultipleObjects(topoll, cdata.pfds + first, TRUE, ts);
    if (ret == WAIT_FAILED) {
      ps__set_error_from_windows_error(0);
      ps__throw_error();
    }

    // all of them (or 64) are done
    if (ret != WAIT_TIMEOUT) {
      for (i = first; i < first + topoll; i++) {
	LOGICAL(res)[i] = 1;
      }
      first += topoll;
      topoll = num_handles - first;
      if (topoll == 0) break;
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

  // we don't know which finished, need to check
  if (topoll > 0) {
    for (i = 0; i < num_handles; i++) {
      if (cdata.pfds[i] != INVALID_HANDLE_VALUE) {
	ret = WaitForSingleObject(cdata.pfds[i], 0);
	if (ret == WAIT_FAILED) {
	  ps__set_error_from_windows_error(0);
	  ps__throw_error();
	}
	if (ret == WAIT_OBJECT_0) {
	  LOGICAL(res)[i] = 1;
	}
      }
    }
  }

  psll__wait_cleanup(&cdata);
  UNPROTECT(2);
  return res;
}

SEXP ps__stat(SEXP paths, SEXP follow) {
  Rf_error("ps__stat is not implemented on Windows");
}

SEXP ps__mount_point(SEXP paths) {
  R_xlen_t i, len = Rf_xlength(paths);
  SEXP res = PROTECT(Rf_allocVector(STRSXP, len));

  for (i = 0; i < len; i++) {
    const char *cpath = CHAR(STRING_ELT(paths, i));
    wchar_t *wpath;
    int iret = ps__utf8_to_utf16(cpath, &wpath);
    if (iret) {
      ps__throw_error();
    }

    // look up mount point
    wchar_t volume[MAX_PATH + 1];
    BOOL ok = GetVolumePathNameW(
      wpath,
      volume,
      sizeof(volume)/sizeof(wchar_t) - 1
    );
    if (!ok) {
      ps__set_error_from_windows_error(0);
      ps__throw_error();
    }
    SET_STRING_ELT(res, i, ps__utf16_to_charsxp(volume, -1));
  }

  UNPROTECT(1);
  return res;
}
