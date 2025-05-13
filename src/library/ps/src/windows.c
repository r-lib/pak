
// Fixes clash between winsock2.h and windows.h
#define WIN32_LEAN_AND_MEAN

#include <windows.h>
#include <Psapi.h>
#include <time.h>
#include <lm.h>
#include <WinIoCtl.h>
#include <tchar.h>
#include <tlhelp32.h>
#include <winsock2.h>
#if (_WIN32_WINNT >= 0x0600) // Windows Vista and above
#include <ws2tcpip.h>
#endif
#if defined(__MINGW32__)
#include <Wincrypt.h>
#endif
#include <iphlpapi.h>
#include <wtsapi32.h>
#include <Winsvc.h>
#include <PowrProf.h>
#include <signal.h>

#include "common.h"
#include "arch/windows/process_info.h"
#ifndef __MINGW32__
#include "arch/windows/ntextapi.h"
#else
#include <winternl.h>
#include <ws2tcpip.h>
#endif

#include <stdlib.h>

#include "windows.h"

/*
 * ============================================================================
 * Utilities
 * ============================================================================
 */

#ifndef AF_INET6
#define AF_INET6 23
#endif

#define LO_T 1e-7
#define HI_T 429.4967296

/*
 * Return an integer vector of all the PIDs running on the system.
 */
SEXP ps__pids() {
  DWORD *proclist = NULL;
  DWORD numberOfReturnedPIDs;
  DWORD i;
  SEXP retlist;

  proclist = ps__get_pids(&numberOfReturnedPIDs);
  if (proclist == NULL)
    ps__throw_error();

  PROTECT(retlist = allocVector(INTSXP, numberOfReturnedPIDs));

  for (i = 0; i < numberOfReturnedPIDs; i++) {
    INTEGER(retlist)[i] = proclist[i];
  }

  // free C array allocated for PIDs
  free(proclist);
  UNPROTECT(1);
  return retlist;
}

/*
 * Kill a process given its PID.
 */
SEXP ps__proc_kill(DWORD pid) {
  HANDLE hProcess;
  DWORD err;

  if (pid == 0) {
    ps__access_denied("");
    return R_NilValue;
  }

  hProcess = OpenProcess(PROCESS_TERMINATE, FALSE, pid);
  if (hProcess == NULL) {
    if (GetLastError() == ERROR_INVALID_PARAMETER) {
      // see https://github.com/giampaolo/psutil/issues/24
      ps__debug("OpenProcess -> ERROR_INVALID_PARAMETER turned "
		   "into NoSuchProcess");
      ps__no_such_process(pid, 0);
    } else {
      ps__set_error_from_windows_error(0);
    }
    return R_NilValue;
  }

  // kill the process
  if (! TerminateProcess(hProcess, SIGTERM)) {
    err = GetLastError();
    // See: https://github.com/giampaolo/psutil/issues/1099
    if (err != ERROR_ACCESS_DENIED) {
      CloseHandle(hProcess);
      ps__set_error_from_windows_error(0);
      return R_NilValue;
    }
  }

  CloseHandle(hProcess);
  return ScalarLogical(1);
}


/*
 * Return a list (user_time, kernel_time)
 */
SEXP ps__proc_cpu_times(DWORD pid) {
  HANDLE      hProcess;
  FILETIME    ftCreate, ftExit, ftKernel, ftUser;
  SEXP result, names;

  hProcess = ps__handle_from_pid(pid);
  if (hProcess == NULL)
    return R_NilValue;
  if (! GetProcessTimes(hProcess, &ftCreate, &ftExit, &ftKernel, &ftUser)) {
    CloseHandle(hProcess);
    if (GetLastError() == ERROR_ACCESS_DENIED) {
      // usually means the process has died so we throw a NoSuchProcess
      // here
      ps__no_such_process(pid, 0);
    } else {
      ps__set_error_from_windows_error(0);
    }
    return R_NilValue;
  }

  CloseHandle(hProcess);

  /*
   * User and kernel times are represented as a FILETIME structure
   * wich contains a 64-bit value representing the number of
   * 100-nanosecond intervals since January 1, 1601 (UTC):
   * http://msdn.microsoft.com/en-us/library/ms724284(VS.85).aspx
   * To convert it into a float representing the seconds that the
   * process has executed in user/kernel mode I borrowed the code
   * below from Python's Modules/posixmodule.c
   */

  PROTECT(result = allocVector(REALSXP, 4));
  REAL(result)[0] =
    (double)(ftUser.dwHighDateTime * 429.4967296 + ftUser.dwLowDateTime * 1e-7),
  REAL(result)[1] =
    (double)(ftKernel.dwHighDateTime * 429.4967296 + ftKernel.dwLowDateTime * 1e-7),
  REAL(result)[2] = REAL(result)[3] = NA_REAL;
  PROTECT(names = ps__build_string("user", "system", "children_user",
				   "children_system", NULL));
  setAttrib(result, R_NamesSymbol, names);

  UNPROTECT(2);
  return result;
}


SEXP ps__exe(DWORD pid) {
  HANDLE hProcess;
  wchar_t exe[MAX_PATH];
  wchar_t *bs = exe;

  if (pid == 0 || pid == 4) return ScalarString(NA_STRING);

  hProcess = ps__handle_from_pid_waccess(pid, PROCESS_QUERY_INFORMATION);
  if (NULL == hProcess) {
    return R_NilValue;
  }

  if (GetProcessImageFileNameW(hProcess, exe, MAX_PATH) == 0) {
    CloseHandle(hProcess);
    ps__set_error_from_windows_error(0);
    return R_NilValue;
  }
  CloseHandle(hProcess);


  /* Convert to DOS path */

  return ps__convert_dos_path(bs);
}

SEXP ps__convert_dos_path(WCHAR *wstr) {

  WCHAR *bs = wstr;
  int nbs = 0;
  wchar_t d = 'A';
  wchar_t device[3] = { d, ':', '\0' };
  wchar_t target[512];

  while (nbs < 3 && *bs) {
    if (*bs == L'\\') nbs++;
    if (nbs == 3) break;
    bs++;
  }

  *bs = L'\0';

  while (d <= 'Z') {
    device[0] = d;
    memset(target, 0, sizeof(wchar_t) * 512);
    if (QueryDosDeviceW(device, target, 511) != 0) {
      if (wcscmp(wstr, target) == 0) break;
    }
    d++;
  }

  if (d > 'Z') {
    ps__set_error("Cannot find device for executable path");
    return R_NilValue;
  }

  *(bs - 2) = d;
  *(bs - 1) = ':';
  *bs = '\\';

  return ScalarString(ps__utf16_to_charsxp(bs - 2, -1));
}

SEXP ps__name(DWORD pid) {
  SEXP exe, name;

  /* This is how PIDs 0 and 4 are always represented in taskmgr */
  /* and process-hacker. */
  if (pid == 0) return mkString("System Idle Process");
  if (pid == 4) return mkString("System");

  exe = ps__exe(pid);
  if (!isNull(exe)) {
    const char *cexe;
    char *last;
    PROTECT(exe);
    cexe = CHAR(STRING_ELT(exe, 0));
    last = strrchr(cexe, '\\');
    if (!last) {
      UNPROTECT(1);
      return exe;
    } else  {
      name = mkString(last + 1);
      UNPROTECT(1);
      return name;
    }
  }

  return ps__proc_name(pid);
}

/*
 * Return process base name.
 * Note: ps__proc_exe() is attempted first because it's faster
 * but it raise AccessDenied for processes owned by other users
 * in which case we fall back on using this.
 */
SEXP ps__proc_name(DWORD pid) {
  int ok;
  PROCESSENTRY32W pentry;
  HANDLE hSnapShot;

  hSnapShot = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if (hSnapShot == INVALID_HANDLE_VALUE) {
    ps__set_error_from_windows_error(0);
    return R_NilValue;
  }
  pentry.dwSize = sizeof(PROCESSENTRY32W);
  ok = Process32FirstW(hSnapShot, &pentry);
  if (! ok) {
    CloseHandle(hSnapShot);
    ps__set_error_from_windows_error(0);
    return R_NilValue;
  }
  while (ok) {
    if (pentry.th32ProcessID == pid) {
      CloseHandle(hSnapShot);
      return ScalarString(ps__utf16_to_charsxp(pentry.szExeFile, -1));
    }
    ok = Process32NextW(hSnapShot, &pentry);
  }

  CloseHandle(hSnapShot);
  ps__no_such_process(pid, 0);
  return R_NilValue;
}


/*
 * Resume or suspends a process
 */
int ps__proc_suspend_or_resume(DWORD pid, int suspend) {
  // a huge thanks to http://www.codeproject.com/KB/threads/pausep.aspx
  HANDLE hThreadSnap = NULL;
  HANDLE hThread;
  THREADENTRY32  te32 = {0};

  if (pid == 0) {
    ps__access_denied("");
    return FALSE;
  }

  hThreadSnap = CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0);
  if (hThreadSnap == INVALID_HANDLE_VALUE) {
    ps__set_error_from_windows_error(0);
    return FALSE;
  }

  // Fill in the size of the structure before using it
  te32.dwSize = sizeof(THREADENTRY32);

  if (! Thread32First(hThreadSnap, &te32)) {
    ps__set_error_from_windows_error(0);
    CloseHandle(hThreadSnap);
    return FALSE;
  }

  // Walk the thread snapshot to find all threads of the process.
  // If the thread belongs to the process, add its information
  // to the display list.
  do {
    if (te32.th32OwnerProcessID == pid) {
      hThread = OpenThread(THREAD_SUSPEND_RESUME, FALSE,
			   te32.th32ThreadID);
      if (hThread == NULL) {
	ps__set_error_from_windows_error(0);
	CloseHandle(hThread);
	CloseHandle(hThreadSnap);
	return FALSE;
      }
      if (suspend == 1) {
	if (SuspendThread(hThread) == (DWORD) - 1) {
	  ps__set_error_from_windows_error(0);
	  CloseHandle(hThread);
	  CloseHandle(hThreadSnap);
	  return FALSE;
	}
      }
      else {
	if (ResumeThread(hThread) == (DWORD) - 1) {
	  ps__set_error_from_windows_error(0);
	  CloseHandle(hThread);
	  CloseHandle(hThreadSnap);
	  return FALSE;
	}
      }
      CloseHandle(hThread);
    }
  } while (Thread32Next(hThreadSnap, &te32));

  CloseHandle(hThreadSnap);
  return TRUE;
}


SEXP ps__proc_suspend(DWORD pid) {
  int suspend = 1;

  if (! ps__proc_suspend_or_resume(pid, suspend)) {
    return R_NilValue;
  } else {
    return ScalarLogical(1);
  }
}


SEXP ps__proc_resume(DWORD pid)  {
  int suspend = 0;

  if (! ps__proc_suspend_or_resume(pid, suspend)) {
    return R_NilValue;
  } else {
    return ScalarLogical(1);
  }
}


/*
 * Return process username as a "DOMAIN//USERNAME" string.
 */
SEXP ps__proc_username(DWORD pid) {
  HANDLE processHandle = NULL;
  HANDLE tokenHandle = NULL;
  PTOKEN_USER user = NULL;
  ULONG bufferSize;
  WCHAR *name = NULL;
  WCHAR *domainName = NULL;
  ULONG nameSize;
  ULONG domainNameSize;
  SID_NAME_USE nameUse;
  SEXP ret;

  processHandle = ps__handle_from_pid_waccess(pid, PROCESS_QUERY_INFORMATION);
  if (processHandle == NULL) {
    return R_NilValue;
  }

  if (!OpenProcessToken(processHandle, TOKEN_QUERY, &tokenHandle)) {
    ps__set_error_from_windows_error(0);
    return R_NilValue;
  }

  CloseHandle(processHandle);
  processHandle = NULL;

  // Get the user SID.
  bufferSize = 0x100;
  while (1) {
    user = malloc(bufferSize);
    if (user == NULL) {
      ps__no_memory("");
      goto error;
    }
    if (!GetTokenInformation(tokenHandle, TokenUser, user, bufferSize,
			     &bufferSize))
      {
	if (GetLastError() == ERROR_INSUFFICIENT_BUFFER) {
	  free(user);
	  continue;
	}
	else {
	  ps__set_error_from_windows_error(0);
	  goto error;
	}
      }
    break;
  }

  CloseHandle(tokenHandle);
  tokenHandle = NULL;

  // resolve the SID to a name
  nameSize = 0x100;
  domainNameSize = 0x100;
  while (1) {
    name = malloc(nameSize * sizeof(WCHAR));
    if (name == NULL) {
      ps__no_memory("");
      goto error;
    }
    domainName = malloc(domainNameSize * sizeof(WCHAR));
    if (domainName == NULL) {
      ps__no_memory("");
      goto error;
    }
    if (!LookupAccountSidW(NULL, user->User.Sid, name, &nameSize,
			   domainName, &domainNameSize, &nameUse))
      {
	if (GetLastError() == ERROR_INSUFFICIENT_BUFFER) {
	  free(name);
	  free(domainName);
	  continue;
	}
	else {
	  ps__set_error_from_windows_error(0);
	  goto error;
	}
      }
    break;
  }

  PROTECT(ret = allocVector(STRSXP, 2));
  SET_STRING_ELT(ret, 0, ps__utf16_to_charsxp(domainName, -1));
  SET_STRING_ELT(ret, 1, ps__utf16_to_charsxp(name, -1));

  free(name);
  free(domainName);
  free(user);

  UNPROTECT(1);
  return ret;

 error:
  if (processHandle != NULL)
    CloseHandle(processHandle);
  if (tokenHandle != NULL)
    CloseHandle(tokenHandle);
  if (name != NULL)
    free(name);
  if (domainName != NULL)
    free(domainName);
  if (user != NULL)
    free(user);
  return R_NilValue;
}


SEXP ps__proc_num_threads(DWORD pid) {
  PSYSTEM_PROCESS_INFORMATION process;
  PVOID buffer;
  int nt;

  if (! ps__get_proc_info(pid, &process, &buffer))  {
    return R_NilValue;
  }

  nt = process->NumberOfThreads;
  free(buffer);
  return ScalarInteger(nt);
}

/*
 * Get various process information by using NtQuerySystemInformation.
 * We use this as a fallback when faster functions fail with access
 * denied. This is slower because it iterates over all processes.
 * Returned tuple includes the following process info:
 *
 * - num_threads()
 * - ctx_switches()
 * - num_handles() (fallback)
 * - cpu_times() (fallback)
 * - create_time() (fallback)
 * - io_counters() (fallback)
 * - memory_info() (fallback)
 */

SEXP ps__proc_info(DWORD pid) {
  PSYSTEM_PROCESS_INFORMATION process;
  PVOID buffer;
  ULONG i;
  ULONG ctx_switches = 0;
  double user_time;
  double kernel_time;
  double create_time;
  SIZE_T mem_private;
  SEXP retlist;

  if (! ps__get_proc_info(pid, &process, &buffer))  {
    return R_NilValue;
  }

  for (i = 0; i < process->NumberOfThreads; i++)
    ctx_switches += process->Threads[i].ContextSwitches;
  user_time = (double)process->UserTime.HighPart * HI_T + \
    (double)process->UserTime.LowPart * LO_T;
  kernel_time = (double)process->KernelTime.HighPart * HI_T + \
    (double)process->KernelTime.LowPart * LO_T;

  if (0 == pid || 4 == pid) {
    SEXP bt = PROTECT(ps__boot_time());
    create_time = REAL(bt)[0];
    UNPROTECT(1);
  } else {
    FILETIME ft;
    ft.dwHighDateTime = process->CreateTime.HighPart;
    ft.dwLowDateTime = process->CreateTime.LowPart;
    create_time = ps__filetime_to_unix(ft);
  }

#if (_WIN32_WINNT >= 0x0501)  // Windows XP with SP2
  mem_private = process->PrivatePageCount;
#else
  mem_private = 0;
#endif

#if defined(_WIN64)
#define BUILD_NAMED_LIST_PROTO "kkdddiKKKKKK" "kKKKKKKKKK"
#else
#define BUILD_NAMED_LIST_PROTO "kkdddiKKKKKK" "kIIIIIIIII"
#endif

  PROTECT(retlist = ps__build_named_list(
    BUILD_NAMED_LIST_PROTO,
    "num_handles",         process->HandleCount,                  /* 0 */
    "ctx_switches",        ctx_switches,	                  /* 1 */
    "user_time",           user_time,		                  /* 2 */
    "kernel_time",         kernel_time,		                  /* 3 */
    "create_time",         create_time,	                          /* 4 */
    "num_threads",         (int)process->NumberOfThreads,         /* 5 */
    // IO counters
    "io_rcount",           process->ReadOperationCount.QuadPart,  /* 6 */
    "io_wcount",           process->WriteOperationCount.QuadPart, /* 7 */
    "io_rbytes",           process->ReadTransferCount.QuadPart,   /* 8 */
    "io_wbytes",           process->WriteTransferCount.QuadPart,  /* 9 */
    "io_count_others",     process->OtherOperationCount.QuadPart, /* 10 */
    "io_bytes_others",     process->OtherTransferCount.QuadPart,  /* 11 */
    // memory
    "num_page_faults",     process->PageFaultCount,               /* 12 */
    "peak_wset",           process->PeakWorkingSetSize,	          /* 13 */
    "wset",                process->WorkingSetSize,               /* 14 */
    "peak_paged_pool",     process->QuotaPeakPagedPoolUsage,      /* 15 */
    "paged_pool",          process->QuotaPagedPoolUsage,          /* 16 */
    "peak_non_paged_pool", process->QuotaPeakNonPagedPoolUsage,   /* 17 */
    "non_paged_oool",      process->QuotaNonPagedPoolUsage,       /* 18 */
    "pagefile",            process->PagefileUsage,                /* 19 */
    "peak_pagefile",       process->PeakPagefileUsage,            /* 20 */
    "mem_private",         mem_private                            /* 21 */
  ));

  free(buffer);
  UNPROTECT(1);
  return retlist;
}


SEXP ps__ppid(DWORD pid) {
  HANDLE handle = NULL;
  PROCESSENTRY32W pe = { 0 };

  pe.dwSize = sizeof(PROCESSENTRY32W);
  handle = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if (handle == INVALID_HANDLE_VALUE) {
    ps__set_error_from_windows_error(0);
    return R_NilValue;
  }

  if (Process32FirstW(handle, &pe)) {
    do {
      if (pe.th32ProcessID == pid) {
	DWORD ppid = pe.th32ParentProcessID;
	CloseHandle(handle);
	return ScalarInteger(ppid);
      }
    } while (Process32NextW(handle, &pe));
  }

  CloseHandle(handle);
  ps__no_such_process(pid, 0);
  return R_NilValue;
}

const WCHAR LONG_PATH_PREFIX[] = L"\\\\?\\";
const WCHAR LONG_PATH_PREFIX_LEN = 4;

const WCHAR UNC_PATH_PREFIX[] = L"\\\\?\\UNC\\";
const WCHAR UNC_PATH_PREFIX_LEN = 8;

typedef DWORD(WINAPI *PFGetFinalPathNameByHandleW)(HANDLE, LPWSTR, DWORD, DWORD);

static PFGetFinalPathNameByHandleW ps__get_fpnbyhandle(void) {
  static PFGetFinalPathNameByHandleW pFunc = NULL;
  PFGetFinalPathNameByHandleW toReturn = pFunc;

  if (!toReturn) {
    HMODULE hModule = GetModuleHandleW(L"kernel32");
    if (hModule)
      toReturn = (PFGetFinalPathNameByHandleW)
	GetProcAddress(hModule, "GetFinalPathNameByHandleW");

    pFunc = toReturn;
  }

  return toReturn;
}

static SEXP psw__realpath_handle(HANDLE handle) {
  DWORD w_realpath_len;
  WCHAR* w_realpath_ptr = NULL;
  WCHAR* w_realpath_buf;

  PFGetFinalPathNameByHandleW GetFinalPathNameByHandleW = ps__get_fpnbyhandle();

  w_realpath_len = GetFinalPathNameByHandleW(handle, NULL, 0,
					     VOLUME_NAME_DOS);
  if (w_realpath_len == 0) {
    return R_NilValue;
  }

  w_realpath_buf = (WCHAR *) R_alloc(w_realpath_len + 1, sizeof(WCHAR));
  if (w_realpath_buf == NULL) {
    SetLastError(ERROR_OUTOFMEMORY);
    return R_NilValue;
  }
  w_realpath_ptr = w_realpath_buf;

  if (GetFinalPathNameByHandleW(
    handle, w_realpath_ptr, w_realpath_len, VOLUME_NAME_DOS) == 0) {
    SetLastError(ERROR_INVALID_HANDLE);
    return R_NilValue;
  }

  /* convert UNC path to long path */
  if (wcsncmp(w_realpath_ptr,
              UNC_PATH_PREFIX,
              UNC_PATH_PREFIX_LEN) == 0) {
    w_realpath_ptr += 6;
    *w_realpath_ptr = L'\\';
    w_realpath_len -= 6;
  } else if (wcsncmp(w_realpath_ptr,
		     LONG_PATH_PREFIX,
		     LONG_PATH_PREFIX_LEN) == 0) {
    w_realpath_ptr += 4;
    w_realpath_len -= 4;
  } else {
    SetLastError(ERROR_INVALID_HANDLE);
    return R_NilValue;
  }

  return ps__utf16_to_strsxp(w_realpath_ptr, w_realpath_len);
}

SEXP psw__realpath(SEXP path) {
  WCHAR *wpath;
  HANDLE handle;
  SEXP res;

  ps__utf8_to_utf16(CHAR(STRING_ELT(path, 0)), &wpath);

  handle = CreateFileW(wpath,
                       0,
                       0,
                       NULL,
                       OPEN_EXISTING,
                       FILE_ATTRIBUTE_NORMAL | FILE_FLAG_BACKUP_SEMANTICS,
                       NULL);
  if (handle == INVALID_HANDLE_VALUE) {
    ps__set_error_from_windows_error(0);
    ps__throw_error();
  }

  PROTECT(res = psw__realpath_handle(handle));
  CloseHandle(handle);
  if (isNull(res)) {
    ps__set_error_from_windows_error(0);
    ps__throw_error();
  }

  UNPROTECT(1);
  return res;
}

SEXP ps__define_tcp_statuses(void) {
  SEXP result, names;

  PROTECT(result = allocVector(INTSXP, 13));
  PROTECT(names = allocVector(STRSXP, 13));

  INTEGER(result)[0] = MIB_TCP_STATE_CLOSED;
  SET_STRING_ELT(names, 0, mkChar("CONN_CLOSE"));
  INTEGER(result)[1] = MIB_TCP_STATE_CLOSING;
  SET_STRING_ELT(names, 1, mkChar("CONN_CLOSING"));
  INTEGER(result)[2] = MIB_TCP_STATE_CLOSE_WAIT;
  SET_STRING_ELT(names, 2, mkChar("CONN_CLOSE_WAIT"));
  INTEGER(result)[3] = MIB_TCP_STATE_LISTEN;
  SET_STRING_ELT(names, 3, mkChar("CONN_LISTEN"));
  INTEGER(result)[4] = MIB_TCP_STATE_ESTAB;
  SET_STRING_ELT(names, 4, mkChar("CONN_ESTABLISHED"));
  INTEGER(result)[5] = MIB_TCP_STATE_SYN_SENT;
  SET_STRING_ELT(names, 5, mkChar("CONN_SYN_SENT"));
  INTEGER(result)[6] = MIB_TCP_STATE_SYN_RCVD;
  SET_STRING_ELT(names, 6, mkChar("CONN_SYN_RECV"));
  INTEGER(result)[7] = MIB_TCP_STATE_FIN_WAIT1;
  SET_STRING_ELT(names, 7, mkChar("CONN_FIN_WAIT_1"));
  INTEGER(result)[8] = MIB_TCP_STATE_FIN_WAIT2;
  SET_STRING_ELT(names, 8, mkChar("CONN_FIN_WAIT_2"));
  INTEGER(result)[9] = MIB_TCP_STATE_LAST_ACK;
  SET_STRING_ELT(names, 9, mkChar("CONN_LAST_ACK"));
  INTEGER(result)[10] = MIB_TCP_STATE_TIME_WAIT;
  SET_STRING_ELT(names, 10, mkChar("CONN_TIME_WAIT"));
  INTEGER(result)[11] = MIB_TCP_STATE_DELETE_TCB;
  SET_STRING_ELT(names, 11, mkChar("CONN_DELETE_TCB"));
  INTEGER(result)[12] = PS__CONN_NONE;
  SET_STRING_ELT(names, 12, mkChar("PS__CONN_NONE"));

  setAttrib(result, R_NamesSymbol, names);
  UNPROTECT(2);
  return result;
}

SEXP ps__define_socket_address_families() {
  SEXP afenv = PROTECT(ps_new_env());

  defineVar(install("AF_INET"), PROTECT(ScalarInteger(AF_INET)), afenv);
  UNPROTECT(1);

  defineVar(install("AF_INET6"), PROTECT(ScalarInteger(AF_INET6)), afenv);
  UNPROTECT(1);

  UNPROTECT(1);
  return afenv;
}

SEXP ps__define_socket_types() {
  SEXP env = PROTECT(ps_new_env());

  defineVar(install("SOCK_STREAM"), PROTECT(ScalarInteger(SOCK_STREAM)), env);
  UNPROTECT(1);

  defineVar(install("SOCK_DGRAM"), PROTECT(ScalarInteger(SOCK_DGRAM)), env);
  UNPROTECT(1);

  UNPROTECT(1);
  return env;
}

NTSTATUS (NTAPI *_NtQueryVirtualMemory) (
  HANDLE ProcessHandle,
  PVOID BaseAddress,
  int MemoryInformationClass,
  PVOID MemoryInformation,
  SIZE_T MemoryInformationLength,
  PSIZE_T ReturnLength
);

// A wrapper around LoadLibrary and GetProcAddress.
PVOID ps__get_proc_address_from_lib(LPCSTR libname, LPCSTR procname) {
  HMODULE mod;
  FARPROC addr;

  mod = LoadLibraryA(libname);
  if (mod  == NULL) {
    ps__set_error_from_windows_error(0);
    ps__throw_error();
  }

  if ((addr = GetProcAddress(mod, procname)) == NULL) {
    ps__set_error_from_windows_error(0);
    FreeLibrary(mod);
    ps__throw_error();
  }

  return addr;
}

SEXP ps__init(SEXP psenv, SEXP constenv) {

  /* Connection statuses */
  defineVar(install("tcp_statuses"), ps__define_tcp_statuses(), constenv);

  /* Socket address families */
  defineVar(install("address_families"),
	    ps__define_socket_address_families(), constenv);

  /* Socket types */
  defineVar(install("socket_types"), ps__define_socket_types(), constenv);

  /* errno values */
  defineVar(install("errno"), ps__define_errno(), constenv);

  /* load libs */
  NtQueryVirtualMemory = ps__get_proc_address_from_lib(
    "ntdll",
    "NtQueryVirtualMemory"
  );

  return R_NilValue;
}
