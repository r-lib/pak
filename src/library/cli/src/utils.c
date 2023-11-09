
#include <unistd.h>

#ifdef _WIN32
#include    <windows.h>
#include    <tlhelp32.h>
#endif

#include <Rinternals.h>

#include "errors.h"

#ifdef _WIN32

SEXP clic_getppid(void) {
  DWORD pid = GetCurrentProcessId();
  HANDLE handle = NULL;
  PROCESSENTRY32W pe = { 0 };

  pe.dwSize = sizeof(PROCESSENTRY32W);
  handle = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if (handle == INVALID_HANDLE_VALUE) {
    R_THROW_SYSTEM_ERROR("Cannot query parent pid");
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

  /* Should not get here */
  CloseHandle(handle);
  R_THROW_ERROR("Cannot find my own process, internal error");
  return R_NilValue;
}

#else

SEXP clic_getppid(void) {
  pid_t pid = getppid();
  return Rf_ScalarInteger(pid);
}

#endif
