
#include "process_handles.h"
#include "../../common.h"
#include "../../windows.h"

static _NtQuerySystemInformation __NtQuerySystemInformation = NULL;
static _NtQueryObject __NtQueryObject = NULL;

BOOL g_initialized = FALSE;
NTSTATUS g_status;
HANDLE g_hFile = NULL;
HANDLE g_hEvtStart = NULL;
HANDLE g_hEvtFinish = NULL;
HANDLE g_hThread = NULL;
PUNICODE_STRING g_pNameBuffer = NULL;
ULONG g_dwSize = 0;
ULONG g_dwLength = 0;

PVOID GetLibraryProcAddress(PSTR LibraryName, PSTR ProcName) {
  return GetProcAddress(GetModuleHandleA(LibraryName), ProcName);
}

VOID ps__get_open_files_init(void) {
  if (g_initialized == TRUE)
    return;

  // Resolve the Windows API calls
  __NtQuerySystemInformation =
    GetLibraryProcAddress("ntdll.dll", "NtQuerySystemInformation");
  __NtQueryObject = GetLibraryProcAddress("ntdll.dll", "NtQueryObject");

  g_hEvtStart = CreateEvent(NULL, FALSE, FALSE, NULL);
  g_hEvtFinish = CreateEvent(NULL, FALSE, FALSE, NULL);

  g_initialized = TRUE;
}


SEXP ps__get_open_files(long dwPid, HANDLE hProcess) {
  NTSTATUS                            status;
  PSYSTEM_HANDLE_INFORMATION_EX       pHandleInfo = NULL;
  DWORD                               dwInfoSize = 0x10000;
  DWORD                               dwRet = 0;
  PSYSTEM_HANDLE_TABLE_ENTRY_INFO_EX  hHandle = NULL;
  DWORD                               i = 0;
  BOOLEAN                             error = FALSE;
  DWORD                               dwWait = 0;
  SEXP                                retlist = NULL;
  SEXP                                path = NULL;
  DWORD                               len = 20;
  DWORD                               num = 0;
  PROTECT_INDEX                       pidx;

  PROTECT_WITH_INDEX(retlist = allocVector(VECSXP, len), &pidx);

  if (g_initialized == FALSE) ps__get_open_files_init();

  if (__NtQuerySystemInformation == NULL ||
      __NtQueryObject == NULL ||
      g_hEvtStart == NULL ||
      g_hEvtFinish == NULL) {
    ps__set_error_from_windows_error(0);
    error = TRUE;
    goto cleanup;
  }

  do {
    if (pHandleInfo != NULL) {
      HeapFree(GetProcessHeap(), 0, pHandleInfo);
      pHandleInfo = NULL;
    }

    // NtQuerySystemInformation won't give us the correct buffer size,
    // so we guess by doubling the buffer size.
    dwInfoSize *= 2;
    pHandleInfo = HeapAlloc(GetProcessHeap(),
			    HEAP_ZERO_MEMORY,
			    dwInfoSize);

    if (pHandleInfo == NULL) {
      ps__no_memory("");
      error = TRUE;
      goto cleanup;
    }
  } while ((status = __NtQuerySystemInformation(
    SystemExtendedHandleInformation,
    pHandleInfo,
    dwInfoSize,
    &dwRet)) == STATUS_INFO_LENGTH_MISMATCH);

  // NtQuerySystemInformation stopped giving us STATUS_INFO_LENGTH_MISMATCH
  if (!NT_SUCCESS(status)) {
    ps__set_error_from_windows_error(HRESULT_FROM_NT(status));
    error = TRUE;
    goto cleanup;
  }

  for (i = 0; i < pHandleInfo->NumberOfHandles; i++) {
    hHandle = &pHandleInfo->Handles[i];

    // Check if this hHandle belongs to the PID the user specified.
#ifdef _WIN64
    if ((DWORD64)(hHandle->UniqueProcessId) != (DWORD64)dwPid) {
#else
    if (hHandle->UniqueProcessId != (HANDLE)dwPid) {
#endif
      goto loop_cleanup;
    }

    if (!DuplicateHandle(hProcess,
			 hHandle->HandleValue,
			 GetCurrentProcess(),
			 &g_hFile,
			 0,
			 TRUE,
			 DUPLICATE_SAME_ACCESS)) {
      goto loop_cleanup;
    }

    // Guess buffer size is MAX_PATH + 1
    g_dwLength = (MAX_PATH+1) * sizeof(WCHAR);

    do {
      // Release any previously allocated buffer
      if (g_pNameBuffer != NULL) {
	      HeapFree(GetProcessHeap(), 0, g_pNameBuffer);
	      g_pNameBuffer = NULL;
	      g_dwSize = 0;
      }

      // NtQueryObject puts the required buffer size in g_dwLength
      // WinXP edge case puts g_dwLength == 0, just skip this handle
      if (g_dwLength == 0) goto loop_cleanup;

      g_dwSize = g_dwLength;
      if (g_dwSize > 0) {
	      g_pNameBuffer = HeapAlloc(GetProcessHeap(),
			  HEAP_ZERO_MEMORY,
			  g_dwSize);
	      if (g_pNameBuffer == NULL) goto loop_cleanup;
      }

      dwWait = ps__NtQueryObject();

      // If the call does not return, skip this handle
      if (dwWait != WAIT_OBJECT_0) goto loop_cleanup;

    } while (g_status == STATUS_INFO_LENGTH_MISMATCH);

    // NtQueryObject stopped returning STATUS_INFO_LENGTH_MISMATCH
    if (!NT_SUCCESS(g_status)) goto loop_cleanup;

    // Convert to UTF-8 and append it to the return list
    if (g_pNameBuffer->Length > 0) {
      PROTECT(path = ps__convert_dos_path(g_pNameBuffer->Buffer));
      if (!isNull(path)) {
	      if (++num == len) {
	        len *= 2;
	        REPROTECT(retlist = Rf_lengthgets(retlist, len), pidx);
	      }
	      SET_VECTOR_ELT(retlist, num, ps__build_list("Oi", path, NA_INTEGER));
      }
      UNPROTECT(1);
    }

  loop_cleanup:
    if (g_pNameBuffer != NULL) HeapFree(GetProcessHeap(), 0, g_pNameBuffer);
    g_pNameBuffer = NULL;
    g_dwSize = 0;
    g_dwLength = 0;

    if (g_hFile != NULL) CloseHandle(g_hFile);
    g_hFile = NULL;
  }

 cleanup:
  if (g_pNameBuffer != NULL) HeapFree(GetProcessHeap(), 0, g_pNameBuffer);
  g_pNameBuffer = NULL;
  g_dwSize = 0;
  g_dwLength = 0;

  if (g_hFile != NULL) CloseHandle(g_hFile);
  g_hFile = NULL;

  if (pHandleInfo != NULL) HeapFree(GetProcessHeap(), 0, pHandleInfo);
  pHandleInfo = NULL;

  UNPROTECT(1);
  if (error) return R_NilValue;
  return retlist;
}


DWORD ps__NtQueryObject() {
  DWORD dwWait = 0;

  if (g_hThread == NULL)
    g_hThread = CreateThread(
			     NULL,
			     0,
			     ps__NtQueryObjectThread,
			     NULL,
			     0,
			     NULL);
  if (g_hThread == NULL)
    return GetLastError();

  // Signal the worker thread to start
  SetEvent(g_hEvtStart);

  // Wait for the worker thread to finish
  dwWait = WaitForSingleObject(g_hEvtFinish, NTQO_TIMEOUT);

  // If the thread hangs, kill it and cleanup
  if (dwWait == WAIT_TIMEOUT) {
    SuspendThread(g_hThread);
    TerminateThread(g_hThread, 1);
    WaitForSingleObject(g_hThread, INFINITE);
    CloseHandle(g_hThread);

    g_hThread = NULL;
  }

  return dwWait;
}


DWORD WINAPI
ps__NtQueryObjectThread(LPVOID lpvParam) {
  // Loop infinitely waiting for work
  while (TRUE) {
    WaitForSingleObject(g_hEvtStart, INFINITE);

    g_status = __NtQueryObject(g_hFile,
			       ObjectNameInformation,
			       g_pNameBuffer,
			       g_dwSize,
			       &g_dwLength);
    SetEvent(g_hEvtFinish);
  }
}

SEXP ps__get_modules(HANDLE hProcess) {
  unsigned int nalloc = 1024;
  HMODULE *hMods = (HMODULE*) R_alloc(nalloc, sizeof(HMODULE));
  DWORD cbNeeded;
  unsigned int i;
  SEXP result = R_NilValue;

  while (1) {
    BOOL ret = EnumProcessModules(
      hProcess,
      hMods,
      sizeof(HMODULE) * nalloc,
      &cbNeeded
    );
    unsigned int got = cbNeeded / sizeof(HMODULE);
    if (ret && got <= nalloc) {
      result = PROTECT(Rf_allocVector(VECSXP, got));
      for (i = 0; i < got; i++) {
        wchar_t szModName[MAX_PATH];
        ret = GetModuleFileNameExW(hProcess, hMods[i], szModName,
                                   MAX_PATH);
        if (!ret) {
          UNPROTECT(1);
          return R_NilValue;
        }
        SEXP utf8name = PROTECT(ps__utf16_to_charsxp(szModName, -1));
        SET_VECTOR_ELT(result, i, ScalarString(utf8name));
        UNPROTECT(1);
      }
      break;

    } else if (got > nalloc) {
      hMods = (HMODULE*) S_realloc((char*) hMods, got + 100, nalloc,
                                   sizeof(HMODULE));
      nalloc = got + 100;
    } else {
      UNPROTECT(1);
      return R_NilValue;
    }
  }

  UNPROTECT(1);
  return result;
}

