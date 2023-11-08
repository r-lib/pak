
#if !defined(__PROCESS_INFO_H)
#define __PROCESS_INFO_H

#include <windows.h>
#include "ntextapi.h"

#include <Rinternals.h>

DWORD* ps__get_pids(DWORD *numberOfReturnedPIDs);
HANDLE ps__handle_from_pid(DWORD pid);
HANDLE ps__handle_from_pid_waccess(DWORD pid, DWORD dwDesiredAccess);
int ps__get_proc_info(DWORD pid, PSYSTEM_PROCESS_INFORMATION *retProcess,
		      PVOID *retBuffer);

int ps__assert_pid_exists(DWORD pid, char *err);
int ps__assert_pid_not_exists(DWORD pid, char *err);

SEXP ps__get_cmdline(DWORD pid);
SEXP ps__get_cwd(DWORD pid);
SEXP ps__get_environ(DWORD pid);

#endif
