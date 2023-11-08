
/* Non-throwing internal API */

SEXP ps__exe(DWORD pid);
SEXP ps__name(DWORD pid);
SEXP ps__ppid(DWORD pid);
SEXP ps__proc_name(DWORD pid);
SEXP ps__get_cmdline(DWORD pid);
SEXP ps__get_cwd(DWORD pid);
SEXP ps__get_environ(DWORD pid);
SEXP ps__proc_num_threads(DWORD pid);
SEXP ps__proc_cpu_times(DWORD pid);
SEXP ps__proc_info(DWORD pid);
SEXP ps__proc_username(DWORD pid);
SEXP ps__proc_suspend(DWORD pid);
SEXP ps__proc_resume(DWORD pid);
SEXP ps__proc_kill(DWORD pid);

double ps__filetime_to_unix(FILETIME ft);
SEXP ps__convert_dos_path(WCHAR *wstr);
void PS__CHECK_HANDLE(ps_handle_t *handle);

#define MALLOC_ZERO(x) HeapAlloc(GetProcessHeap(), HEAP_ZERO_MEMORY, (x))
#define FREE(x) HeapFree(GetProcessHeap(), 0, (x))
