#ifndef R_PROCESSX_SUPERVISOR_WINDOWS_H
#define R_PROCESSX_SUPERVISOR_WINDOWS_H

#include <windows.h>
#include <tlhelp32.h>
#include <process.h>


// Constants ------------------------------------------------------------------

#define WIN_INPUT_BUF_LEN 1024


// Functions ------------------------------------------------------------------

int getppid(void);
HANDLE open_stdin(void);
HANDLE open_named_pipe(const char* pipe_name);
void configure_input_handle(HANDLE h_input);
char* get_line_nonblock(char* buf, int max_chars, HANDLE h_input);

void sendCtrlC(int pid);

BOOL CALLBACK enumCloseWindowProc(_In_ HWND hwnd, LPARAM lParam);
void sendWmClose(int pid);

BOOL kill_pid(DWORD dwProcessId);

#endif
