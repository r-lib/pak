
#ifndef R_PROCESSX_WIN_H
#define R_PROCESSX_WIN_H

#include <windows.h>

typedef struct processx_handle_s {
  int exitcode;
  int collected;	 /* Whether exit code was collected already */
  HANDLE hProcess;
  DWORD  dwProcessId;
  BYTE *child_stdio_buffer;
  HANDLE waitObject;
  processx_connection_t *pipes[3];
  int cleanup;
  double create_time;
} processx_handle_t;

int processx__utf8_to_utf16_alloc(const char* s, WCHAR** ws_ptr);

int processx__stdio_create(processx_handle_t *handle,
			   SEXP connections, BYTE** buffer_ptr, SEXP privatex,
			   const char *encoding, const char *cname, int* inherit_std);
WORD processx__stdio_size(BYTE* buffer);
HANDLE processx__stdio_handle(BYTE* buffer, int fd);
void processx__stdio_destroy(BYTE* buffer);

int processx__create_pipe(void *id, HANDLE* parent_pipe_ptr, HANDLE* child_pipe_ptr,
                          const char *cname);
int processx__create_input_pipe(void *id, HANDLE* parent_pipe_ptr, HANDLE* child_pipe_ptr,
				const char *cname);

void processx__handle_destroy(processx_handle_t *handle);

void processx__stdio_noinherit(BYTE* buffer);
int processx__stdio_verify(BYTE* buffer, WORD size);
double processx__create_time(HANDLE process);
extern HANDLE processx__connection_iocp;

#endif
