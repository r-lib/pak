
#include <R.h>

#include "../processx.h"
#include "processx-stdio.h"

#include <stdio.h>
#include <io.h>

HANDLE processx__default_iocp = NULL;

static int processx__create_nul_handle(HANDLE *handle_ptr, DWORD access) {
  HANDLE handle;
  SECURITY_ATTRIBUTES sa;

  sa.nLength = sizeof(sa);
  sa.lpSecurityDescriptor = NULL;
  sa.bInheritHandle = TRUE;

  handle = CreateFileW(
    /* lpFilename =            */ L"NUL",
    /* dwDesiredAccess=        */ access,
    /* dwShareMode =           */ FILE_SHARE_READ | FILE_SHARE_WRITE,
    /* lpSecurityAttributes =  */ &sa,
    /* dwCreationDisposition = */ OPEN_EXISTING,
    /* dwFlagsAndAttributes =  */ 0,
    /* hTemplateFile =         */ NULL);
  if (handle == INVALID_HANDLE_VALUE) { return GetLastError(); }

  *handle_ptr = handle;
  return 0;
}

static int processx__create_input_handle(HANDLE *handle_ptr, const char *file,
					  DWORD access) {
  HANDLE handle;
  SECURITY_ATTRIBUTES sa;
  int  err;

  sa.nLength = sizeof(sa);
  sa.lpSecurityDescriptor = NULL;
  sa.bInheritHandle = TRUE;
  WCHAR *filew;

  err = processx__utf8_to_utf16_alloc(file, &filew);
  if (err) return(err);

  handle = CreateFileW(
    /* lpFilename =            */ filew,
    /* dwDesiredAccess=        */ access,
    /* dwShareMode =           */ FILE_SHARE_READ | FILE_SHARE_WRITE,
    /* lpSecurityAttributes =  */ &sa,
    /* dwCreationDisposition = */ OPEN_EXISTING,
    /* dwFlagsAndAttributes =  */ 0,
    /* hTemplateFile =         */ NULL);
  if (handle == INVALID_HANDLE_VALUE) { return GetLastError(); }

  *handle_ptr = handle;
  return 0;
}

static int processx__create_output_handle(HANDLE *handle_ptr, const char *file,
					  DWORD access) {
  HANDLE handle;
  SECURITY_ATTRIBUTES sa;
  int err;

  sa.nLength = sizeof(sa);
  sa.lpSecurityDescriptor = NULL;
  sa.bInheritHandle = TRUE;
  WCHAR *filew;

  err = processx__utf8_to_utf16_alloc(file, &filew);
  if (err) return(err);

  handle = CreateFileW(
    /* lpFilename =            */ filew,
    /* dwDesiredAccess=        */ access,
    /* dwShareMode =           */ FILE_SHARE_READ | FILE_SHARE_WRITE,
    /* lpSecurityAttributes =  */ &sa,
    /* dwCreationDisposition = */ CREATE_ALWAYS,
    /* dwFlagsAndAttributes =  */ 0,
    /* hTemplateFile =         */ NULL);
  if (handle == INVALID_HANDLE_VALUE) { return GetLastError(); }

  /* We will append, so set pointer to end of file */
  SetFilePointer(handle, 0, NULL, FILE_END);

  *handle_ptr = handle;
  return 0;
}

static void processx__unique_pipe_name(char* ptr, char* name, size_t size) {
  // we'll retry with a larger number if it already exists
  static int cntr = 0;
  snprintf(
    name, size, "\\\\?\\pipe\\px\\%p-%lu", ptr + cntr++,
    GetCurrentProcessId()
  );
}

int processx__create_pipe(void *id, HANDLE* parent_pipe_ptr,
                          HANDLE* child_pipe_ptr, const char *cname) {

  char pipe_name[40];
  HANDLE hOutputRead = INVALID_HANDLE_VALUE;
  HANDLE hOutputWrite = INVALID_HANDLE_VALUE;
  SECURITY_ATTRIBUTES sa;
  DWORD err;
  char *errmessage = "error for process '%s'";

  sa.nLength = sizeof(sa);
  sa.lpSecurityDescriptor = NULL;
  sa.bInheritHandle = TRUE;

  for (;;) {
    processx__unique_pipe_name(id, pipe_name, sizeof(pipe_name));

    hOutputRead = CreateNamedPipeA(
      pipe_name,
      PIPE_ACCESS_OUTBOUND | PIPE_ACCESS_INBOUND |
        FILE_FLAG_OVERLAPPED | FILE_FLAG_FIRST_PIPE_INSTANCE,
      PIPE_TYPE_BYTE | PIPE_READMODE_BYTE | PIPE_WAIT,
      1,
      65536,
      65536,
      0,
      NULL);

    if (hOutputRead != INVALID_HANDLE_VALUE) {
      break;
    }

    err = GetLastError();
    if (err != ERROR_PIPE_BUSY && err != ERROR_ACCESS_DENIED) {
      errmessage = "creating read pipe for '%s'";
      goto error;
    }
  }

  hOutputWrite = CreateFileA(
    pipe_name,
    GENERIC_WRITE,
    0,
    &sa,
    OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL,
    NULL);

  if (hOutputWrite == INVALID_HANDLE_VALUE) {
    err = GetLastError();
    errmessage = "creating write pipe for '%s'";
    goto error;
  }

  *parent_pipe_ptr = hOutputRead;
  *child_pipe_ptr  = hOutputWrite;

  return 0;

 error:
  if (hOutputRead != INVALID_HANDLE_VALUE) CloseHandle(hOutputRead);
  if (hOutputWrite != INVALID_HANDLE_VALUE) CloseHandle(hOutputWrite);
  R_THROW_SYSTEM_ERROR_CODE(err, errmessage, cname);
  return 0;			/* never reached */
}

int processx__create_input_pipe(void *id, HANDLE* parent_pipe_ptr,
                                HANDLE* child_pipe_ptr, const char *cname) {

  char pipe_name[40];
  HANDLE hOutputRead = INVALID_HANDLE_VALUE;
  HANDLE hOutputWrite = INVALID_HANDLE_VALUE;
  SECURITY_ATTRIBUTES sa;
  DWORD err;
  char *errmessage = "error for '%s'";

  sa.nLength = sizeof(sa);
  sa.lpSecurityDescriptor = NULL;
  sa.bInheritHandle = TRUE;

  for (;;) {
    processx__unique_pipe_name(id, pipe_name, sizeof(pipe_name));

    hOutputRead = CreateNamedPipeA(
      pipe_name,
      PIPE_ACCESS_OUTBOUND | PIPE_ACCESS_INBOUND |
        FILE_FLAG_FIRST_PIPE_INSTANCE,
      PIPE_TYPE_BYTE | PIPE_READMODE_BYTE | PIPE_WAIT,
      1,
      65536,
      65536,
      0,
      NULL);

    if (hOutputRead != INVALID_HANDLE_VALUE) {
      break;
    }

    err = GetLastError();
    if (err != ERROR_PIPE_BUSY && err != ERROR_ACCESS_DENIED) {
      errmessage = "creating read pipe for '%s'";
      goto error;
    }
  }

  hOutputWrite = CreateFileA(
    pipe_name,
    GENERIC_READ,
    0,
    &sa,
    OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL,
    NULL);

  if (hOutputWrite == INVALID_HANDLE_VALUE) {
    err = GetLastError();
    errmessage = "creating write pipe for '%s'";
    goto error;
  }

  *parent_pipe_ptr = hOutputRead;
  *child_pipe_ptr  = hOutputWrite;

  return 0;

 error:
  if (hOutputRead != INVALID_HANDLE_VALUE) CloseHandle(hOutputRead);
  if (hOutputWrite != INVALID_HANDLE_VALUE) CloseHandle(hOutputWrite);
  R_THROW_SYSTEM_ERROR_CODE(err, errmessage, cname);
  return 0;			/* never reached */
}

processx_connection_t * processx__create_connection(
  HANDLE pipe_handle, const char *membername, SEXP private,
  const char *encoding, BOOL async) {

  processx_connection_t *con;
  SEXP res;

  con = processx_c_connection_create(
    pipe_handle,
    async ? PROCESSX_FILE_TYPE_ASYNCPIPE : PROCESSX_FILE_TYPE_PIPE,
    encoding, NULL, &res);

  defineVar(install(membername), res, private);

  return con;
}

static int processx__duplicate_handle(HANDLE handle, HANDLE* dup) {
  HANDLE current_process;

  /* _get_osfhandle will sometimes return -2 in case of an error. This seems */
  /* to happen when fd <= 2 and the process' corresponding stdio handle is */
  /* set to NULL. Unfortunately DuplicateHandle will happily duplicate */
  /* (HANDLE) -2, so this situation goes unnoticed until someone tries to */
  /* use the duplicate. Therefore we filter out known-invalid handles here. */
  if (handle == INVALID_HANDLE_VALUE ||
      handle == NULL ||
      handle == (HANDLE) -2) {
    *dup = INVALID_HANDLE_VALUE;
    return ERROR_INVALID_HANDLE;
  }

  current_process = GetCurrentProcess();

  if (!DuplicateHandle(current_process,
                       handle,
                       current_process,
                       dup,
                       0,
                       TRUE,
                       DUPLICATE_SAME_ACCESS)) {
    *dup = INVALID_HANDLE_VALUE;
    return GetLastError();
  }

  return 0;
}

int processx__stdio_create(processx_handle_t *handle,
			   SEXP connections, BYTE** buffer_ptr, SEXP private,
			   const char *encoding, const char *cname, int* inherit_std) {
  BYTE* buffer;
  int i;
  int err;
  int count = LENGTH(connections);

  if (count > 255) {
    R_THROW_ERROR("Too many processx connections to inherit, '%s'", cname);
  }

  /* Allocate the child stdio buffer */
  buffer = malloc(CHILD_STDIO_SIZE(count));
  if (!buffer) { R_THROW_ERROR("Out of memory for process"); }

  /* Prepopulate the buffer with INVALID_HANDLE_VALUE handles, so we can
     clean up on failure*/
  CHILD_STDIO_COUNT(buffer) = count;
  for (i = 0; i < count; i++) {
    CHILD_STDIO_CRT_FLAGS(buffer, i) = 0;
    CHILD_STDIO_HANDLE(buffer, i) = INVALID_HANDLE_VALUE;
  }

  handle->pipes[0] = handle->pipes[1] = handle->pipes[2] = 0;

  for (i = 0; i < count; i++) {
    DWORD access = (i == 0) ?
      FILE_GENERIC_READ | FILE_WRITE_ATTRIBUTES :
      FILE_GENERIC_WRITE | FILE_READ_ATTRIBUTES;

    SEXP output = VECTOR_ELT(connections, i);
    const char *stroutput =
      Rf_isString(output) ? CHAR(STRING_ELT(output, 0)) : NULL;

    /* NULL means ignore */
    if (isNull(output)) {
      /* ignored output */
      err = processx__create_nul_handle(&CHILD_STDIO_HANDLE(buffer, i), access);
      if (err) { goto error; }
      CHILD_STDIO_CRT_FLAGS(buffer, i) = FOPEN | FDEV;

    } else if (i == 2 && stroutput && ! strcmp("2>&1", stroutput)) {
      /* This is stderr, sent to stdout */
      /* We need to turn off buffering, otherwise the output on
       the two handles won't be correctly interleaved.
       We set FDEV on the pipes/files. This tricks windows
       into turning off the CRT buffering */
      CHILD_STDIO_COPY(buffer, 2, 1);
      CHILD_STDIO_CRT_FLAGS(buffer, 1) = FOPEN | FDEV;
      CHILD_STDIO_CRT_FLAGS(buffer, 2) = FOPEN | FDEV;

    } else if (stroutput && ! strcmp("|", stroutput)) {
      /* piped output */
      processx_connection_t *con = 0;
      HANDLE parent_handle;
      const char *r_pipe_name = i == 0 ? "stdin_pipe" :
        (i == 1 ? "stdout_pipe" : "stderr_pipe");
      if (i == 0) {
        err = processx__create_input_pipe(handle, &parent_handle,
                                          &CHILD_STDIO_HANDLE(buffer, i), cname);
      } else {
        err = processx__create_pipe(handle, &parent_handle,
                                    &CHILD_STDIO_HANDLE(buffer, i), cname);
      }
      if (err) goto error;
      CHILD_STDIO_CRT_FLAGS(buffer, i) = FOPEN | FPIPE;
      con = processx__create_connection(parent_handle, r_pipe_name,
                                        private, encoding, i != 0);
      handle->pipes[i] = con;

    } else if (stroutput && strcmp("", stroutput)) {
      /* output to file */
      if (i == 0) {
        err = processx__create_input_handle(&CHILD_STDIO_HANDLE(buffer, i),
                                            stroutput, access);
      } else {
        err = processx__create_output_handle(&CHILD_STDIO_HANDLE(buffer, i),
                                             stroutput, access);
      }
      if (err) { goto error; }
      CHILD_STDIO_CRT_FLAGS(buffer, i) = FOPEN | FDEV;

    } else {
      /* inherit connection */
      HANDLE child_handle;
      HANDLE ihnd;

      /* std connection or extra connection */
      if (stroutput) {
        *inherit_std = 1;
        DWORD nh = i == 0 ? STD_INPUT_HANDLE :
          (i == 1 ? STD_OUTPUT_HANDLE : STD_ERROR_HANDLE );
        ihnd = GetStdHandle(nh);
        if (ihnd == INVALID_HANDLE_VALUE ||
            ihnd == NULL ||
            ihnd == (HANDLE) -2) {
          FILE *sh = i == 0 ? stdin : (i == 1 ? stdout : stderr);
          int fd = _fileno(sh);
          REprintf("Opening fd %i\n", fd);
          ihnd = (HANDLE) _get_osfhandle(fd);
        }

      } else {
        processx_connection_t *ccon = R_ExternalPtrAddr(output);
        if (!ccon) R_THROW_ERROR("Invalid (closed) connection");
        ihnd = (HANDLE*) processx_c_connection_fileno(ccon);
      }

      err = processx__duplicate_handle(ihnd, &child_handle);
      if (err) goto error;

      switch (GetFileType(child_handle)) {
      case FILE_TYPE_DISK:
        CHILD_STDIO_CRT_FLAGS(buffer, i) = FOPEN;
        break;
      case FILE_TYPE_PIPE:
        CHILD_STDIO_CRT_FLAGS(buffer, i) = FOPEN | FDEV;
        break;
      case FILE_TYPE_CHAR:
      case FILE_TYPE_REMOTE:
      case FILE_TYPE_UNKNOWN:
        CHILD_STDIO_CRT_FLAGS(buffer, i) = FOPEN | FDEV;
        break;
      default:
        err = -1;
        goto error;
      }

      CHILD_STDIO_HANDLE(buffer, i) = child_handle;
    }
  }

  *buffer_ptr = buffer;
  return 0;

 error:
  processx__stdio_destroy(buffer);
  for (i = 0; i < 3; i++) {
    if (handle->pipes[i]) {
      processx_c_connection_destroy(handle->pipes[i]);
    }
  }
  return err;
}

void processx__stdio_destroy(BYTE* buffer) {
  int i, count;

  count = CHILD_STDIO_COUNT(buffer);
  for (i = 0; i < count; i++) {
    HANDLE handle = CHILD_STDIO_HANDLE(buffer, i);
    if (handle != INVALID_HANDLE_VALUE) {
      CloseHandle(handle);
    }
  }

  free(buffer);
}

void processx__stdio_noinherit(BYTE* buffer) {
  int i, count;

  count = CHILD_STDIO_COUNT(buffer);
  for (i = 0; i < count; i++) {
    HANDLE handle = CHILD_STDIO_HANDLE(buffer, i);
    if (handle != INVALID_HANDLE_VALUE) {
      SetHandleInformation(handle, HANDLE_FLAG_INHERIT, 0);
    }
  }
}

int processx__stdio_verify(BYTE* buffer, WORD size) {
  unsigned int count;

  /* Check the buffer pointer. */
  if (buffer == NULL)
    return 0;

  /* Verify that the buffer is at least big enough to hold the count. */
  if (size < CHILD_STDIO_SIZE(0))
    return 0;

  /* Verify if the count is within range. */
  count = CHILD_STDIO_COUNT(buffer);
  if (count > 256)
    return 0;

  /* Verify that the buffer size is big enough to hold info for N FDs. */
  if (size < CHILD_STDIO_SIZE(count))
    return 0;

  return 1;
}

WORD processx__stdio_size(BYTE* buffer) {
  return (WORD) CHILD_STDIO_SIZE(CHILD_STDIO_COUNT((buffer)));
}

HANDLE processx__stdio_handle(BYTE* buffer, int fd) {
  return CHILD_STDIO_HANDLE(buffer, fd);
}
