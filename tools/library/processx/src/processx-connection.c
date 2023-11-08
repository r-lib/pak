
#include "processx-connection.h"

#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>

#ifndef _WIN32
#include <sys/uio.h>
#include <poll.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <sys/un.h>
#else
#include <io.h>
#endif

#include "processx.h"

#ifdef _WIN32
#include "win/processx-win.h"
#else
#include "unix/processx-unix.h"
#endif

/* Internal functions in this file */

static void processx__connection_find_chars(processx_connection_t *ccon,
					    ssize_t maxchars,
					    ssize_t maxbytes,
					    size_t *chars,
					    size_t *bytes);

static void processx__connection_find_lines(processx_connection_t *ccon,
					    ssize_t maxlines,
					    size_t *lines,
					    int *eof);

static void processx__connection_alloc(processx_connection_t *ccon);
static void processx__connection_realloc(processx_connection_t *ccon);
static ssize_t processx__connection_read(processx_connection_t *ccon);
static ssize_t processx__find_newline(processx_connection_t *ccon,
				      size_t start);
static ssize_t processx__connection_read_until_newline(processx_connection_t
						       *ccon);
static void processx__connection_xfinalizer(SEXP con);
static ssize_t processx__connection_to_utf8(processx_connection_t *ccon);
static void processx__connection_find_utf8_chars(processx_connection_t *ccon,
						 ssize_t maxchars,
						 ssize_t maxbytes,
						 size_t *chars,
						 size_t *bytes);

#ifdef _WIN32
#define PROCESSX_CHECK_VALID_CONN(x) do {				\
    if (!x) R_THROW_ERROR("Invalid connection object");                 \
    if (!(x)->handle.handle) {						\
      R_THROW_ERROR("Invalid (uninitialized or closed?) connection object"); \
    }									\
  } while (0)
#else
#define PROCESSX_CHECK_VALID_CONN(x) do {				\
    if (!x) R_THROW_ERROR("Invalid connection object");				\
    if ((x)->handle < 0) {                                              \
      R_THROW_ERROR("Invalid (uninitialized or closed?) connection object");	\
}                                                                       \
  } while (0)
#endif

/* --------------------------------------------------------------------- */
/* API from R                                                            */
/* --------------------------------------------------------------------- */

SEXP processx_connection_create(SEXP handle, SEXP encoding) {
  processx_file_handle_t *os_handle = R_ExternalPtrAddr(handle);
  const char *c_encoding = CHAR(STRING_ELT(encoding, 0));
  SEXP result = R_NilValue;

  if (!os_handle) R_THROW_ERROR("Cannot create connection, invalid handle");

  processx_c_connection_create(*os_handle, PROCESSX_FILE_TYPE_ASYNCPIPE,
			       c_encoding, NULL, &result);
  return result;
}

SEXP processx_connection_create_fd(SEXP handle, SEXP encoding, SEXP close) {
  int fd = INTEGER(handle)[0];
  const char *c_encoding = CHAR(STRING_ELT(encoding, 0));
  processx_file_handle_t os_handle;
  processx_connection_t *con;
  SEXP result = R_NilValue;

#ifdef _WIN32
  os_handle = (HANDLE) _get_osfhandle(fd);
#else
  os_handle = fd;
#endif

  con = processx_c_connection_create(os_handle, PROCESSX_FILE_TYPE_ASYNCPIPE,
				     c_encoding, NULL, &result);

  if (! LOGICAL(close)[0]) con->close_on_destroy = 0;

  return result;
}

SEXP processx_connection_create_file(SEXP filename, SEXP read, SEXP write) {
  const char *c_filename = CHAR(STRING_ELT(filename, 0));
  int c_read = LOGICAL(read)[0];
  int c_write = LOGICAL(write)[0];
  SEXP result = R_NilValue;
  processx_file_handle_t os_handle;

#ifdef _WIN32
  DWORD access = 0, create = 0;
  if (c_read) access |= GENERIC_READ;
  if (c_write) access |= GENERIC_WRITE;
  if (c_read) create |= OPEN_EXISTING;
  if (c_write) create |= CREATE_ALWAYS;
  os_handle = CreateFile(
    /* lpFilename = */ c_filename,
    /* dwDesiredAccess = */ access,
    /* dwShareMode = */ 0,
    /* lpSecurityAttributes = */ NULL,
    /* dwCreationDisposition = */ create,
    /* dwFlagsAndAttributes = */ FILE_ATTRIBUTE_NORMAL,
    /* hTemplateFile = */ NULL);
  if (os_handle == INVALID_HANDLE_VALUE) {
    R_THROW_SYSTEM_ERROR("Cannot open file `%s`", c_filename);
  }

#else
  int flags = 0;
  if ( c_read && !c_write) flags |= O_RDONLY;
  if (!c_read &&  c_write) flags |= O_WRONLY | O_CREAT | O_TRUNC;
  if ( c_read &&  c_write) flags |= O_RDWR;
  os_handle = open(c_filename, flags, 0644);
  if (os_handle == -1) {
    R_THROW_SYSTEM_ERROR("Cannot open file `%s`", c_filename);
  }
#endif

  processx_c_connection_create(os_handle, PROCESSX_FILE_TYPE_FILE,
			       "", c_filename, &result);

  return result;
}

SEXP processx_connection_read_chars(SEXP con, SEXP nchars) {

  processx_connection_t *ccon = R_ExternalPtrAddr(con);
  SEXP result;
  int cnchars = asInteger(nchars);
  size_t utf8_chars, utf8_bytes;

  processx__connection_find_chars(ccon, cnchars, -1, &utf8_chars,
				  &utf8_bytes);

  result = PROTECT(ScalarString(mkCharLenCE(ccon->utf8, (int) utf8_bytes,
					    CE_UTF8)));
  ccon->utf8_data_size -= utf8_bytes;
  memmove(ccon->utf8, ccon->utf8 + utf8_bytes, ccon->utf8_data_size);

  UNPROTECT(1);
  return result;
}

SEXP processx_connection_read_lines(SEXP con, SEXP nlines) {

  processx_connection_t *ccon = R_ExternalPtrAddr(con);
  SEXP result;
  int cn = asInteger(nlines);
  ssize_t newline, eol = -1;
  size_t lines_read = 0, l;
  int eof = 0;
  int slashr;

  processx__connection_find_lines(ccon, cn, &lines_read, &eof);

  result = PROTECT(allocVector(STRSXP, lines_read + eof));
  for (l = 0, newline = -1; l < lines_read; l++) {
    eol = processx__find_newline(ccon, newline + 1);
    slashr = eol > 0 && ccon->utf8[eol - 1] == '\r';
    SET_STRING_ELT(
      result, l,
      mkCharLenCE(ccon->utf8 + newline + 1,
		  (int) (eol - newline - 1 - slashr), CE_UTF8));
    newline = eol;
  }

  if (eof) {
    eol = ccon->utf8_data_size - 1;
    SET_STRING_ELT(
      result, l,
      mkCharLenCE(ccon->utf8 + newline + 1,
		  (int) (eol - newline), CE_UTF8));
  }

  if (eol >= 0) {
    ccon->utf8_data_size -= eol + 1;
    memmove(ccon->utf8, ccon->utf8 + eol + 1, ccon->utf8_data_size);
  }

  UNPROTECT(1);
  return result;
}

SEXP processx_connection_write_bytes(SEXP con, SEXP bytes) {
  processx_connection_t *ccon = R_ExternalPtrAddr(con);
  Rbyte *cbytes = RAW(bytes);
  size_t nbytes = LENGTH(bytes);
  SEXP result;

  ssize_t written = processx_c_connection_write_bytes(ccon, cbytes, nbytes);

  size_t left = nbytes - written;
  PROTECT(result = allocVector(RAWSXP, left));
  if (left > 0) memcpy(RAW(result), cbytes + written, left);

  UNPROTECT(1);
  return result;
}

SEXP processx_connection_is_eof(SEXP con) {
  processx_connection_t *ccon = R_ExternalPtrAddr(con);
  if (!ccon) R_THROW_ERROR("Invalid connection object");
  return ScalarLogical(ccon->is_eof_);
}

SEXP processx_connection_file_name(SEXP con) {
  processx_connection_t *ccon = R_ExternalPtrAddr(con);
  if (!ccon) R_THROW_ERROR("Invalid connection object");
  if (ccon->filename) {
    SEXP fn = PROTECT(allocVector(STRSXP, 1));
    SET_STRING_ELT(fn, 0, Rf_mkCharCE(ccon->filename, CE_UTF8));
    UNPROTECT(1);
    return fn;
  } else {
    return(NA_STRING);
  }
}

SEXP processx_connection_close(SEXP con) {
  processx_connection_t *ccon = R_ExternalPtrAddr(con);
  if (!ccon) R_THROW_ERROR("Invalid connection object");
  processx_c_connection_close(ccon);
  return R_NilValue;
}

SEXP processx_connection_is_closed(SEXP con) {
  processx_connection_t *ccon = R_ExternalPtrAddr(con);
  if (!ccon) R_THROW_ERROR("Invalid connection object");
  return ScalarLogical(processx_c_connection_is_closed(ccon));
}

/* Poll connections and other pollable handles */
SEXP processx_connection_poll(SEXP pollables, SEXP timeout) {
  /* TODO: this is not used currently */
  R_THROW_ERROR("Not implemented");
  return R_NilValue;
}

SEXP processx_connection_create_fifo(SEXP read, SEXP write,
                                     SEXP filename, SEXP encoding,
                                     SEXP nonblocking) {
  const char *c_encoding = CHAR(STRING_ELT(encoding, 0));
  const char *c_filename = CHAR(STRING_ELT(filename, 0));
  int c_read = LOGICAL(read)[0];
  int c_write = LOGICAL(write)[0];
  int c_nonblocking = LOGICAL(nonblocking)[0];
  SEXP result;
  processx_file_handle_t os_handle;

#ifdef _WIN32
  SECURITY_ATTRIBUTES sa;
  DWORD openmode = FILE_FLAG_FIRST_PIPE_INSTANCE;
  if (c_read) openmode |= PIPE_ACCESS_INBOUND;
  if (c_write) openmode |= PIPE_ACCESS_OUTBOUND;
  if (c_nonblocking) openmode |= FILE_FLAG_OVERLAPPED;

  sa.nLength = sizeof(sa);
  sa.lpSecurityDescriptor = NULL;
  sa.bInheritHandle = TRUE;

  os_handle = CreateNamedPipeA(
    c_filename,
    openmode,
    PIPE_TYPE_BYTE | PIPE_READMODE_BYTE | PIPE_WAIT,
    1,
    65536,
    65536,
    0,
    NULL);

  if (os_handle == INVALID_HANDLE_VALUE) {
    R_THROW_SYSTEM_ERROR("could not create pipe");
  }

#else
  int ret = mkfifo(c_filename, 0600);
  if (ret < 0) {
    R_THROW_SYSTEM_ERROR("Cannot create fifo at %s", c_filename);
  }
  int flags = 0;
  if ( c_read && !c_write) flags |= O_RDONLY;
  // This is undefined behavior according to the standard, but in practice
  // it works on Linux and macOS, and probably all Unix systems. It lets us
  // open the write end of the fifo without blocking.
  if (!c_read &&  c_write) flags |= c_nonblocking ? O_RDWR : O_WRONLY;
  if (c_nonblocking) flags |= O_NONBLOCK;
  os_handle = open(c_filename, flags);
  if (os_handle == -1) {
    R_THROW_SYSTEM_ERROR("Cannot open fifo `%s`", c_filename);
  }
  processx__nonblock_fcntl(os_handle, c_nonblocking);
#endif

  processx_c_connection_create(
    os_handle,
    c_nonblocking ? PROCESSX_FILE_TYPE_ASYNCPIPE : PROCESSX_FILE_TYPE_PIPE,
    c_encoding,
    c_filename,
    &result
  );

  return result;
}

SEXP processx_connection_connect_fifo(SEXP filename, SEXP read, SEXP write,
                                      SEXP encoding, SEXP nonblocking) {
  const char *c_filename = CHAR(STRING_ELT(filename, 0));
  int c_read = LOGICAL(read)[0];
  int c_write = LOGICAL(write)[0];
  const char *c_encoding = CHAR(STRING_ELT(encoding, 0));
  int c_nonblocking = LOGICAL(nonblocking)[0];
  SEXP result;
  processx_file_handle_t os_handle;

#ifdef _WIN32
  SECURITY_ATTRIBUTES sa;
  DWORD access = 0;
  DWORD attr = 0;

  sa.nLength = sizeof(sa);
  sa.lpSecurityDescriptor = NULL;
  sa.bInheritHandle = TRUE;

  if (c_read) access |= GENERIC_READ;
  if (c_write) access |= GENERIC_WRITE;

  if (c_nonblocking) {
    attr |= FILE_FLAG_OVERLAPPED;
  } else {
    attr |= FILE_ATTRIBUTE_NORMAL;
  }

  os_handle = CreateFileA(
    c_filename,
    access,
    0,
    &sa,
    OPEN_EXISTING,
    attr,
    NULL
  );

#else
  int flags = 0;
  if ( c_read && !c_write) flags |= O_RDONLY;
  if (!c_read &&  c_write) flags |= c_nonblocking ? O_RDWR : O_WRONLY;
  if (c_nonblocking) flags |= O_NONBLOCK;
  os_handle = open(c_filename, flags);
  if (os_handle == -1) {
    R_THROW_SYSTEM_ERROR("Cannot open fifo `%s`", c_filename);
  }
  processx__nonblock_fcntl(os_handle, c_nonblocking);
#endif

  processx_c_connection_create(
    os_handle,
    c_nonblocking ? PROCESSX_FILE_TYPE_ASYNCPIPE : PROCESSX_FILE_TYPE_PIPE,
    c_encoding,
    c_filename,
    &result
  );

  return result;
}

SEXP processx_connection_create_socket(SEXP filename, SEXP encoding) {
  const char *c_encoding = CHAR(STRING_ELT(encoding, 0));
  const char *c_filename = CHAR(STRING_ELT(filename, 0));
  SEXP result;
  processx_file_handle_t os_handle;

#ifdef _WIN32
  SECURITY_ATTRIBUTES sa;
  DWORD openmode = FILE_FLAG_FIRST_PIPE_INSTANCE |
    PIPE_ACCESS_INBOUND | PIPE_ACCESS_OUTBOUND |
    FILE_FLAG_OVERLAPPED;

  sa.nLength = sizeof(sa);
  sa.lpSecurityDescriptor = NULL;
  sa.bInheritHandle = TRUE;

  os_handle = CreateNamedPipeA(
    c_filename,
    openmode,
    PIPE_TYPE_BYTE | PIPE_READMODE_BYTE | PIPE_WAIT,
    1,
    65536,
    65536,
    0,
    NULL);

  if (os_handle == INVALID_HANDLE_VALUE) {
    R_THROW_SYSTEM_ERROR("could not create pipe");
  }

#else
  struct sockaddr_un addr;
  if (strlen(c_filename) > sizeof(addr.sun_path) - 1) {
    R_THROW_ERROR("Server socket path too long: %s", c_filename);
  }
  os_handle = socket(AF_UNIX, SOCK_STREAM, 0);
  if (os_handle == -1) {
    R_THROW_SYSTEM_ERROR("Cannot create socket");        // __NO_COVERAGE__
  }                                                      // __NO_COVERAGE__
  memset(&addr, 0, sizeof(struct sockaddr_un));
  addr.sun_family = AF_UNIX;
  strncpy(addr.sun_path, c_filename, sizeof(addr.sun_path) - 1);

  int ret = bind(
    os_handle,
    (struct sockaddr *) &addr,
    sizeof(struct sockaddr_un)
  );
  if (ret == -1) {
    R_THROW_SYSTEM_ERROR("Cannot bind to socket");
  }
  ret = listen(os_handle, 1);
  if (ret == -1) {
    R_THROW_SYSTEM_ERROR("Cannot listen on socket");     // __NO_COVERAGE__
  }                                                      // __NO_COVERAGE__
  processx__nonblock_fcntl(os_handle, 1);

#endif

  processx_c_connection_create(
    os_handle,
    PROCESSX_FILE_TYPE_SOCKET,
    c_encoding,
    c_filename,
    &result
  );

  processx_connection_t *ccon = R_ExternalPtrAddr(result);
  ccon->state = PROCESSX_SOCKET_LISTEN;

  return result;

}

SEXP processx_connection_connect_socket(SEXP filename, SEXP encoding) {
  const char *c_filename = CHAR(STRING_ELT(filename, 0));
  const char *c_encoding = CHAR(STRING_ELT(encoding, 0));
  SEXP result;
  processx_file_handle_t os_handle;

#ifdef _WIN32
  SECURITY_ATTRIBUTES sa;
  DWORD access = GENERIC_READ | GENERIC_WRITE;
  DWORD attr = FILE_FLAG_OVERLAPPED;

  sa.nLength = sizeof(sa);
  sa.lpSecurityDescriptor = NULL;
  sa.bInheritHandle = TRUE;

  os_handle = CreateFileA(
    c_filename,
    access,
    0,
    &sa,
    OPEN_EXISTING,
    attr,
    NULL
  );

  if (os_handle == INVALID_HANDLE_VALUE) {
    R_THROW_SYSTEM_ERROR("Cannot connect to Unix(-like) socket");
  }

#else
  struct sockaddr_un addr;
  os_handle = socket(AF_UNIX, SOCK_STREAM, 0);
  if (os_handle == -1) {
    R_THROW_SYSTEM_ERROR("Cannot create socket");        // __NO_COVERAGE__
  }                                                      // __NO_COVERAGE__
  processx__nonblock_fcntl(os_handle, 1);
  memset(&addr, 0, sizeof(struct sockaddr_un));
  addr.sun_family = AF_UNIX;
  strncpy(addr.sun_path, c_filename, sizeof(addr.sun_path) - 1);

  int ret = connect(
    os_handle,
    (struct sockaddr *) &addr,
    sizeof(struct sockaddr_un)
  );
  if (ret == -1) {
    R_THROW_SYSTEM_ERROR("Cannot connect to socket");
  }

#endif

  processx_c_connection_create(
    os_handle,
    PROCESSX_FILE_TYPE_SOCKET,
    c_encoding,
    c_filename,
    &result
  );

  processx_connection_t *ccon = R_ExternalPtrAddr(result);
  ccon->state = PROCESSX_SOCKET_CONNECTED_CLIENT;

  return result;
}

SEXP processx_connection_accept_socket(SEXP con) {
  processx_connection_t *ccon = R_ExternalPtrAddr(con);
  if (!ccon) R_THROW_ERROR("Invalid connection object");
  if (ccon->type != PROCESSX_FILE_TYPE_SOCKET) {
    R_THROW_ERROR("Not a socket connection");
  }
  if (ccon->state != PROCESSX_SOCKET_LISTEN &&
      ccon->state != PROCESSX_SOCKET_LISTEN_PIPE_READY) {
    R_THROW_ERROR("Socket is not listening");
  }

#ifdef _WIN32
  // We can probably use GetNamedPipeHandleStateA to get the current
  // state of the pipe, and if it is connected, then OK, otherwise we
  // return an error. This simulates the Unix behavior.
  DWORD instances = -1;
  BOOL ret = GetNamedPipeHandleStateA(
    /* hNamedPipe=           */ ccon->handle.handle,
    /* lpState=              */ NULL,
    /* lpInstances=          */ &instances,
    /* lpMaxCollectionCount= */ NULL,
    /* lpCollectDataTimeout= */ NULL,
    /* lpUserName=           */ NULL,
    /* nMaxUserNameSize=     */ 0
  );
  if (!ret) {
    R_THROW_SYSTEM_ERROR("Cannot query state of Unix socket (server named pipe)");
  }
  if (instances == 1) {
    ccon->state = PROCESSX_SOCKET_CONNECTED_SERVER;
  }

#else
  int newfd = accept(ccon->handle, NULL, NULL);
  if (newfd == -1) {
    R_THROW_SYSTEM_ERROR("Could not accept socket connection"); // __NO_COVERAGE__
  }                                                             // __NO_COVERAGE__
  /* Need to set the new fd to non-blocking as well, otherwise it */
  /* is blocking on some OSes, e.g. Linux */
  processx__nonblock_fcntl(newfd, 1);

  close(ccon->handle);
  ccon->handle = newfd;
  ccon->state = PROCESSX_SOCKET_CONNECTED_SERVER;
#endif

  return R_NilValue;
}

SEXP processx_connection_socket_state(SEXP con) {
  processx_connection_t *ccon = R_ExternalPtrAddr(con);
  if (!ccon) R_THROW_ERROR("Invalid connection object");
  if (ccon->type != PROCESSX_FILE_TYPE_SOCKET) {
    R_THROW_ERROR("Not a socket connection");
  }

  return ScalarInteger(ccon->state);
}

SEXP processx_connection_create_pipepair(SEXP encoding, SEXP nonblocking) {
  const char *c_encoding = CHAR(STRING_ELT(encoding, 0));
  int *c_nonblocking = LOGICAL(nonblocking);
  SEXP result, con1, con2;

#ifdef _WIN32
  HANDLE h1, h2;
  processx__create_pipe(0, &h1, &h2, "???");

#else
  int pipe[2], h1, h2;
  processx__make_socketpair(pipe, NULL);
  processx__nonblock_fcntl(pipe[0], c_nonblocking[0]);
  processx__nonblock_fcntl(pipe[1], c_nonblocking[1]);
  h1 = pipe[0];
  h2 = pipe[1];
#endif

  processx_c_connection_create(h1, c_nonblocking[0] ?
    PROCESSX_FILE_TYPE_ASYNCPIPE : PROCESSX_FILE_TYPE_PIPE, c_encoding,
    NULL, &con1);
  PROTECT(con1);
  processx_c_connection_create(h2, c_nonblocking[1] ?
    PROCESSX_FILE_TYPE_ASYNCPIPE : PROCESSX_FILE_TYPE_PIPE, c_encoding,
    NULL, &con2);
  PROTECT(con2);

  result = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(result, 0, con1);
  SET_VECTOR_ELT(result, 1, con2);

  UNPROTECT(3);
  return result;
}

SEXP processx__connection_set_std(SEXP con, int which, int drop) {
  processx_connection_t *ccon = R_ExternalPtrAddr(con);
  if (!ccon) R_THROW_ERROR("Invalid connection object");
  SEXP result = R_NilValue;

#ifdef _WIN32
  int fd, ret;
  if (!drop) {
    int saved = _dup(which);
    processx_file_handle_t os_handle;
    if (saved == -1) {
      R_THROW_POSIX_ERROR("Cannot save stdout/stderr for rerouting");
    }
    os_handle = (HANDLE) _get_osfhandle(saved) ;
    processx_c_connection_create(os_handle, PROCESSX_FILE_TYPE_PIPE,
				 "", NULL, &result);
  }

  fd = _open_osfhandle((intptr_t) ccon->handle.handle, 0);
  ret = _dup2(fd, which);
  if (ret) R_THROW_POSIX_ERROR("Cannot reroute stdout/stderr");

#else
  const char *what[] = { "stdin", "stdout", "stderr" };
  int ret;
  if (!drop) {
    processx_file_handle_t os_handle = dup(which);
    if (os_handle == -1) {
      R_THROW_SYSTEM_ERROR("Cannot save %s for rerouting", what[which]);
    }
    processx_c_connection_create(os_handle, PROCESSX_FILE_TYPE_PIPE,
				 "", NULL, &result);
  }
  ret = dup2(ccon->handle, which);
  if (ret == -1) {
    R_THROW_SYSTEM_ERROR("Cannot reroute %s", what[which]);
  }
#endif

  return result;
}

SEXP processx_connection_set_stdout(SEXP con, SEXP drop) {
  return processx__connection_set_std(con, 1, LOGICAL(drop)[0]);
}

SEXP processx_connection_set_stderr(SEXP con, SEXP drop) {
  return processx__connection_set_std(con, 2, LOGICAL(drop)[0]);
}

SEXP processx_connection_get_fileno(SEXP con) {
  processx_connection_t *ccon = R_ExternalPtrAddr(con);
  if (!ccon) R_THROW_ERROR("Invalid connection object");
  int fd;

#ifdef _WIN32
  fd = _open_osfhandle((intptr_t) ccon->handle.handle, 0);
#else
  fd = ccon->handle;
#endif

  return ScalarInteger(fd);
}

#ifdef _WIN32

/*
 * Clear the HANDLE_FLAG_INHERIT flag from all HANDLEs that were inherited
 * the parent process. Don't check for errors - the stdio handles may not be
 * valid, or may be closed already. There is no guarantee that this function
 * does a perfect job.
 */

SEXP processx_connection_disable_inheritance() {
  HANDLE handle;
  STARTUPINFOW si;

  /* Make the windows stdio handles non-inheritable. */
  handle = GetStdHandle(STD_INPUT_HANDLE);
  if (handle != NULL && handle != INVALID_HANDLE_VALUE) {
    SetHandleInformation(handle, HANDLE_FLAG_INHERIT, 0);
  }

  handle = GetStdHandle(STD_OUTPUT_HANDLE);
  if (handle != NULL && handle != INVALID_HANDLE_VALUE) {
    SetHandleInformation(handle, HANDLE_FLAG_INHERIT, 0);
  }

  handle = GetStdHandle(STD_ERROR_HANDLE);
  if (handle != NULL && handle != INVALID_HANDLE_VALUE) {
    SetHandleInformation(handle, HANDLE_FLAG_INHERIT, 0);
  }

  /* Make inherited CRT FDs non-inheritable. */
  GetStartupInfoW(&si);
  if (processx__stdio_verify(si.lpReserved2, si.cbReserved2)) {
    processx__stdio_noinherit(si.lpReserved2);
  }

  return R_NilValue;
}

#else

SEXP processx_connection_disable_inheritance(void) {
  int fd;

  /* Set the CLOEXEC flag on all open descriptors. Unconditionally try the
   * first 16 file descriptors. After that, bail out after the first error.
   * We skip the standard streams, because R and `system()` is not prepared
   * to not inheriting stdin and eg. an R subprocess does not even start in
   * system(). See https://github.com/r-lib/callr/issues/236. */

  int firstfd = 3;
  if (getenv("PROCESSX_CLOEXEC_STDIO")) firstfd = 0;
  for (fd = firstfd; ; fd++) {
    if (processx__cloexec_fcntl(fd, 1) && fd > 15) break;
  }

  return R_NilValue;
}

#endif

/* Api from C -----------------------------------------------------------*/

processx_connection_t *processx_c_connection_create(
  processx_file_handle_t os_handle,
  processx_file_type_t type,
  const char *encoding,
  const char *filename,
  SEXP *r_connection) {

  processx_connection_t *con;
  SEXP result, class;

  con = malloc(sizeof(processx_connection_t));
  if (!con) R_THROW_ERROR("cannot create connection, out of memory");

  con->type = type;
  con->is_closed_ = 0;
  con->is_eof_  = 0;
  con->is_eof_raw_ = 0;
  con->close_on_destroy = 1;
  con->iconv_ctx = 0;

  con->buffer = 0;
  con->buffer_allocated_size = 0;
  con->buffer_data_size = 0;

  con->utf8 = 0;
  con->utf8_allocated_size = 0;
  con->utf8_data_size = 0;

  con->encoding = 0;
  if (encoding && encoding[0]) {
    con->encoding = strdup(encoding);
    if (!con->encoding) {
      free(con);
      R_THROW_ERROR("cannot create connection, out of memory");
      return 0;			/* never reached */
    }
  }

  con->filename = 0;
  if (filename) {
    con->filename = strdup(filename);
    if (!con->filename) {
      if (con->encoding) free(con->encoding);
      free(con);
      R_THROW_ERROR("cannot create connection, out of memory");
      return 0;			/* never reached */
    }
  }

#ifdef _WIN32
  con->handle.handle = os_handle;
  memset(&con->handle.overlapped, 0, sizeof(OVERLAPPED));
  con->handle.read_pending = FALSE;
  con->handle.connecting = FALSE;
  con->handle.freelist = FALSE;
#else
  con->handle = os_handle;
#endif

  if (r_connection) {
    result = PROTECT(R_MakeExternalPtr(con, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(result, processx__connection_xfinalizer, 0);
    class = PROTECT(ScalarString(mkChar("processx_connection")));
    setAttrib(result, R_ClassSymbol, class);
    *r_connection = result;
    UNPROTECT(2);
  }

  return con;
}

/* Destroy */
void processx_c_connection_destroy(processx_connection_t *ccon) {

  if (!ccon) return;

  if (ccon->close_on_destroy) processx_c_connection_close(ccon);

  /* Even if not close_on_destroy, for us the connection is closed. */
  ccon->is_closed_ = 1;

#ifdef _WIN32
  /* Check if we can free the connection. If there is a pending read,
     then we cannot. In this case schedule_destroy will add it to a free
     list and return 1. */
  if (processx__connection_schedule_destroy(ccon)) return;
#endif

  if (ccon->iconv_ctx) {
    Riconv_close(ccon->iconv_ctx);
    ccon->iconv_ctx = NULL;
  }

  if (ccon->buffer) { free(ccon->buffer); ccon->buffer = NULL; }
  if (ccon->utf8) { free(ccon->utf8); ccon->utf8 = NULL; }
  if (ccon->encoding) { free(ccon->encoding); ccon->encoding = NULL; }
  if (ccon->filename) { free(ccon->filename); ccon->filename = NULL; }

 #ifdef _WIN32
  if (ccon->handle.overlapped.hEvent) {
    CloseHandle(ccon->handle.overlapped.hEvent);
  }
  ccon->handle.overlapped.hEvent = 0;
#endif

  free(ccon);
}

/* Read characters */
ssize_t processx_c_connection_read_chars(processx_connection_t *ccon,
					 void *buffer,
					 size_t nbyte) {
  size_t utf8_chars, utf8_bytes;

  if (nbyte < 4) {
    R_THROW_ERROR("Buffer size must be at least 4 bytes, to allow multibyte "
	  "characters");
  }

  processx__connection_find_chars(ccon, -1, nbyte, &utf8_chars, &utf8_bytes);

  memcpy(buffer, ccon->utf8, utf8_bytes);
  ccon->utf8_data_size -= utf8_bytes;
  memmove(ccon->utf8, ccon->utf8 + utf8_bytes, ccon->utf8_data_size);

  return utf8_bytes;
}

/**
 * Read a single line, ending with \n
 *
 * The trailing \n character is not copied to the buffer.
 *
 * @param ccon Connection.
 * @param linep Must point to a buffer pointer. If must not be NULL. If
 *   the buffer pointer is NULL, it will be allocated. If it is not NULL,
 *   it might be reallocated using `realloc`, as needed.
 * @param linecapp Initial size of the buffer. It will be updated if the
 *   buffer is newly allocated or reallocated.
 * @return Number of characters read, not including the \n character.
 *   It returns -1 on EOF. If the connection is not at EOF yet, but there
 *   is nothing to read currently, it returns 0. If 0 is returned, `linep`
 *   and `linecapp` are not touched.
 *
 */
ssize_t processx_c_connection_read_line(processx_connection_t *ccon,
					char **linep, size_t *linecapp) {

  int eof = 0;
  ssize_t newline;

  if (!linep) {
    R_THROW_ERROR("cannot read line, linep cannot be a null pointer");
  }
  if (!linecapp) {
    R_THROW_ERROR("cannot read line, linecapp cannot be a null pointer");
  }

  if (ccon->is_eof_) return -1;

  /* Read until a newline character shows up, or there is nothing more
     to read (at least for now). */
  newline = processx__connection_read_until_newline(ccon);

  /* If there is no newline at the end of the file, we still add the
     last line. */
  if (ccon->is_eof_raw_ && ccon->utf8_data_size != 0 &&
      ccon->buffer_data_size == 0 &&
      ccon->utf8[ccon->utf8_data_size - 1] != '\n') {
    eof = 1;
  }

  /* We cannot serve a line currently. Maybe later. */
  if (newline == -1 && ! eof) return 0;

  /* Newline will contain the end of the line now, even if EOF */
  if (newline == -1) newline = ccon->utf8_data_size;
  if (ccon->utf8[newline - 1] == '\r') newline--;

  if (! *linep) {
    *linep = malloc(newline + 1);
    *linecapp = newline + 1;
  } else if (*linecapp < newline + 1) {
    char *tmp = realloc(*linep, newline + 1);
    if (!tmp) R_THROW_ERROR("cannot read line, out of memory");
    *linep = tmp;
    *linecapp = newline + 1;
  }

  memcpy(*linep, ccon->utf8, newline);
  (*linep)[newline] = '\0';

  if (!eof) {
    ccon->utf8_data_size -= (newline + 1);
    memmove(ccon->utf8, ccon->utf8 + newline + 1, ccon->utf8_data_size);
  } else {
    ccon->utf8_data_size = 0;
  }

  return newline;
}

/* Write bytes */
ssize_t processx_c_connection_write_bytes(
  processx_connection_t *ccon,
  const void *buffer,
  size_t nbytes) {

  PROCESSX_CHECK_VALID_CONN(ccon);

  /* Do not allow writing to an un-accepted server socket */
  if (ccon->type == PROCESSX_FILE_TYPE_SOCKET &&
      (ccon->state == PROCESSX_SOCKET_LISTEN ||
       ccon->state == PROCESSX_SOCKET_LISTEN_PIPE_READY)) {
    R_THROW_ERROR("Cannot write to an un-accepted socket connection");
  }

#ifdef _WIN32
  DWORD written;
  BOOL ret = WriteFile(
    /* hFile =                  */ ccon->handle.handle,
    /* lpBuffer =               */ buffer,
    /* nNumberOfBytesToWrite =  */ nbytes,
    /* lpNumberOfBytesWritten = */ &written,
    /* lpOverlapped =           */ NULL);
  if (!ret) R_THROW_SYSTEM_ERROR("Cannot write connection");
  return (ssize_t) written;
#else
  /* Need to ignore SIGPIPE here, otherwise R might crash */
  struct sigaction old_handler, new_handler;
  memset(&new_handler, 0, sizeof(new_handler));
  sigemptyset(&new_handler.sa_mask);
  new_handler.sa_handler = SIG_IGN;
  sigaction(SIGPIPE, &new_handler, &old_handler );

  ssize_t ret = write(ccon->handle, buffer, nbytes);
  int err = errno;

  sigaction(SIGPIPE, &old_handler, NULL );

  if (ret == -1) {
    if (err == EAGAIN || err == EWOULDBLOCK) {
      return 0;
    } else {
      R_THROW_SYSTEM_ERROR("Cannot write connection");
    }
  }
  return ret;
#endif
}

/* Check if the connection has ended */
int processx_c_connection_is_eof(processx_connection_t *ccon) {
  return ccon->is_eof_;
}

/* Close */
void processx_c_connection_close(processx_connection_t *ccon) {
#ifdef _WIN32
  if (ccon->handle.handle) {
    CloseHandle(ccon->handle.handle);
  }
  ccon->handle.handle = 0;
#else
  if (ccon->handle >= 0) close(ccon->handle);
  ccon->handle = -1;
#endif
  ccon->is_closed_ = 1;
}

int processx_c_connection_is_closed(processx_connection_t *ccon) {
  return ccon->is_closed_;
}

#ifdef _WIN32

/* TODO: errors */
int processx__socket_pair(SOCKET fds[2]) {
  struct sockaddr_in inaddr;
  struct sockaddr addr;
  SOCKET lst = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  memset(&inaddr, 0, sizeof(inaddr));
  memset(&addr, 0, sizeof(addr));
  inaddr.sin_family = AF_INET;
  inaddr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
  inaddr.sin_port = 0;
  int yes = 1;
  setsockopt(lst, SOL_SOCKET, SO_REUSEADDR, (char*)&yes, sizeof(yes));
  bind(lst, (struct sockaddr *)&inaddr, sizeof(inaddr));
  listen(lst, 1);
  int len = sizeof(inaddr);
  getsockname(lst, &addr,&len);
  fds[0] = socket(AF_INET, SOCK_STREAM, 0);
  connect(fds[0], &addr, len);
  fds[1] = accept(lst,0,0);
  closesocket(lst);
  return 0;
}

int processx_c_connection_poll(processx_pollable_t pollables[],
			       size_t npollables, int timeout) {

  int hasdata = 0;
  size_t i, j = 0, selj = 0;
  int *ptr;
  int timeleft = timeout;
  DWORD bytes;
  OVERLAPPED *overlapped = 0;
  ULONG_PTR key;
  int *events;

  FD_ZERO(&processx__readfds);
  FD_ZERO(&processx__writefds);
  FD_ZERO(&processx__exceptionfds);

  events = (int*) R_alloc(npollables, sizeof(int));

  /* First iteration, we call the pre-poll method, and collect the
     handles for the IOCP, and the fds for select(). */
  for (i = 0; i < npollables; i++) {
    processx_pollable_t *el = pollables + i;
    events[i] = PXSILENT;
    if (el->pre_poll_func) events[i] = el->pre_poll_func(el);
    switch (events[i]) {
    case PXHANDLE:
      j++;
      break;
    default:
      break;
    }
  }

  /* j contains the number of IOCP handles to poll */

  ptr = (int*) R_alloc(j, sizeof(int));

  for (i = 0, j = 0; i < npollables; i++) {
    processx_pollable_t *el = pollables + i;

    switch (events[i]) {
    case PXNOPIPE:
    case PXCLOSED:
    case PXSILENT:
      el->event = events[i];
      break;

    case PXREADY:
      hasdata++;
      el->event = events[i];
      break;

    case PXCONNECT:
      hasdata++;
      el->event = events[i];
      break;

    case PXHANDLE:
      el->event = PXSILENT;
      ptr[j] = i;
      j++;
      break;

    case PXSELECT: {
      SEXP elem;
      el->event = PXSILENT;
      int k, n;
      elem = VECTOR_ELT(el->fds, 0);
      n = LENGTH(elem);
      selj += n;
      for (k = 0; k < n; k++) FD_SET(INTEGER(elem)[k], &processx__readfds);
      elem = VECTOR_ELT(el->fds, 1);
      n = LENGTH(elem);
      selj += n;
      for (k = 0; k < n; k++) FD_SET(INTEGER(elem)[k], &processx__writefds);
      elem = VECTOR_ELT(el->fds, 2);
      n = LENGTH(elem);
      selj += n;
      for (k = 0; k < n; k++) FD_SET(INTEGER(elem)[k], &processx__exceptionfds);
    } }
  }

  if (j == 0 && selj == 0) return hasdata;

  if (hasdata) timeout = timeleft = 0;

  if (selj != 0) {
    processx__socket_pair(processx__notify_socket);
    FD_SET(processx__notify_socket[0], &processx__readfds);
    processx__select = 1;
  } else {
    processx__select = 0;
  }

  while (timeout < 0 || timeleft >= 0) {
    int poll_timeout;
    if (timeout < 0 || timeleft > PROCESSX_INTERRUPT_INTERVAL) {
      poll_timeout = PROCESSX_INTERRUPT_INTERVAL;
    } else {
      poll_timeout = timeleft;
    }

    BOOL sres;
    if (selj == 0) {
      sres = processx__thread_getstatus(&bytes, &key, &overlapped,
					poll_timeout);
    } else {
      sres = processx__thread_getstatus_select(&bytes, &key, &overlapped,
					       poll_timeout);
    }
    DWORD err = sres ? ERROR_SUCCESS : processx__thread_get_last_error();

    /* See if there was any data from the curl sockets */

    if (processx__select) {
      for (i = 0; i < npollables; i++) {
	if (events[i] == PXSELECT) {
	  processx_pollable_t *el = pollables + i;
	  SEXP elem;
	  int k, n;
	  int has = 0;
	  elem = VECTOR_ELT(el->fds, 0);
	  n = LENGTH(elem);
	  for (k = 0; k < n; k++) {
	    if (FD_ISSET(INTEGER(elem)[k], &processx__readfds)) has = 1;
	    FD_SET(INTEGER(elem)[k], &processx__readfds);
	  }
	  elem = VECTOR_ELT(el->fds, 1);
	  n = LENGTH(elem);
	  for (k = 0; k < n; k++) {
	    if (FD_ISSET(INTEGER(elem)[k], &processx__writefds)) has = 1;
	    FD_SET(INTEGER(elem)[k], &processx__writefds);
	  }
	  elem = VECTOR_ELT(el->fds, 2);
	  n = LENGTH(elem);
	  for (k = 0; k < n; k++) {
	    if (FD_ISSET(INTEGER(elem)[k], &processx__exceptionfds)) has = 1;
	    FD_SET(INTEGER(elem)[k], &processx__exceptionfds);
	  }
	  if (has) {
	    el->event = PXEVENT;
	    hasdata++;
	  }
	}
      }
    }

    /* See if there was any data from the IOCP */

    if (overlapped) {
      /* data */
      processx_connection_t *con = (processx_connection_t*) key;
      int poll_idx = con->poll_idx;
      con->handle.read_pending = FALSE;
      con->buffer_data_size += bytes;
      if (con->buffer_data_size > 0) processx__connection_to_utf8(con);
      if (con->type == PROCESSX_FILE_TYPE_ASYNCFILE) {
	/* TODO: larger files */
	con->handle.overlapped.Offset += bytes;
      }

      if (!bytes && !con->handle.connecting) {
	con->is_eof_raw_ = 1;
	if (con->utf8_data_size == 0 && con->buffer_data_size == 0) {
	  con->is_eof_ = 1;
	}
      }

      if (con->handle.freelist) processx__connection_freelist_remove(con);

      if (poll_idx < npollables &&
	  pollables[poll_idx].object == con) {
	if (con->handle.connecting && con->type == PROCESSX_FILE_TYPE_SOCKET) {
	  pollables[poll_idx].event = PXCONNECT;
	  con->state = PROCESSX_SOCKET_LISTEN_PIPE_READY;
	} else {
	  pollables[poll_idx].event = PXREADY;
	}
	hasdata++;
      }
      // TODO: if we just connected a FIFO asynchronously, then we could
      // start a new read here, to avoid returning "ready" without data.
      // NOTE: when updating this, also update internals.Rmd!
      con->handle.connecting = FALSE;
    } else if (err != WAIT_TIMEOUT && err != ERROR_SUCCESS) {
      R_THROW_SYSTEM_ERROR_CODE(err, "Cannot poll");
    }

    if (hasdata) break;
    R_CheckUserInterrupt();
    timeleft -= PROCESSX_INTERRUPT_INTERVAL;
  }

  if (hasdata == 0) {
    for (i = 0; i < j; i++) pollables[ptr[i]].event = PXTIMEOUT;
  }

  closesocket(processx__notify_socket[0]);
  closesocket(processx__notify_socket[1]);

  return hasdata;
}

#else

static int processx__poll_decode(short code) {
  if (code & POLLNVAL) return PXCLOSED;
  if (code & POLLIN || code & POLLHUP || code & POLLOUT) return PXREADY;
  return PXSILENT;
}

/* Poll connections and other pollable handles */
int processx_c_connection_poll(processx_pollable_t pollables[],
			       size_t npollables, int timeout) {

  int hasdata = 0;
  size_t i, j = 0;
  struct pollfd *fds;
  int *ptr;
  int ret;
  int *events;

  if (npollables == 0) return 0;

  /* Need to allocate this, because we need to put in the fds, maybe */
  events = (int*) R_alloc(npollables, sizeof(int));

  /* First iteration, we call the pre-poll method, and collect the
     fds to poll. */
  for (i = 0; i < npollables; i++) {
    processx_pollable_t *el = pollables + i;
    events[i] = PXSILENT;
    if (el->pre_poll_func) events[i] = el->pre_poll_func(el);
    switch (events[i]) {
    case PXHANDLE:
      j++;
      break;
    case PXSELECT: {
      /* This is three vectors of fds to poll, in an R list */
      int w;
      for (w = 0; w < 3; w++) {
        j += LENGTH(VECTOR_ELT(el->fds, w));
      } }
    default:
      break;
    }
  }

  /* j contains the number of fds to poll now */

  fds = (struct pollfd*) R_alloc(j, sizeof(struct pollfd));
  ptr = (int*) R_alloc(j, sizeof(int));

  /* Need to go over them again, collect the ones that we need to poll */
  for (i = 0, j = 0; i < npollables; i++) {
    processx_pollable_t *el = pollables + i;
    switch (events[i]) {
    case PXNOPIPE:
    case PXCLOSED:
    case PXSILENT:
      el->event = events[i];
      break;

    case PXREADY:
      hasdata++;
      el->event = events[i];
      break;

    case PXHANDLE:
      el->event = PXSILENT;
      fds[j].fd = el->handle;
      fds[j].events = POLLIN;
      fds[j].revents = 0;
      ptr[j] = (int) i;
      j++;
      break;

    case PXSELECT: {
      int pollevs[3] = { POLLIN, POLLOUT, POLLIN | POLLOUT };
      int w;
      el->event = PXSILENT;
      for (w = 0; w < 3; w++) {
        SEXP elem = VECTOR_ELT(el->fds, w);
        int k, n = LENGTH(elem);
        for (k = 0; k < n; k++) {
          fds[j].fd = INTEGER(elem)[k];
          fds[j].events = pollevs[w];
          fds[j].revents = 0;
          ptr[j] = (int) i;
          j++;
        }
      }
      break; }
    }
  }

  /* Nothing to poll */
  if (j == 0) return hasdata;

  /* If we already have some data, then we don't wait any more,
     just check if other connections are ready */
  ret = processx__interruptible_poll(fds, (nfds_t) j,
				     hasdata > 0 ? 0 : timeout);

  if (ret == -1) {
    R_THROW_SYSTEM_ERROR("Processx poll error");

  } else if (ret == 0) {
    if (hasdata == 0) {
      for (i = 0; i < j; i++) pollables[ptr[i]].event = PXTIMEOUT;
    }

  } else {
    for (i = 0; i < j; i++) {
      if (events[ptr[i]] == PXSELECT) {
        if (pollables[ptr[i]].event == PXSILENT) {
          int ev = fds[i].revents;
          if (ev & (POLLNVAL | POLLIN | POLLHUP | POLLOUT)) {
            pollables[ptr[i]].event = PXEVENT;
          }
        }
      } else {
        pollables[ptr[i]].event = processx__poll_decode(fds[i].revents);
        if (pollables[ptr[i]].event == PXREADY) {
          hasdata ++;
          processx_connection_t *ccon = pollables[ptr[i]].object;
          if (ccon -> type == PROCESSX_FILE_TYPE_SOCKET &&
              ccon -> state == PROCESSX_SOCKET_LISTEN) {
            pollables[ptr[i]].event = PXCONNECT;
          }
        }
      }
    }
  }

  return hasdata;
}

#endif

#ifdef _WIN32

void processx__connection_start_read(processx_connection_t *ccon) {
  DWORD bytes_read;
  BOOLEAN res;
  size_t todo;

  if (! ccon->handle.handle) return;

  if (ccon->handle.read_pending) return;

  if (!ccon->buffer) processx__connection_alloc(ccon);

  todo = ccon->buffer_allocated_size - ccon->buffer_data_size;

  if (ccon->type == PROCESSX_FILE_TYPE_SOCKET &&
      ccon->state == PROCESSX_SOCKET_LISTEN) {
    ccon->handle.connecting = TRUE;
    res = processx__thread_connectpipe(ccon);

  } else {
    res = processx__thread_readfile(
      ccon,
      ccon->buffer + ccon->buffer_data_size,
      todo,
      &bytes_read);
  }

  if (!res) {
    DWORD err = processx__thread_get_last_error();
    if (err == ERROR_BROKEN_PIPE || err == ERROR_HANDLE_EOF) {
      ccon->is_eof_raw_ = 1;
      if (ccon->utf8_data_size == 0 && ccon->buffer_data_size == 0) {
        ccon->is_eof_ = 1;
      }
      if (ccon->buffer_data_size) processx__connection_to_utf8(ccon);
    } else if (err == ERROR_IO_PENDING) {
      ccon->handle.read_pending = TRUE;
    } else if (err == ERROR_PIPE_LISTENING &&
	       (ccon->type == PROCESSX_FILE_TYPE_ASYNCPIPE ||
                ccon->type == PROCESSX_FILE_TYPE_SOCKET)) {
      ccon->handle.read_pending = TRUE;
      ccon->handle.connecting = TRUE;
      processx__thread_connectpipe(ccon);
    } else if (err == ERROR_PIPE_CONNECTED &&
	       ccon->type == PROCESSX_FILE_TYPE_SOCKET) {
      ccon->handle.read_pending = FALSE;
      ccon->handle.connecting = FALSE;
      ccon->state = PROCESSX_SOCKET_LISTEN_PIPE_READY;
    } else {
      ccon->handle.read_pending = FALSE;
      R_THROW_SYSTEM_ERROR_CODE(err, "reading from connection");
    }
  } else {
    /* Returned synchronously, but the event will be still signalled,
       so we just drop the sync data for now. */
    ccon->handle.read_pending  = TRUE;
  }
}

#endif

/* Poll a connection
 *
 * Checks if there is anything in the buffer. If yes, it returns
 * PXREADY. Otherwise it returns the handle.
 *
 * We can read immediately (without an actual device read), potentially:
 * 1. if the connection is already closed, we return PXCLOSED
 * 2. if the connection is already EOF, we return PXREADY
 * 3. if there is data in the UTF8 buffer, we return PXREADY
 * 4. if there is data in the raw buffer, and the raw file was EOF, we
 *    return PXREADY, because we can surely return something, even if the
 *    raw buffer has incomplete UTF8 characters.
 * 5. otherwise, if there is something in the raw buffer, we try
 *    to convert it to UTF8.
 */

#define PROCESSX__I_PRE_POLL_FUNC_CONNECTION_READY do {			\
  if (!ccon) return PXNOPIPE;						\
  if (ccon->is_closed_) return PXCLOSED;				\
  if (ccon->is_eof_) return PXREADY;					\
  if (ccon->utf8_data_size > 0) return PXREADY;				\
  if (ccon->buffer_data_size > 0 && ccon->is_eof_raw_) return PXREADY;	\
  if (ccon->buffer_data_size > 0) {					\
    processx__connection_to_utf8(ccon);					\
    if (ccon->utf8_data_size > 0) return PXREADY;			\
  } } while (0)

int processx_i_pre_poll_func_connection(processx_pollable_t *pollable) {

  processx_connection_t *ccon = pollable->object;

  PROCESSX__I_PRE_POLL_FUNC_CONNECTION_READY;

#ifdef _WIN32
  if (ccon->type == PROCESSX_FILE_TYPE_SOCKET &&
      ccon->state == PROCESSX_SOCKET_LISTEN_PIPE_READY) {
    return PXCONNECT;
  } else {
    processx__connection_start_read(ccon);
    /* Starting to read may actually get some data, or an EOF, so check again */
    PROCESSX__I_PRE_POLL_FUNC_CONNECTION_READY;
    if (ccon->type == PROCESSX_FILE_TYPE_SOCKET &&
	ccon->state == PROCESSX_SOCKET_LISTEN_PIPE_READY) {
      return PXCONNECT;
    }
    pollable->handle = ccon->handle.overlapped.hEvent;
  }
#else
  pollable->handle = ccon->handle;
#endif

  return PXHANDLE;
}

int processx_c_pollable_from_connection(
  processx_pollable_t *pollable,
  processx_connection_t *ccon) {

  pollable->pre_poll_func = processx_i_pre_poll_func_connection;
  pollable->object = ccon;
  pollable->free = 0;
  pollable->fds = R_NilValue;
  return 0;
}

int processx_i_pre_poll_func_curl(processx_pollable_t *pollable) {
  return PXSELECT;
}

int processx_c_pollable_from_curl(
  processx_pollable_t *pollable,
  SEXP fds) {

  pollable->pre_poll_func = processx_i_pre_poll_func_curl;
  pollable->object = NULL;
  pollable->free = 0;
  pollable->fds = fds;
  return 0;
}

processx_file_handle_t processx_c_connection_fileno(
  const processx_connection_t *con) {
#ifdef _WIN32
  return con->handle.handle;
#else
  return con->handle;
#endif
}

/* --------------------------------------------------------------------- */
/* Internals                                                             */
/* --------------------------------------------------------------------- */

/**
 * Work out how many UTF-8 characters we can read
 *
 * It might try to read more data, but it does not modify the buffer
 * otherwise.
 *
 * @param ccon Connection.
 * @param maxchars Maximum number of characters to find.
 * @param maxbytes Maximum number of bytes to check while searching.
 * @param chars Number of characters found is stored here.
 * @param bytes Number of bytes the `chars` characters span.
 *
 */

static void processx__connection_find_chars(processx_connection_t *ccon,
					    ssize_t maxchars,
					    ssize_t maxbytes,
					    size_t *chars,
					    size_t *bytes) {

  int should_read_more;

  PROCESSX_CHECK_VALID_CONN(ccon);

  should_read_more = ! ccon->is_eof_ && ccon->utf8_data_size == 0;
  if (should_read_more) processx__connection_read(ccon);

  if (ccon->utf8_data_size == 0 || maxchars == 0) { *bytes = 0; return; }

  /* At at most cnchars characters from the UTF8 buffer */
  processx__connection_find_utf8_chars(ccon, maxchars, maxbytes, chars,
				       bytes);
}

/**
 * Find one or more lines in the buffer
 *
 * Since the buffer is UTF-8 encoded, `\n` is assumed as end-of-line
 * character.
 *
 * @param ccon Connection.
 * @param maxlines Maximum number of lines to find.
 * @param lines Number of lines found is stored here.
 * @param eof If the end of the file is reached, and there is no `\n`
 *   at the end of the file, this is set to 1.
 *
 */

static void processx__connection_find_lines(processx_connection_t *ccon,
					    ssize_t maxlines,
					    size_t *lines,
					    int *eof ) {

  ssize_t newline;

  *eof = 0;

  if (maxlines < 0) maxlines = 1000;

  PROCESSX_CHECK_VALID_CONN(ccon);

  /* Read until a newline character shows up, or there is nothing more
     to read (at least for now). */
  newline = processx__connection_read_until_newline(ccon);

  /* Count the number of lines we got. */
  while (newline != -1 && *lines < maxlines) {
    (*lines) ++;
    newline = processx__find_newline(ccon, /* start = */ newline + 1);
  }

  /* If there is no newline at the end of the file, we still add the
     last line. */
  if (ccon->is_eof_raw_ && ccon->utf8_data_size != 0 &&
      ccon->buffer_data_size == 0 &&
      ccon->utf8[ccon->utf8_data_size - 1] != '\n') {
    *eof = 1;
  }

}

static void processx__connection_xfinalizer(SEXP con) {
  processx_connection_t *ccon = R_ExternalPtrAddr(con);
  processx_c_connection_destroy(ccon);
}

static ssize_t processx__find_newline(processx_connection_t *ccon,
				     size_t start) {

  if (ccon->utf8_data_size == 0) return -1;
  const char *ret = ccon->utf8 + start;
  const char *end = ccon->utf8 + ccon->utf8_data_size;

  while (ret < end && *ret != '\n') ret++;

  if (ret < end) return ret - ccon->utf8; else return -1;
}

static ssize_t processx__connection_read_until_newline
  (processx_connection_t *ccon) {

  char *ptr, *end;

  /* Make sure we try to have something, unless EOF */
  if (ccon->utf8_data_size == 0) processx__connection_read(ccon);
  if (ccon->utf8_data_size == 0) return -1;

  /* We have sg in the utf8 at this point */

  ptr = ccon->utf8;
  end = ccon->utf8 + ccon->utf8_data_size;
  while (1) {
    ssize_t new_bytes;
    while (ptr < end && *ptr != '\n') ptr++;

    /* Have we found a newline? */
    if (ptr < end) return ptr - ccon->utf8;

    /* No newline, but EOF? */
    if (ccon->is_eof_) return -1;

    /* Maybe we can read more, but might need a bigger utf8.
     * The 8 bytes is definitely more than what we need for a UTF8
     * character, and this makes sure that we don't stop just because
     * no more UTF8 characters fit in the UTF8 buffer. */
    if (ccon->utf8_data_size >= ccon->utf8_allocated_size - 8) {
      size_t ptrnum = ptr - ccon->utf8;
      size_t endnum = end - ccon->utf8;
      processx__connection_realloc(ccon);
      ptr = ccon->utf8 + ptrnum;
      end = ccon->utf8 + endnum;
    }
    new_bytes = processx__connection_read(ccon);

    /* If we cannot read now, then we give up */
    if (new_bytes == 0) return -1;
  }
}

/* Allocate buffer for reading */

static void processx__connection_alloc(processx_connection_t *ccon) {
  ccon->buffer = malloc(64 * 1024);
  if (!ccon->buffer) R_THROW_ERROR("Cannot allocate memory for processx buffer");
  ccon->buffer_allocated_size = 64 * 1024;
  ccon->buffer_data_size = 0;

  ccon->utf8 = malloc(64 * 1024);
  if (!ccon->utf8) {
    free(ccon->buffer);
    R_THROW_ERROR("Cannot allocate memory for processx buffer");
  }
  ccon->utf8_allocated_size = 64 * 1024;
  ccon->utf8_data_size = 0;
}

/* We only really need to re-alloc the UTF8 buffer, because the
   other buffer is transient, even if there are no newline characters. */

static void processx__connection_realloc(processx_connection_t *ccon) {
  size_t new_size = (size_t) (ccon->utf8_allocated_size * 1.2);
  void *nb;
  if (new_size == ccon->utf8_allocated_size) new_size = 2 * new_size;
  nb = realloc(ccon->utf8, new_size);
  if (!nb) R_THROW_ERROR("Cannot allocate memory for processx line");
  ccon->utf8 = nb;
  ccon->utf8_allocated_size = new_size;
}

/* Read as much as we can. This is the only function that explicitly
   works with the raw buffer. It is also the only function that actually
   reads from the data source.

   When this is called, the UTF8 buffer is probably empty, but the raw
   buffer might not be. */

#ifdef _WIN32

ssize_t processx__connection_read(processx_connection_t *ccon) {
  DWORD todo, bytes_read = 0;

  /* Do not allow reading on an un-accepted server socket */
  if (ccon->type == PROCESSX_FILE_TYPE_SOCKET &&
      (ccon->state == PROCESSX_SOCKET_LISTEN ||
       ccon->state == PROCESSX_SOCKET_LISTEN_PIPE_READY)) {
    R_THROW_ERROR("Cannot read from an un-accepted socket connection");
  }

  /* Nothing to read, nothing to convert to UTF8 */
  if (ccon->is_eof_raw_ && ccon->buffer_data_size == 0) {
    if (ccon->utf8_data_size == 0) ccon->is_eof_ = 1;
    return 0;
  }

  if (!ccon->buffer) processx__connection_alloc(ccon);

  /* If cannot read anything more, then try to convert to UTF8 */
  todo = ccon->buffer_allocated_size - ccon->buffer_data_size;
  if (todo == 0) return processx__connection_to_utf8(ccon);

  /* Otherwise we read. If there is no read pending, we start one. */
  processx__connection_start_read(ccon);

  /* A read might be pending at this point. See if it has finished. */
  if (ccon->handle.read_pending) {
    ULONG_PTR key;
    DWORD bytes;
    OVERLAPPED *overlapped = 0;

    while (1) {
      BOOL sres = processx__thread_getstatus(&bytes, &key, &overlapped, 0);
      DWORD err = sres ? ERROR_SUCCESS : processx__thread_get_last_error();
      if (overlapped) {
	processx_connection_t *con = (processx_connection_t *) key;
	con->handle.read_pending = FALSE;
	con->buffer_data_size += bytes;
	if (con->buffer && con->buffer_data_size > 0) {
	  bytes = processx__connection_to_utf8(con);
	}
	if (con->type == PROCESSX_FILE_TYPE_ASYNCFILE) {
	  /* TODO: large files */
	  con->handle.overlapped.Offset += bytes;
	}
	if (!bytes) {
	  con->is_eof_raw_ = 1;
	  if (con->utf8_data_size == 0 && con->buffer_data_size == 0) {
	    con->is_eof_ = 1;
	  }
	}

	if (con->handle.freelist) processx__connection_freelist_remove(con);

	if (con == ccon) {
	  bytes_read = bytes;
	  break;
	}

      } else if (err != WAIT_TIMEOUT) {
	R_THROW_SYSTEM_ERROR_CODE(err, "Read error");

      } else {
	break;
      }
    }
  }

  return bytes_read;
}

#else

static ssize_t processx__connection_read(processx_connection_t *ccon) {
  ssize_t todo, bytes_read;

  /* Nothing to read, nothing to convert to UTF8 */
  if (ccon->is_eof_raw_ && ccon->buffer_data_size == 0) {
    if (ccon->utf8_data_size == 0) ccon->is_eof_ = 1;
    return 0;
  }

  if (!ccon->buffer) processx__connection_alloc(ccon);

  /* If cannot read anything more, then try to convert to UTF8 */
  todo = ccon->buffer_allocated_size - ccon->buffer_data_size;
  if (todo == 0) return processx__connection_to_utf8(ccon);

  /* Otherwise we read */
  bytes_read = read(ccon->handle, ccon->buffer + ccon->buffer_data_size, todo);

  if (bytes_read == 0) {
    /* EOF */
    ccon->is_eof_raw_ = 1;
    if (ccon->utf8_data_size == 0 && ccon->buffer_data_size == 0) {
      ccon->is_eof_ = 1;
    }

  } else if (bytes_read == -1 && errno == EAGAIN) {
    /* There is still data to read, potentially */
    bytes_read = 0;

  } else if (bytes_read == -1) {
    /* Proper error  */
    R_THROW_SYSTEM_ERROR("Cannot read from processx connection");
  }

  ccon->buffer_data_size += bytes_read;

  /* If there is anything to convert to UTF8, try converting */
  if (ccon->buffer_data_size > 0) {
    bytes_read = processx__connection_to_utf8(ccon);
  } else {
    bytes_read = 0;
  }

  return bytes_read;
}
#endif

static ssize_t processx__connection_to_utf8(processx_connection_t *ccon) {

  const char *inbuf, *inbufold;
  char *outbuf, *outbufold;
  size_t inbytesleft = ccon->buffer_data_size;
  size_t outbytesleft = ccon->utf8_allocated_size - ccon->utf8_data_size;
  size_t r, indone = 0, outdone = 0;
  int moved = 0;
  const char *emptystr = "";
  const char *encoding = ccon->encoding ? ccon->encoding : emptystr;

  inbuf = inbufold = ccon->buffer;
  outbuf = outbufold = ccon->utf8 + ccon->utf8_data_size;

  /* If we this is the first time we are here. */
  if (! ccon->iconv_ctx) ccon->iconv_ctx = Riconv_open("UTF-8", encoding);

  /* If nothing to do, or no space to do more, just return */
  if (inbytesleft == 0 || outbytesleft == 0) return 0;

  while (!moved) {
    r = Riconv(ccon->iconv_ctx, &inbuf, &inbytesleft, &outbuf,
	       &outbytesleft);
    moved = 1;

    if (r == (size_t) -1) {
      /* Error */
      if (errno == E2BIG) {
	/* Output buffer is full, that's fine, we'll try later.
	   Just use what we have done so far. */

      } else if (errno == EILSEQ) {
	/* Invalid characters in encoding, *inbuf points to the beginning
	   of the invalid sequence. We can just try to remove this, and
	   convert again? */
	inbuf++; inbytesleft--;
	if (inbytesleft > 0) moved = 0;

      } else if (errno == EINVAL) {
	/* Does not end with a complete multi-byte character */
	/* This is fine, we'll handle it later, unless we are at the end */
	if (ccon->is_eof_raw_) {
	  warning("Invalid multi-byte character at end of stream ignored");
	  inbuf += inbytesleft; inbytesleft = 0;
	}
      }
    }
  }

  /* We converted 'r' bytes, update the buffer structure accordingly */
  indone = inbuf - inbufold;
  outdone = outbuf - outbufold;
  if (outdone > 0 || indone > 0) {
    ccon->buffer_data_size -= indone;
    memmove(ccon->buffer, ccon->buffer + indone, ccon->buffer_data_size);
    ccon->utf8_data_size += outdone;
  }

  return outdone;
}

/* Try to get at max 'max' UTF8 characters from the buffer. Return the
 * number of characters found, and also the corresponding number of
 * bytes. */

/* Number of additional bytes */
static const unsigned char processx__utf8_length[] = {
  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
  3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
  4,4,4,4,4,4,4,4,5,5,5,5,6,6,6,6 };

static void processx__connection_find_utf8_chars(processx_connection_t *ccon,
						 ssize_t maxchars,
						 ssize_t maxbytes,
						 size_t *chars,
						 size_t *bytes) {

  char *ptr = ccon->utf8;
  char *end = ccon->utf8 + ccon->utf8_data_size;
  size_t length = ccon->utf8_data_size;
  *chars = *bytes = 0;

  while (maxchars != 0 && maxbytes != 0 && ptr < end) {
    int clen, c = (unsigned char) *ptr;

    /* ASCII byte */
    if (c < 128) {
      (*chars) ++; (*bytes) ++; ptr++; length--;
      if (maxchars > 0) maxchars--;
      if (maxbytes > 0) maxbytes--;
      continue;
    }

    /* Catch some errors */
    if (c <  0xc0) goto invalid;
    if (c >= 0xfe) goto invalid;

    clen = processx__utf8_length[c & 0x3f];
    if (length < clen) goto invalid;
    if (maxbytes > 0 && clen > maxbytes) break;
    (*chars) ++; (*bytes) += clen; ptr += clen; length -= clen;
    if (maxchars > 0) maxchars--;
    if (maxbytes > 0) maxbytes -= clen;
  }

  return;

 invalid:
  R_THROW_ERROR("Invalid UTF-8 string, internal error");
}

#ifndef _WIN32

int processx__interruptible_poll(struct pollfd fds[],
				 nfds_t nfds, int timeout) {
  int ret = 0;
  int timeleft = timeout;

  while (timeout < 0 || timeleft > PROCESSX_INTERRUPT_INTERVAL) {
    do {
      ret = poll(fds, nfds, PROCESSX_INTERRUPT_INTERVAL);
    } while (ret == -1 && errno == EINTR);

    /* If not a timeout, then return */
    if (ret != 0) return ret;

    R_CheckUserInterrupt();
    timeleft -= PROCESSX_INTERRUPT_INTERVAL;
  }

  /* Maybe we are not done, and there is a little left from the timeout */
  if (timeleft >= 0) {
    do {
      ret = poll(fds, nfds, timeleft);
    } while (ret == -1 && errno == EINTR);
  }

  return ret;
}

#endif

#ifdef _WIN32

processx__connection_freelist_t freelist_head = { 0, 0 };
processx__connection_freelist_t *freelist = &freelist_head;

int processx__connection_freelist_add(processx_connection_t *ccon) {
  if (ccon->handle.freelist) return 0;
  processx__connection_freelist_t *node =
    calloc(1, sizeof(processx__connection_freelist_t));
  if (!node) R_THROW_ERROR("Cannot add to connection freelist, this is a leak");

  node->ccon = ccon;
  node->next = freelist->next;
  freelist->next = node;
  ccon->handle.freelist = TRUE;

  return 0;
}

void processx__connection_freelist_remove(processx_connection_t *ccon) {
  processx__connection_freelist_t *prev = freelist, *ptr = freelist->next;
  while (ptr) {
    if (ptr->ccon == ccon) {
      prev->next = ptr->next;
      free(ptr);
      return;
    }
    prev = ptr;
    ptr = ptr->next;
  }
}

int processx__connection_schedule_destroy(processx_connection_t *ccon) {
  /* The connection is already closed here, but reads might still be
     pending... if this is the case, then we add the connection to the
     free list. */
  if (ccon->handle.read_pending) {
    processx__connection_freelist_add(ccon);
    return 1;

  } else {
    return 0;
  }
}

#endif

#ifdef _WIN32

SEXP processx_is_valid_fd(SEXP fd) {
  int cfd = INTEGER(fd)[0];
  HANDLE hnd = (HANDLE) _get_osfhandle(cfd);
  int valid =
    hnd != INVALID_HANDLE_VALUE &&
    hnd != NULL &&
    hnd != (HANDLE) (-2);
  return ScalarLogical(valid);
}

#else

SEXP processx_is_valid_fd(SEXP fd) {
  int cfd = INTEGER(fd)[0];
  errno = 0;
  int valid = fcntl(cfd, F_GETFD) != -1 || errno != EBADF;
  return ScalarLogical(valid);
}

#endif

#undef PROCESSX_CHECK_VALID_CONN
