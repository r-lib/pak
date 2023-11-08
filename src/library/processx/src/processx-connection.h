
#ifndef PROCESSX_CONNECTION_H
#define PROCESSX_CONNECTION_H

#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif

#ifdef __INTEL_COMPILER
#define _BSD_SOURCE 1
#define _POSIX_C_SOURCE  200809L
#endif

#ifdef _WIN32
#ifndef FD_SETSIZE
#define FD_SETSIZE 32767
#endif
#include <winsock2.h>
#include <windows.h>
#else
#include <unistd.h>
#endif

#include "errors.h"

#include <Rinternals.h>
#include <R_ext/Riconv.h>

/* --------------------------------------------------------------------- */
/* Data types                                                            */
/* --------------------------------------------------------------------- */

#define ARRAY_SIZE(a) (sizeof(a) / sizeof((a)[0]))

#ifdef _WIN32
typedef HANDLE processx_file_handle_t;
typedef struct {
  HANDLE handle;
  OVERLAPPED overlapped;
  BOOLEAN async;
  BOOLEAN read_pending;
  BOOLEAN connecting;
  BOOLEAN freelist;
} processx_i_connection_t;
#else
typedef int processx_file_handle_t;
typedef int processx_i_connection_t;
#endif

typedef enum {
  PROCESSX_FILE_TYPE_FILE = 1,	/* regular file, blocking IO */
  PROCESSX_FILE_TYPE_ASYNCFILE,	/* regular file, async IO (well, win only) */
  PROCESSX_FILE_TYPE_PIPE,	/* pipe, blocking IO */
  PROCESSX_FILE_TYPE_ASYNCPIPE, /* pipe, async IO */
  PROCESSX_FILE_TYPE_SOCKET
} processx_file_type_t;

typedef enum {
  PROCESSX_SOCKET_LISTEN = 1,
  PROCESSX_SOCKET_LISTEN_PIPE_READY,
  PROCESSX_SOCKET_CONNECTED_SERVER,
  PROCESSX_SOCKET_CONNECTED_CLIENT
} processx_socket_state_t;

typedef struct processx_connection_s {
  processx_file_type_t type;

  int is_closed_;
  int is_eof_;			/* the UTF8 buffer */
  int is_eof_raw_;		/* the raw file */
  int close_on_destroy;

  char *encoding;
  void *iconv_ctx;

  processx_i_connection_t handle;

  char* buffer;
  size_t buffer_allocated_size;
  size_t buffer_data_size;

  char *utf8;
  size_t utf8_allocated_size;
  size_t utf8_data_size;

  int poll_idx;
  char *filename;
  int state;
} processx_connection_t;

struct processx_pollable_s;

/* Generic poll method
 *
 * @param object The thing to poll.
 * @param handle A handle can be returned here, to `poll` or wait on.
 *   If this is not needed, set it to NULL.
 * @param timeout A timeout value can be returned here, for the next
 *   poll. If this is not needed, set it to NULL.
 * @return The result of the pre-polling. PXCLOSED, PXREADY or PXSILENT.
 *   PXREADY: data is readily available, at least one character.
 *     (But maybe not a full line.)
 *   PXSILENT: we don't know if data is available, we need to check the
 *     operating system via `poll` or `WaitForStatus`.
 *   PXHANDLE
 *   PXPOLLFD
 */

typedef int (*processx_connection_pre_poll_func_t)(
  struct processx_pollable_s *pollable);

/* Data structure for a pollable object
 *
 * @member pre_poll_func The function to call on the object, before
 *   the poll/wait system call. The pollable object might have data
 *   available without immediately, without poll/wait. If not, it
 *   will return the file descriptor or HANDLE to poll.
 * @member object The object to pass to `poll_func`.
 * @member free Whether to call `free()` on `object` when finalizing
 *   `processx_pollable_t` objects.
 * @member event The result of the polling is stored here. Possible values:
 *   `PXSILENT` (no data), `PXREADY` (data), `PXTIMEOUT` (timeout).
 * @member fd If the pollable is an fd, then it is stored here instead of
 *   in `object`, for simplicity.
 */

typedef struct processx_pollable_s {
  processx_connection_pre_poll_func_t pre_poll_func;
  void *object;
  int free;
  int event;
  processx_file_handle_t handle;
  SEXP fds;
} processx_pollable_t;

/* --------------------------------------------------------------------- */
/* API from R                                                            */
/* --------------------------------------------------------------------- */

/* Create connection from fd / HANDLE */
SEXP processx_connection_create(SEXP handle, SEXP encoding);

/* Create from fd, this is only different on Windows */
SEXP processx_connection_create_fd(SEXP handle, SEXP encoding, SEXP close);

/* Create file connection */
SEXP processx_connection_create_file(SEXP filename, SEXP read, SEXP write);
SEXP processx_connection_create_fifo(SEXP read, SEXP write,
                                     SEXP filename, SEXP encoding,
                                     SEXP nonblocking);
SEXP processx_connection_connect_fifo(SEXP filename, SEXP read, SEXP write,
                                      SEXP encoding, SEXP nonblocking);
SEXP processx_connection_create_socket(SEXP filename, SEXP encoding);
SEXP processx_connection_connect_socket(SEXP filename, SEXP encoding);
SEXP processx_connection_accept_socket(SEXP con);
SEXP processx_connection_socket_state(SEXP con);

/* Read characters in a given encoding from the connection. */
SEXP processx_connection_read_chars(SEXP con, SEXP nchars);

/* Read lines of characters from the connection. */
SEXP processx_connection_read_lines(SEXP con, SEXP nlines);

/* Write characters */
SEXP processx_connection_write_bytes(SEXP con, SEXP chars);

/* Check if the connection has ended. */
SEXP processx_connection_is_eof(SEXP con);

/* Query file name, if any */
SEXP processx_connection_file_name(SEXP con);

/* Close the connection. */
SEXP processx_connection_close(SEXP con);
SEXP processx_is_closed(SEXP con);

/* Poll connections and other pollable handles */
SEXP processx_connection_poll(SEXP pollables, SEXP timeout);

/* Functions for connection inheritance */
SEXP processx_connection_create_pipepair(SEXP encoding, SEXP nonblocking);

SEXP processx_connection_set_stdout(SEXP con, SEXP drop);

SEXP processx_connection_set_stderr(SEXP con, SEXP drop);

SEXP processx_connection_get_fileno(SEXP con);

SEXP processx_connection_disable_inheritance(void);

SEXP processx_is_valid_fd(SEXP fd);

/* --------------------------------------------------------------------- */
/* API from C                                                            */
/* --------------------------------------------------------------------- */

/* Create connection object */
processx_connection_t *processx_c_connection_create(
  processx_file_handle_t os_handle,
  processx_file_type_t type,
  const char *encoding,
  const char *filename,
  SEXP *r_connection);

/* Destroy connection object. We need this for the C API */
void processx_c_connection_destroy(processx_connection_t *ccon);

/* Read characters */
ssize_t processx_c_connection_read_chars(
  processx_connection_t *con,
  void *buffer,
  size_t nbyte);

/* Read lines of characters */
ssize_t processx_c_connection_read_line(
  processx_connection_t *ccon,
  char **linep,
  size_t *linecapp);

/* Write characters */
ssize_t processx_c_connection_write_bytes(
  processx_connection_t *con,
  const void *buffer,
  size_t nbytes);

/* Check if the connection has ended */
int processx_c_connection_is_eof(
  processx_connection_t *con);

/* Close */
void processx_c_connection_close(
  processx_connection_t *con);
int processx_c_connection_is_closed(
  processx_connection_t *con);

/* Poll connections and other pollable handles */
int processx_c_connection_poll(
  processx_pollable_t pollables[],
  size_t npollables, int timeout);

/* Helper function to create pollable handles*/
int processx_c_pollable_from_connection(
  processx_pollable_t *pollable,
  processx_connection_t *ccon);

int processx_c_pollable_from_curl(
  processx_pollable_t *pollable, SEXP fds);

processx_file_handle_t processx_c_connection_fileno(
  const processx_connection_t *con);

/* --------------------------------------------------------------------- */
/* Internals                                                             */
/* --------------------------------------------------------------------- */

#ifndef _WIN32
typedef unsigned long DWORD;
#endif

/* Threading in Windows */

#ifdef _WIN32
int processx__start_thread(void);
extern HANDLE processx__iocp_thread;
extern HANDLE processx__thread_start;
extern HANDLE processx__thread_done;

extern fd_set processx__readfds, processx__writefds,
  processx__exceptionfds;
extern SOCKET processx__notify_socket[2];
extern int processx__select;
extern ULONG_PTR processx__key_none;

extern int processx__thread_cmd;
#define PROCESSX__THREAD_CMD_INIT 0
#define PROCESSX__THREAD_CMD_IDLE 1
#define PROCESSX__THREAD_CMD_READFILE 2
#define PROCESSX__THREAD_CMD_GETSTATUS 3
#define PROCESSX__THREAD_CMD_CONNECTPIPE 4

BOOL processx__thread_readfile(processx_connection_t *ccon,
			       LPVOID lpBuffer,
			       DWORD nNumberOfBytesToRead,
			       LPDWORD lpNumberOfBytesRead);
BOOL processx__thread_connectpipe(processx_connection_t *ccon);
BOOL processx__thread_getstatus(LPDWORD lpNumberOfBytes,
				PULONG_PTR lpCompletionKey,
				LPOVERLAPPED *lpOverlapped,
				DWORD dwMilliseconds);
BOOL processx__thread_getstatus_select(LPDWORD lpNumberOfBytes,
				       PULONG_PTR lpCompletionKey,
				       LPOVERLAPPED *lpOverlapped,
				       DWORD dwMilliseconds);
DWORD processx__thread_get_last_error(void);

#endif

/* Free-list of connection in Windows */

typedef struct processx__connection_freelist_s {
  processx_connection_t *ccon;
  struct processx__connection_freelist_s *next;
} processx__connection_freelist_t;

int processx__connection_freelist_add(processx_connection_t *con);
void processx__connection_freelist_remove(processx_connection_t *con);
int processx__connection_schedule_destroy(processx_connection_t *con);

#endif
