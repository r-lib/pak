
#include "../processx.h"

HANDLE processx__connection_iocp = NULL;

HANDLE processx__get_default_iocp(void) {
  if (! processx__connection_iocp) {
    processx__connection_iocp = CreateIoCompletionPort(
    /* FileHandle = */                 INVALID_HANDLE_VALUE,
    /* ExistingCompletionPort = */     NULL,
    /* CompletionKey = */              0,
    /* NumberOfConcurrentThreads =  */ 0);
  }
  return processx__connection_iocp;
}

HANDLE processx__iocp_thread = NULL;
HANDLE processx__thread_start = NULL;
HANDLE processx__thread_done = NULL;
BOOL processx__thread_success;
void *processx__thread_data = NULL;
DWORD processx__thread_last_error = 0;
int processx__thread_cmd = PROCESSX__THREAD_CMD_INIT;

fd_set processx__readfds,
  processx__writefds,
  processx__exceptionfds;
SOCKET processx__notify_socket[2] = { 0, 0 };
int processx__select = 0;

struct processx__thread_readfile_data {
  processx_connection_t *ccon;
  LPVOID lpBuffer;
  DWORD nNumberOfBytesToRead;
  LPDWORD lpNumberOfBytesRead;
} processx__thread_readfile_data;

struct processx__thread_getstatus_data {
  LPDWORD lpNumberOfBytes;
  PULONG_PTR lpCompletionKey;
  LPOVERLAPPED *lpOverlapped;
  DWORD dwMilliseconds;
} processx__thread_getstatus_data;

ULONG_PTR processx__key_none = 1;

DWORD processx_i_thread_readfile(void) {

  processx_connection_t *ccon = processx__thread_readfile_data.ccon;

  if (! ccon->handle.overlapped.hEvent &&
      (ccon->type == PROCESSX_FILE_TYPE_ASYNCFILE ||
       ccon->type == PROCESSX_FILE_TYPE_ASYNCPIPE ||
       ccon->type == PROCESSX_FILE_TYPE_SOCKET)) {
    ccon->handle.overlapped.hEvent = CreateEvent(
      /* lpEventAttributes = */ NULL,
      /* bManualReset = */      FALSE,
      /* bInitialState = */     FALSE,
      /* lpName = */            NULL);

    if (ccon->handle.overlapped.hEvent == NULL) return FALSE;

    HANDLE iocp = processx__get_default_iocp();
    if (!iocp) return FALSE;

    HANDLE res = CreateIoCompletionPort(
      /* FileHandle =  */                ccon->handle.handle,
      /* ExistingCompletionPort = */     iocp,
      /* CompletionKey = */              (ULONG_PTR) ccon,
      /* NumberOfConcurrentThreads = */  0);

    if (!res) return FALSE;
  }

  /* These need to be set to zero for non-file handles */
  if (ccon->type != PROCESSX_FILE_TYPE_ASYNCFILE) {
    ccon->handle.overlapped.Offset = 0;
    ccon->handle.overlapped.OffsetHigh = 0;
  }

  DWORD res = ReadFile(ccon->handle.handle,
		       processx__thread_readfile_data.lpBuffer,
		       processx__thread_readfile_data.nNumberOfBytesToRead,
		       processx__thread_readfile_data.lpNumberOfBytesRead,
		       &ccon->handle.overlapped);
  return res;
}

DWORD processx_i_thread_connectpipe(void) {

  processx_connection_t *ccon = processx__thread_readfile_data.ccon;

  if (! ccon->handle.overlapped.hEvent &&
      (ccon->type == PROCESSX_FILE_TYPE_ASYNCFILE ||
       ccon->type == PROCESSX_FILE_TYPE_ASYNCPIPE ||
       ccon->type == PROCESSX_FILE_TYPE_SOCKET)) {
    ccon->handle.overlapped.hEvent = CreateEvent(
      /* lpEventAttributes = */ NULL,
      /* bManualReset = */      FALSE,
      /* bInitialState = */     FALSE,
      /* lpName = */            NULL);

    if (ccon->handle.overlapped.hEvent == NULL) return FALSE;

    HANDLE iocp = processx__get_default_iocp();
    if (!iocp) return FALSE;

    HANDLE res = CreateIoCompletionPort(
      /* FileHandle =  */                ccon->handle.handle,
      /* ExistingCompletionPort = */     iocp,
      /* CompletionKey = */              (ULONG_PTR) ccon,
      /* NumberOfConcurrentThreads = */  0);

    if (!res) return FALSE;
  }

  /* These need to be set to zero for non-file handles */
  if (ccon->type != PROCESSX_FILE_TYPE_ASYNCFILE) {
    ccon->handle.overlapped.Offset = 0;
    ccon->handle.overlapped.OffsetHigh = 0;
  }

  DWORD res = ConnectNamedPipe(ccon->handle.handle,
			       &ccon->handle.overlapped);
  return res;
}

DWORD processx_i_thread_getstatus(void) {
  static const char *ok_buf = "OK";
  HANDLE iocp = processx__get_default_iocp();
  if (!iocp) return FALSE;

  DWORD res = GetQueuedCompletionStatus(
    iocp,
    processx__thread_getstatus_data.lpNumberOfBytes,
    processx__thread_getstatus_data.lpCompletionKey,
    processx__thread_getstatus_data.lpOverlapped,
    processx__thread_getstatus_data.dwMilliseconds);

  if (processx__select) {
    /* TODO: error */
    send(processx__notify_socket[1], ok_buf, 2, 0);
  }

  return res;
}

DWORD processx__thread_callback(void *data) {
  while (1) {
    WaitForSingleObject(processx__thread_start, INFINITE);

    processx__thread_success = TRUE;
    processx__thread_last_error = 0;

    switch (processx__thread_cmd) {
    case PROCESSX__THREAD_CMD_INIT:
    case PROCESSX__THREAD_CMD_IDLE:
      break;

    case PROCESSX__THREAD_CMD_READFILE:
      processx__thread_success = processx_i_thread_readfile();
      break;

    case PROCESSX__THREAD_CMD_GETSTATUS:
      processx__thread_success = processx_i_thread_getstatus();
      break;

    case PROCESSX__THREAD_CMD_CONNECTPIPE:
      processx__thread_success = processx_i_thread_connectpipe();
      break;

    default:
      /* ???? */
      processx__thread_success = FALSE;
      break;
    }

    if (!processx__thread_success) {
      processx__thread_last_error = GetLastError();
    }

    processx__thread_cmd = PROCESSX__THREAD_CMD_IDLE;
    SetEvent(processx__thread_done);
  }
  return 0;
}

int processx__start_thread(void) {
  if (processx__iocp_thread != NULL) return 0;

  DWORD threadid;

  processx__thread_start = CreateEventA(NULL, FALSE, FALSE, NULL);
  processx__thread_done  = CreateEventA(NULL, FALSE, FALSE, NULL);

  if (processx__thread_start == NULL || processx__thread_done == NULL) {
    if (processx__thread_start) CloseHandle(processx__thread_start);
    if (processx__thread_done ) CloseHandle(processx__thread_done);
    processx__thread_start = processx__thread_done = NULL;
    R_THROW_SYSTEM_ERROR("Cannot create I/O events");
  }

  processx__thread_cmd = PROCESSX__THREAD_CMD_INIT;

  processx__iocp_thread = CreateThread(
    /* lpThreadAttributes = */ NULL,
    /* dwStackSize = */        0,
    /* lpStartAddress = */
      (LPTHREAD_START_ROUTINE) processx__thread_callback,
    /* lpParameter = */        0,
    /* dwCreationFlags = */    0,
    /* lpThreadId = */         &threadid);

  if (processx__iocp_thread == NULL) {
    CloseHandle(processx__thread_start);
    CloseHandle(processx__thread_done);
    processx__thread_start = processx__thread_done = NULL;
    R_THROW_SYSTEM_ERROR("Cannot start I/O thread");
  }

  /* Wait for thread to be ready */
  SetEvent(processx__thread_start);
  WaitForSingleObject(processx__thread_done, INFINITE);

  return 0;
}

/* ReadFile, but in the bg thread */

BOOL processx__thread_readfile(processx_connection_t *ccon,
			       LPVOID lpBuffer,
			       DWORD nNumberOfBytesToRead,
			       LPDWORD lpNumberOfBytesRead) {

  processx__start_thread();
  processx__thread_cmd = PROCESSX__THREAD_CMD_READFILE;

  processx__thread_readfile_data.ccon = ccon;
  processx__thread_readfile_data.lpBuffer = lpBuffer;
  processx__thread_readfile_data.nNumberOfBytesToRead = nNumberOfBytesToRead;
  processx__thread_readfile_data.lpNumberOfBytesRead = lpNumberOfBytesRead;

  SetEvent(processx__thread_start);
  WaitForSingleObject(processx__thread_done, INFINITE);

  return processx__thread_success;
}

BOOL processx__thread_connectpipe(processx_connection_t *ccon) {
  processx__start_thread();
  processx__thread_cmd = PROCESSX__THREAD_CMD_CONNECTPIPE;

  processx__thread_readfile_data.ccon = ccon;

  SetEvent(processx__thread_start);
  WaitForSingleObject(processx__thread_done, INFINITE);

  return processx__thread_success;
}

/* GetQueuedCompletionStatus but in the bg thread */

BOOL  processx__thread_getstatus(LPDWORD lpNumberOfBytes,
				 PULONG_PTR lpCompletionKey,
				 LPOVERLAPPED *lpOverlapped,
				 DWORD dwMilliseconds) {

  processx__start_thread();
  processx__thread_cmd = PROCESSX__THREAD_CMD_GETSTATUS;

  processx__thread_getstatus_data.lpNumberOfBytes = lpNumberOfBytes;
  processx__thread_getstatus_data.lpCompletionKey = lpCompletionKey;
  processx__thread_getstatus_data.lpOverlapped = lpOverlapped;
  processx__thread_getstatus_data.dwMilliseconds = dwMilliseconds;

  SetEvent(processx__thread_start);
  WaitForSingleObject(processx__thread_done, INFINITE);

  return processx__thread_success;
}

BOOL processx__thread_getstatus_select(LPDWORD lpNumberOfBytes,
				       PULONG_PTR lpCompletionKey,
				       LPOVERLAPPED *lpOverlapped,
				       DWORD dwMilliseconds) {
  TIMEVAL timeout;
  char buf[10];
  HANDLE iocp = processx__get_default_iocp();

  processx__start_thread();

  timeout.tv_sec = dwMilliseconds / 1000;
  timeout.tv_usec = dwMilliseconds % 1000 * 1000;

  processx__thread_cmd = PROCESSX__THREAD_CMD_GETSTATUS;

  processx__select = 1;
  processx__thread_getstatus_data.lpNumberOfBytes = lpNumberOfBytes;
  processx__thread_getstatus_data.lpCompletionKey = lpCompletionKey;
  processx__thread_getstatus_data.lpOverlapped = lpOverlapped;
  processx__thread_getstatus_data.dwMilliseconds = dwMilliseconds;

  SetEvent(processx__thread_start);
  select(/* (ignored) */ 0, &processx__readfds, &processx__writefds,
    &processx__exceptionfds, &timeout);
  if (FD_ISSET(processx__notify_socket[0], &processx__readfds)) {
    /* TODO: error */
    recv(processx__notify_socket[0], buf, 10, 0);
  } else {
    /* Wake up the IO thread. */
    PostQueuedCompletionStatus(iocp, 0, processx__key_none, 0);
  }

  /* This waits until the IO thread is done */
  WaitForSingleObject(processx__thread_done, INFINITE);

  return processx__thread_success;
}

DWORD processx__thread_get_last_error(void) {
  return processx__thread_last_error;
}
