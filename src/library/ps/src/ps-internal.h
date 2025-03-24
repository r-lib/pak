
#ifndef R_PS_INTERNAL_H
#define R_PS_INTERNAL_H

#include "config.h"
#include "ps.h"

#define PROCESSX_INTERRUPT_INTERVAL 200

#ifdef PS__MACOS

#include <signal.h>

typedef struct {
  pid_t pid;
  double create_time;
  int gone;
} ps_handle_t;

#endif

#ifdef PS__LINUX

#include <signal.h>
#include <unistd.h>
#include <sys/types.h>

typedef struct {
  pid_t pid;
  double create_time;
  int gone;
} ps_handle_t;

#endif

#ifdef PS__WINDOWS

#include <windows.h>

typedef struct {
  DWORD  pid;
  double create_time;
  int gone;
  FILETIME wtime;
} ps_handle_t;

#endif

#ifndef PS__MACOS
#ifndef PS__LINUX
#ifndef PS__WINDOWS

typedef struct {
  int pid;
  double create_time;
} ps_handle_t;

#endif
#endif
#endif

/* Internal utilities */

SEXP psll__is_running(ps_handle_t *handle);

SEXP ps__get_pw_uid(SEXP r_uid);
SEXP ps__define_signals(void);
SEXP ps__define_errno(void);
SEXP ps__define_socket_address_families(void);
SEXP ps__define_socket_types(void);

#define PS_MAYBE 0
#define PS_YEAH 1
#define PS_NOPE 2

extern int ps_pidfd_open_support;

/* Errors */

extern SEXP ps__last_error;

void ps__protect_free_finalizer(SEXP ptr);

#define PROTECT_PTR(ptr) do {						\
    SEXP x = PROTECT(R_MakeExternalPtr(ptr, R_NilValue, R_NilValue));	\
    R_RegisterCFinalizerEx(x, ps__protect_free_finalizer, 1);		\
  } while (0)

void *ps__set_error(const char *msg, ...);
void *ps__set_error_from_errno(void);
SEXP ps__throw_error(void);

void *ps__access_denied(const char *msg);
void *ps__access_denied_pid(long pid, const char *msg);
void *ps__no_such_process(long pid, const char *name);
void *ps__zombie_process(long pid);
void *ps__no_memory(const char *msg);
void *ps__not_implemented(const char *what);
void ps__check_for_zombie(ps_handle_t *handle, int err);

void *ps__set_error_from_windows_error(long err);

/* Build SEXP values */

SEXP ps__build_string(const char *str, ...);
SEXP ps__build_list(const char *template, ...);
SEXP ps__build_named_list(const  char *template, ...);

/* String conversions */

SEXP ps__str_to_utf8(const char *str);
SEXP ps__str_to_utf8_size(const char *str, size_t size);

#ifdef PS__WINDOWS
SEXP ps__utf16_to_rawsxp(const WCHAR* ws, int size);
SEXP ps__utf16_to_charsxp(const WCHAR* ws, int size);
SEXP ps__utf16_to_strsxp(const WCHAR* ws, int size);
int ps__utf8_to_utf16(const char* s, WCHAR** ws_ptr);
#endif

#endif
