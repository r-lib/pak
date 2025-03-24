
#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif

#include <string.h>
#include <errno.h>
#include <ctype.h>
#include <stdlib.h>
#include <unistd.h>

#include "ps-internal.h"

#ifdef PS__WINDOWS
#include <windows.h>
#endif

char ps__last_error_string[1024];
SEXP ps__last_error;

/* TODO: these should throw real error objects */

void *ps__vset_error_impl(const char *class, int system_errno,
			 long pid, const char *msg, va_list args) {
  const char *ps_error = "ps_error", *error = "error", *condition = "condition";
  SEXP rclass;

  vsnprintf(ps__last_error_string,
	    sizeof(ps__last_error_string) - 1, msg, args);

  SET_VECTOR_ELT(ps__last_error, 0, mkString(ps__last_error_string));
  if (class) {
    PROTECT(rclass = ps__build_string(class, ps_error, error, condition, NULL));
  } else {
    PROTECT(rclass = ps__build_string(ps_error, error, condition, NULL));
  }
  SET_VECTOR_ELT(ps__last_error, 1, rclass);
  UNPROTECT(1);
  SET_VECTOR_ELT(ps__last_error, 2, ScalarInteger(system_errno));
  SET_VECTOR_ELT(ps__last_error, 3, ScalarInteger((int)  pid));
  return NULL;
}

void *ps__set_error_impl(const char *class, int system_errno,
			 long pid, const char *msg, ...) {
  va_list args;

  va_start(args, msg);
  void *ret = ps__vset_error_impl(class, system_errno, pid, msg, args);
  va_end(args);
  return ret;
}

void *ps__set_error(const char *msg, ...) {
  va_list args;

  va_start(args, msg);
  ps__vset_error_impl(0, 0, NA_INTEGER, msg, args);
  va_end(args);

  return NULL;
}

void *ps__no_such_process(long pid, const char *name) {
  const char *class = "no_such_process";
  return ps__set_error_impl(
    class, 0, pid, "No such process, pid %ld, %s", pid,
    name ? name : "???");
}

void *ps__access_denied(const char *msg) {
  return ps__set_error_impl("access_denied", 0, NA_INTEGER,
			    msg && strlen(msg) ? msg : "Permission denied");
}

void *ps__access_denied_pid(long pid, const char *msg) {
  return ps__set_error_impl("access_denied", 0, pid,
			    msg && strlen(msg) ? msg : "Permission denied");
}

void *ps__zombie_process(long pid) {
  return ps__set_error_impl("zombie_process", 0, pid,
			    "Process is a zombie, pid %ld", pid);
}

void *ps__not_implemented(const char *what) {
  return ps__set_error_impl("not_implemented", 0, NA_INTEGER,
			    "Not implemented on this platform: `%s`", what);
}

void *ps__no_memory(const char *msg) {
  return ps__set_error_impl("no_memory",
#ifdef PS__WINDOWS
			    ERROR_NOT_ENOUGH_MEMORY,
#else
			    ENOMEM,
#endif
			    NA_INTEGER,
			    msg && strlen(msg) ? msg : "Out of memory");
}

void *ps__set_error_from_errno(void) {
  if (errno) {
    return ps__set_error_impl("os_error", errno, NA_INTEGER, "%s",
			      strerror(errno));
  } else {
    return ps__set_error_impl(0, errno, NA_INTEGER, "run time error");
  }
}

#ifdef PS__WINDOWS
void *ps__set_error_from_windows_error(long err) {
  /* TODO: get the actual message */
  if (!err) err = GetLastError();
  return ps__set_error_impl("os_error", err, NA_INTEGER,
			    "System error: %i", err);
}
#endif

SEXP ps__throw_error(void) {
  SEXP stopfun, call, out;

  Rf_setAttrib(ps__last_error, R_ClassSymbol, VECTOR_ELT(ps__last_error, 1));
  PROTECT(stopfun = Rf_findFun(Rf_install("stop"), R_BaseEnv));
  PROTECT(call = Rf_lang2(stopfun, ps__last_error));
  PROTECT(out = Rf_eval(call, R_GlobalEnv));

  UNPROTECT(3);
  return out;
}

void ps__protect_free_finalizer(SEXP ptr) {
  void *vptr = R_ExternalPtrAddr(ptr);
  if (!vptr) return;
  free(vptr);
}

SEXP ps__str_to_utf8(const char *str) {
  /* TODO: really convert */
  return mkString(str);
}

SEXP ps__str_to_utf8_size(const char *str, size_t size) {
  /* TODO: really convert */
  return ScalarString(Rf_mkCharLen(str, (int) size));
}

#ifdef PS__WINDOWS

int ps__utf8_to_utf16(const char* s, WCHAR** ws_ptr) {
  int ws_len, r;
  WCHAR* ws;

  ws_len = MultiByteToWideChar(
    /* CodePage =       */ CP_UTF8,
    /* dwFlags =        */ 0,
    /* lpMultiByteStr = */ s,
    /* cbMultiByte =    */ -1,
    /* lpWideCharStr =  */ NULL,
    /* cchWideChar =    */ 0);

  if (ws_len <= 0) { return GetLastError(); }

  ws = (WCHAR*) R_alloc(ws_len,  sizeof(WCHAR));
  if (ws == NULL) { return ERROR_OUTOFMEMORY; }

  r = MultiByteToWideChar(
    /* CodePage =       */ CP_UTF8,
    /* dwFlags =        */ 0,
    /* lpMultiByteStr = */ s,
    /* cbMultiBytes =   */ -1,
    /* lpWideCharStr =  */ ws,
    /* cchWideChar =    */ ws_len);

  if (r != ws_len) {
    error("processx error interpreting UTF8 command or arguments");
  }

  *ws_ptr = ws;
  return 0;
}

SEXP ps__utf16_to_rawsxp(const WCHAR* ws, int size) {
  int s_len, r;
  SEXP s;

  s_len = WideCharToMultiByte(
    /* CodePage =           */ CP_UTF8,
    /* dwFlags =            */ 0,
    /* lpWideCharStr =      */ ws,
    /* cchWideChar =        */ size,
    /* lpMultiByteStr =     */ NULL,
    /* cbMultiByte =        */ 0,
    /* lpDefaultChar =      */ NULL,
    /* lpUsedDefaultChar  = */ NULL);

  if (s_len <= 0) {
    error("error converting wide chars to UTF-8");
  }

  PROTECT(s = allocVector(RAWSXP, s_len));

  r = WideCharToMultiByte(
    /* CodePage =           */ CP_UTF8,
    /* dwFlags =            */ 0,
    /* lpWideCharStr =      */ ws,
    /* cchWideChar =        */ size,
    /* lpMultiByteStr =     */ (char*) RAW(s),
    /* cbMultiByte =        */ s_len,
    /* lpDefaultChar =      */ NULL,
    /* lpUsedDefaultChar  = */ NULL);

  if (r != s_len) {
    error("error converting wide chars to UTF-8");
  }

  UNPROTECT(1);
  return s;
}

SEXP ps__utf16_to_strsxp(const WCHAR* ws, int size) {
  SEXP r, s;
  int r_len, s_len, idx, notr = 0;
  char *ptr, *end, *prev;

  PROTECT(r = ps__utf16_to_rawsxp(ws, size));

  r_len = LENGTH(r);
  ptr = (char*) RAW(r);
  end = ptr + r_len;
  s_len = 0;
  while (ptr < end) {
    if (!*ptr) s_len++;
    ptr++;
  }

  /* If ther is no \0 at the end */
  if (r_len > 0 && *(end - 1) !=  '\0') notr = 1;

  PROTECT(s = allocVector(STRSXP, s_len + notr));

  prev = ptr = (char*) RAW(r);
  idx = 0;
  while (ptr < end) {
    while (ptr < end && *ptr) ptr++;
    SET_STRING_ELT(s, idx++, mkCharLen(prev, ptr - prev));
    prev = ++ptr;
  }

  if (notr) {
    SET_STRING_ELT(s, idx++, mkCharLen(prev, end - prev));
  }

  UNPROTECT(2);
  return s;
}

SEXP ps__utf16_to_charsxp(const WCHAR* ws, int size) {
  SEXP r, s;

  PROTECT(r = ps__utf16_to_rawsxp(ws, size));
  PROTECT(s = mkCharLen((char*) RAW(r), LENGTH(r) - 1));
  UNPROTECT(2);
  return s;
}

#endif

static size_t ps__build_template_length(const char *template) {
  size_t len = 0;
  size_t n = strlen(template);
  size_t i;

  for (i = 0; i < n; i++) {
    len += isalpha(template[i]) != 0;
  }

  return len;
}

SEXP ps__build_string(const char *str, ...) {
  va_list args;
  size_t len = 1;
  SEXP res;
  char *s;

  /* Length 0 character */
  if (!str) return(allocVector(STRSXP, 0));

  /* Count the length first */
  va_start(args, str);
  while (va_arg(args, char*)) len++;
  va_end(args);

  PROTECT(res = allocVector(STRSXP, len));
  SET_STRING_ELT(res, 0, mkChar(str));
  len = 1;
  va_start(args, str);
  while ((s = va_arg(args, char*))) {
    SET_STRING_ELT(res, len++, mkChar(s));
  }
  va_end(args);

  UNPROTECT(1);
  return res;
}

static SEXP ps__build_list_impl(const char *template, int named,
				va_list args) {
  size_t slen = strlen(template);
  size_t len = ps__build_template_length(template);
  SEXP res = PROTECT(allocVector(VECSXP, len));
  SEXP names = named ? PROTECT(allocVector(STRSXP, len)) : R_NilValue;
  int ptr = 0, lptr = 0;

  char *tmp1;
  size_t tmp2;
  char tmp3;

  while (ptr < slen) {
    if (named) {
      SET_STRING_ELT(names, lptr, mkChar(va_arg(args, const char*)));
    }

    switch(template[ptr]) {

    case 's':
    case 'z':
    case 'U':
      tmp1 = va_arg(args, char*);
      SET_VECTOR_ELT(res, lptr, tmp1 ? mkString(tmp1) : R_NilValue);
      break;

    case 'y':
      tmp1 = va_arg(args, char*);
      tmp2 = strlen(tmp1);
      SET_VECTOR_ELT(res, lptr, allocVector(RAWSXP, tmp2));
      memcpy(RAW(VECTOR_ELT(res, lptr)), tmp1, tmp2);
      break;
    case 'u':
      error("'u' is not implemented yet");
      break;

    case 'i':
    case 'b':
    case 'h':
    case 'B':
    case 'H':
      SET_VECTOR_ELT(res, lptr, ScalarInteger(va_arg(args, int)));
      break;

    case 'l':
      SET_VECTOR_ELT(res, lptr, ScalarReal(va_arg(args, long int)));
      break;

    case 'I':
      SET_VECTOR_ELT(res, lptr, ScalarReal(va_arg(args, unsigned int)));
      break;

    case 'k':
      SET_VECTOR_ELT(res, lptr, ScalarReal(va_arg(args, unsigned long)));
      break;

    case 'L':
      SET_VECTOR_ELT(res, lptr, ScalarReal(va_arg(args, long long)));
      break;

    case 'K':
      SET_VECTOR_ELT(res, lptr, ScalarReal(va_arg(args, unsigned long long)));
      break;

    case 'n':
      SET_VECTOR_ELT(res, lptr, ScalarReal(va_arg(args, size_t)));
      break;

    case 'c':
      tmp3 = (char) va_arg(args, int);
      SET_VECTOR_ELT(res, lptr, ScalarRaw(tmp3));
      break;

    case 'C':
      tmp3 = (char) va_arg(args, int);
      SET_VECTOR_ELT(res, lptr, ScalarString(mkCharLen(&tmp3, 1)));
      break;

    case 'd':
    case 'f':
      SET_VECTOR_ELT(res, lptr, ScalarReal(va_arg(args, double)));
      break;

    case 'D':
      error("'D' is not implemented yet");
      break;

    case 'S':
    case 'N':
    case 'O':
      SET_VECTOR_ELT(res, lptr, (SEXP) va_arg(args, void*));
      break;

    default:
      error("Unknown conversion key: `%c`", template[ptr]);
    }
    ptr++;
    lptr++;
  }

  if (named) {
    setAttrib(res, R_NamesSymbol, names);
    UNPROTECT(1);
  }

  UNPROTECT(1);
  return res;
}

SEXP ps__build_list(const char *template, ...) {
  va_list args;
  SEXP res;
  va_start(args, template);
  res = PROTECT(ps__build_list_impl(template, 0, args));
  va_end(args);
  UNPROTECT(1);
  return res;
}

SEXP ps__build_named_list(const char *template, ...) {
  va_list args;
  SEXP res;
  va_start(args, template);
  res = PROTECT(ps__build_list_impl(template, 1, args));
  va_end(args);
  UNPROTECT(1);
  return res;
}
