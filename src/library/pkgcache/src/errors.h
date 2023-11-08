
/* Informative error messages from C in R packages.
 *
 * Features:
 * - templating
 * - include the function name
 * - include the file name and line number of the error
 * - look up (localized) system error messages from errno on Unix, or for
 *   POSIX functions on Windows.
 * - look up (localized) system error messages from GetLastError()
 *   on Windows
 *
 * See the API below.
 */

#ifndef R_THROW_ERROR_H
#define R_THROW_ERROR_H

#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif

#ifdef _WIN32
#include <windows.h>
#else
#include <errno.h>
#endif

#include <Rinternals.h>

/* Throw a generic (non-system) error.
 *
 * ARGUMENTS
 *     ... Error message template, and values to be substituted into the
 *         template. They are passed to `vsnprintf()`.
 *
 * EXAMPLES
 *     If `fun` is a character string, then
 *
 *         R_THROW_ERROR("`%s` must be a function", fun);
 *
 *     will produce an error message like this:
 *
 *         Error in set_mock(...): `old_fun` must be a function
 *         @reassign.c:9 (reassign_function)
 */

#define R_THROW_ERROR(...) \
  r_throw_error(__func__, __FILE__, __LINE__, __VA_ARGS__)

SEXP r_throw_error(const char *func, const char *filename, int line,
                   const char *msg, ...);

#define R_FORMAT_ERROR(...) \
  r_format_error(__func__, __FILE__, __LINE__, __VA_ARGS__)

SEXP r_format_error(const char *func, const char *filename, int line,
                    const char *msg, ...);

#ifdef _WIN32

/* Throw a system error on Windows.
 *
 * DESCRIPTION
 *     `R_THROW_SYSTEM_ERROR()` queries the error code via `GetLastError()`,
 *     and constructs an error message that includes both the error code and
 *     the (localized) error message.
 *
 *     `R_THROW_SYSTEM_ERROR_CODE()` is similar, but you can specify the
 *     error code explicitly. Use this if you already had to query the
 *     error code before deciding to throw an error.
 *
 * ARGUMENTS
 *     errorcode The Windows error code. Sometimes you need to query this
 *         explicitly, because some `GetLastError()` codes are not errors.
 *     ... Error message template, and values to be substituted into the
 *         template. They are passed to `vsnprintf()`.
 *
 * EXAMPLES
 *     This is a way to handle a `CreateFileW()` failure on Windows.
 *     (Some details omitted for brevity.)
 *
 *         HANDLE hnd = CreateFileW(filename, ...);
 *         if (hnd == INVALID_HANDLE_VALUE) {
 *              R_THROW_SYSTEM_ERROR("cannot open file `%ls`", filename);
 *         }
 *
 *     which will create an error message like this:
 *
 *         Error in read_file(...): cannot open file `input.txt`
 *         (system error 5, Access is denied.) @read.c:234 (read_file)
 */

#define R_THROW_SYSTEM_ERROR(...) \
  r_throw_system_error(__func__, __FILE__, __LINE__, (-1), NULL, __VA_ARGS__)
#define R_THROW_SYSTEM_ERROR_CODE(errorcode, ...)                       \
  r_throw_system_error(__func__, __FILE__, __LINE__, (errorcode), NULL, __VA_ARGS__)

#define R_FORMAT_SYSTEM_ERROR(...)                                       \
  r_format_system_error(__func__, __FILE__, __LINE__, (-1), NULL, __VA_ARGS__)
#define R_FORMAT_SYSTEM_ERROR_CODE(errorcode, ...)                       \
  r_format_system_error(__func__, __FILE__, __LINE__, (errorcode), NULL, __VA_ARGS__)


SEXP r_throw_system_error(const char *func, const char *filename, int line,
                          DWORD errorcode, const char *sysmsg,
                          const char *msg, ...);

SEXP r_format_system_error(const char *func, const char *filename, int line,
                           DWORD errorcode, const char *sysmsg,
                           const char *msg, ...);

/* Throw an error for a POSIX system call failure on Windows or in
 * portable code that is shared between Unix and Windows.
 *
 * DESCRIPTION
 *     `R_THROW_POSIX_ERROR()` queries the error code from `errno`, and
 *     constructs and error message and includes both the error code and
 *     the localized error message.
 *
 *     `R_THROW_POSIX_ERROR_CODE()` is similar, but you can pass in the
 *     POSIX error code directly.
 *
 *     Use these functions for POSIX system call failures in Windows.
 *     You can also use them for code that is shared between Unix and
 *     Windows.
 *
 * ARGUMENTS
 *     errorcode The POSIX error code.
 *     ... Error message template, and values to be substituted into the
 *         template. They are passed to `vsnprintf()`.
 *
 * EXAMPLES
 *     Here is a way to handle a `fopen()` failure on Windows or in
 *     portable code:
 *
 *         FILE infile = fopen(filename, "r");
 *         if (infile == NULL) {
 *             R_THROW_POSIX_ERROR("cannot open `%s`", filename);
 *         }
 *
 *     which will create an error message like this:
 *
 *         Error in read_file(...): cannot open file `input.txt`
 *         (system error 13, Permission denied) @read.c:234 (read_file)
 */

#define R_THROW_POSIX_ERROR(...)                                        \
  r_throw_posix_error(__func__, __FILE__, __LINE__, errno, NULL, __VA_ARGS__)
#define R_THROW_POSIX_ERROR_CODE(errorcode, ...)           \
  r_throw_posix_error(__func__, __FILE__, __LINE__, errorcode, NULL, __VA_ARGS__)

#define R_FORMAT_POSIX_ERROR(...)                                        \
  r_format_posix_error(__func__, __FILE__, __LINE__, errno, NULL, __VA_ARGS__)
#define R_FORMAT_POSIX_ERROR_CODE(errorcode, ...)           \
  r_format_posix_error(__func__, __FILE__, __LINE__, errorcode, NULL, __VA_ARGS__)

SEXP r_throw_posix_error(const char *func, const char *filename, int line,
                         int errorcode, const char *sysmsg,
                         const char *msg, ...);

SEXP r_format_posix_error(const char *func, const char *filename, int line,
                          int errorcode, const char *sysmsg,
                          const char *msg, ...);

#else

/* See R_THROW_SYSTEM_ERROR(...) above. On Unix `R_THROW_SYSTEM_ERROR()`
 * is the same as `R_THROW_POSIX_ERROR()`, and `R_THROW_SYSTEM_ERROR_CODE()`
 * is the same as `R_THROW_POSIX_ERROR_CODE(). */

#define R_THROW_SYSTEM_ERROR(...) \
  r_throw_system_error(__func__, __FILE__, __LINE__, errno, NULL, __VA_ARGS__)
#define R_THROW_SYSTEM_ERROR_CODE(errorcode, ...)           \
  r_throw_system_error(__func__, __FILE__, __LINE__, errorcode, NULL, __VA_ARGS__)

#define R_THROW_POSIX_ERROR(...) \
  r_throw_system_error(__func__, __FILE__, __LINE__, errno, NULL, __VA_ARGS__)
#define R_THROW_POSIX_ERROR_CODE(errorcode, ...)            \
  r_throw_system_error(__func__, __FILE__, __LINE__, errorcode, NULL, __VA_ARGS__)

#define R_FORMAT_SYSTEM_ERROR(...) \
  r_format_system_error(__func__, __FILE__, __LINE__, errno, NULL, __VA_ARGS__)
#define R_FORMAT_SYSTEM_ERROR_CODE(errorcode, ...)           \
  r_format_system_error(__func__, __FILE__, __LINE__, errorcode, NULL, __VA_ARGS__)

#define R_FORMAT_POSIX_ERROR(...) \
  r_format_system_error(__func__, __FILE__, __LINE__, errno, NULL, __VA_ARGS__)
#define R_FORMAT_POSIX_ERROR_CODE(errorcode, ...)            \
  r_format_system_error(__func__, __FILE__, __LINE__, errorcode, NULL, __VA_ARGS__)

SEXP r_throw_system_error(const char *func, const char *filename, int line,
                          int errorcode, const char *sysmsg,
                          const char *msg, ...);

SEXP r_format_system_error(const char *func, const char *filename, int line,
                           int errorcode, const char *sysmsg,
                           const char *msg, ...);

#endif

#endif
