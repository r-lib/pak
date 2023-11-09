
// nocov start

#include "errors.h"

#ifdef WIN32

#include <windows.h>
#include <io.h>
#include <fcntl.h>

static int utf8_to_utf16(const char* s, WCHAR** ws_ptr) {
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

  /* Jumps on error */
  ws = (WCHAR*) R_alloc(ws_len,  sizeof(WCHAR));

  r = MultiByteToWideChar(
    /* CodePage =       */ CP_UTF8,
    /* dwFlags =        */ 0,
    /* lpMultiByteStr = */ s,
    /* cbMultiBytes =   */ -1,
    /* lpWideCharStr =  */ ws,
    /* cchWideChar =    */ ws_len);

  if (r != ws_len) {
    R_THROW_ERROR("error converting UTF-8 file name to UTF-16");
  }

  *ws_ptr = ws;
  return 0;
}

int open_file(const char *path, int oflag) {
  WCHAR *wpath;
  int ret = utf8_to_utf16(path, &wpath);
  if (ret) {
    R_THROW_SYSTEM_ERROR_CODE(
      ret,
      "Failed to open `%s`, cannot convert path to UTF-16",
      path
    );
  }
  return _wopen(wpath, oflag | _O_BINARY);
}

FILE *fopen_file(const char *filename, const char *mode) {
  WCHAR *wfilename, *wmode;
  int ret = utf8_to_utf16(filename, &wfilename);
  if (ret) {
    R_THROW_SYSTEM_ERROR_CODE(
      ret,
      "Failed to open `%s`, cannot convert filename to UTF-16",
      filename
    );
  }
  ret = utf8_to_utf16(mode, &wmode);
  if (ret) {
    R_THROW_SYSTEM_ERROR_CODE(
      ret,
      "Failed to open `%s`, cannot convert mode `%s` to UTF-16",
      filename,
      mode
    );
  }

  return _wfopen(wfilename, wmode);
}

#else

#include <fcntl.h>

int open_file(const char *path, int oflag) {
  return open(path, oflag);
}

FILE *fopen_file(const char *filename, const char *mode) {
  return fopen(filename, mode);
}

#endif

// nocov end
