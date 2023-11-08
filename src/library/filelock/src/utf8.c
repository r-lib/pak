
#include <R.h>
#include <windows.h>

#include "filelock.h"

void filelock__error(const char *str, DWORD errorcode);

int filelock__utf8_to_utf16_alloc(const char* s, WCHAR** ws_ptr) {
  int ws_len, r;
  WCHAR* ws;

  ws_len = MultiByteToWideChar(
    /* CodePage =       */ CP_UTF8,
    /* dwFlags =        */ 0,
    /* lpMultiByteStr = */ s,
    /* cbMultiByte =    */ -1,
    /* lpWideCharStr =  */ NULL,
    /* cchWideChar =    */ 0);

  if (ws_len <= 0) {
    filelock__error("Cannot convert UTF8 file name to wide chararacter",
		    GetLastError());
  }

  ws = (WCHAR*) R_alloc(ws_len, sizeof(WCHAR));

  r = MultiByteToWideChar(
    /* CodePage =       */ CP_UTF8,
    /* dwFlags =        */ 0,
    /* lpMultiByteStr = */ s,
    /* cbMultiBytes =   */ -1,
    /* lpWideCharStr =  */ ws,
    /* cchWideChar =    */ ws_len);

  if (r != ws_len) {
    error("filelock error interpreting UTF8 filename");
  }

  *ws_ptr = ws;
  return 0;
}
