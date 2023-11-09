
#include <windows.h>
#include <direct.h>
#include <sys/stat.h>
#include <stdio.h>
#include "zip.h"

/* -------------------------------------------------------------- */

int zip__utf8_to_utf16(const char* s, wchar_t** buffer,
                       size_t *buffer_size) {
  int ws_len, r;

  ws_len = MultiByteToWideChar(
    /* CodePage =       */ CP_UTF8,
    /* dwFlags =        */ 0,
    /* lpMultiByteStr = */ s,
    /* cbMultiByte =    */ -1,
    /* lpWideCharStr =  */ NULL,
    /* cchWideChar =    */ 0);

  if (ws_len <= 0) { return GetLastError(); }

  if (*buffer == NULL) {
    /* Let's allocated something bigger, so no need to grow much */
    *buffer_size = ws_len > 255 ? ws_len : 255;
    *buffer = (wchar_t*) calloc(*buffer_size, sizeof(wchar_t));
  } else if (ws_len > *buffer_size) {
    *buffer_size = ws_len;
    *buffer = (wchar_t*) realloc(*buffer, ws_len * sizeof(wchar_t));
  }
  if (*buffer == NULL) { return ERROR_OUTOFMEMORY; }

  r = MultiByteToWideChar(
    /* CodePage =       */ CP_UTF8,
    /* dwFlags =        */ 0,
    /* lpMultiByteStr = */ s,
    /* cbMultiBytes =   */ -1,
    /* lpWideCharStr =  */ *buffer,
    /* cchWideChar =    */ ws_len);

  if (r != ws_len) { return GetLastError(); }

  return 0;
}

int zip__utf16_to_utf8(const wchar_t *ws, char** buffer, size_t *buffer_size) {

  int slen, r;

  slen = WideCharToMultiByte(CP_UTF8, 0, ws, -1, NULL, 0, NULL, NULL);

  if (slen <= 0) { return GetLastError(); }

  if (*buffer == NULL) {
    /* Let's allocated something bigger, so no need to grow much */
    *buffer_size = slen > 255 ? slen : 255;
    *buffer = (char*) calloc(*buffer_size, sizeof(char));
  } else if (slen > *buffer_size) {
    *buffer_size = slen;
    *buffer = (char*) realloc(*buffer, slen * sizeof(char));
  }
  if (*buffer == NULL) { return ERROR_OUTOFMEMORY; }

  r = WideCharToMultiByte(CP_UTF8, 0, ws, -1, *buffer, slen, NULL, NULL);

  if (r != slen) { return GetLastError(); }

  return 0;
}

FILE *zip_open_utf8(const char *filename, const wchar_t *mode,
                    wchar_t **buffer, size_t *buffer_size) {
  int ret = zip__utf8_to_utf16(filename, buffer, buffer_size);
  if (ret) return NULL;

  FILE *fh = _wfopen(*buffer, mode);
  return fh;
}


int zip_str_file_path(const char *cexdir, const char *key,
                      zip_char_t **buffer, size_t *buffer_size,
                      int cjunkpaths) {

  int need_size, len1, len2, i;
  wchar_t *newbuffer;

  if (cjunkpaths) {
    char *base = strrchr(key, '/');
    if (base) key = base;
  } else if (key[0] == '/') {
    key = key + 1;
  }

  len1 = MultiByteToWideChar(
    /* CodePage =       */ CP_UTF8,
    /* dwFlags =        */ 0,
    /* lpMultiByteStr = */ cexdir,
    /* cbMultiByte =    */ -1,
    /* lpWideCharStr =  */ NULL,
    /* cchWideChar =    */ 0
  );
  len2 = MultiByteToWideChar(
    /* CodePage =       */ CP_UTF8,
    /* dwFlags =        */ 0,
    /* lpMultiByteStr = */ key,
    /* cbMultiByte =    */ -1,
    /* lpWideCharStr =  */ NULL,
    /* cchWideChar =    */ 0
  );
  /* No need to +1 for '\\' because both have a terminating zero. */
  int offset = cjunkpaths ? 0 : 4;
  need_size = len1 + len2 + offset;

  if (*buffer == NULL) {
    *buffer_size = need_size > 256 ? need_size : 256;
    *buffer = calloc(*buffer_size, sizeof(zip_char_t));
    if (!*buffer) return 1;

  } else if (*buffer_size < need_size) {
    newbuffer = realloc((void*) *buffer, need_size * sizeof(zip_char_t));
    if (!newbuffer) return 1;

    *buffer = newbuffer;
    *buffer_size = need_size;
  }

  /* work around path length limitations */
  if (!cjunkpaths) {
    (*buffer)[0] = L'\\';
    (*buffer)[1] = L'\\';
    (*buffer)[2] = L'?';
    (*buffer)[3] = L'\\';
  }

  len1 = MultiByteToWideChar(
    /* CodePage =       */ CP_UTF8,
    /* dwFlags =        */ 0,
    /* lpMultiByteStr = */ cexdir,
    /* cbMultiByte =    */ -1,
    /* lpWideCharStr =  */ (*buffer) + offset,
    /* cchWideChar =    */ len1
  );
  len2 = MultiByteToWideChar(
    /* CodePage =       */ CP_UTF8,
    /* dwFlags =        */ 0,
    /* lpMultiByteStr = */ key,
    /* cbMultiByte =    */ -1,
    /* lpWideCharStr =  */ (*buffer) + len1 + offset,
    /* cchWideChar =    */ len2
  );

  (*buffer)[offset + len1 - 1] = L'\\';

  for (i = offset; i < need_size; i++) {
    if ((*buffer)[i] == L'/') (*buffer)[i] = L'\\';
  }

  return 0;
}

int zip_mkdirp(zip_char_t *path, int complete)  {
  wchar_t *p = path;
  BOOL status;
  DWORD err = 0;

  /* Skip \\?\ if any */
  if (*p == L'\\' && *(p+1) == L'\\' && *(p+2) == L'?' &&
      *(p+3) == L'\\') {
    p += 4;
  }

  /* Skip host part of an UNC path */
  if ((*p == L'\\' && *(p+1) == L'\\') ||
      (*p == L'/'  && *(p+1) == L'/' )) {
    p += 2;
    while (*p && *p != L'/' && *p != L'\\') p++;
  }

  /* Skip drive letter if any. */
  if (*(p+1) == L':' && (*(p+2) == L'/' || (*(p+2)) == L'\\')) {
    p += 3;
  }

  /* Iterate the string */
  for (; *p; p++) {
    if (*p == L'/' || *p == L'\\') {
      *p = L'\0';
      status = CreateDirectoryW(path, NULL);
      *p = L'\\';
      if (!status) {
        err = GetLastError();
        if (err != ERROR_ALREADY_EXISTS) return 1;
      }
    }
  }

  if (complete) {
    p--;
    if (*p == L'/' || *p == L'\\') *p = L'\0';
    status = CreateDirectoryW(path, NULL);
    if (*p == L'\0') *p = L'\\';
    if (!status) {
      err = GetLastError();
      if (err != ERROR_ALREADY_EXISTS) return 1;
    }
  }

  return 0;
}

int zip_file_exists(zip_char_t *filename) {
  DWORD attrib = GetFileAttributesW(filename);
  return attrib != INVALID_FILE_ATTRIBUTES;
}

int zip_set_mtime(const zip_char_t *filename, time_t mtime) {
  SYSTEMTIME st;
  FILETIME modft;
  struct tm *utctm;
  HANDLE hFile;
  time_t ftimei = (time_t) mtime;

  utctm = gmtime(&ftimei);
  if (!utctm) return 1;

  st.wYear         = (WORD) utctm->tm_year + 1900;
  st.wMonth        = (WORD) utctm->tm_mon + 1;
  st.wDayOfWeek    = (WORD) utctm->tm_wday;
  st.wDay          = (WORD) utctm->tm_mday;
  st.wHour         = (WORD) utctm->tm_hour;
  st.wMinute       = (WORD) utctm->tm_min;
  st.wSecond       = (WORD) utctm->tm_sec;
  st.wMilliseconds = (WORD) 1000*(mtime - ftimei);
  if (!SystemTimeToFileTime(&st, &modft)) return 1;

  hFile = CreateFileW(filename, GENERIC_WRITE, 0, NULL, OPEN_EXISTING,
                      FILE_FLAG_BACKUP_SEMANTICS, NULL);

  if (hFile == INVALID_HANDLE_VALUE) return 1;
  int res  = SetFileTime(hFile, NULL, NULL, &modft);
  CloseHandle(hFile);
  return res == 0; /* success is non-zero */
}

int zip_file_size(FILE *fh, mz_uint64 *size) {
  if (_fseeki64(fh, 0, SEEK_END)) return 1;
  *size = _ftelli64(fh);
  if (*size == -1) return 2;
  if (_fseeki64(fh, 0, SEEK_SET)) return 3;
  return 0;
}

FILE* zip_long_wfopen(const wchar_t *filename, const wchar_t *mode) {
  FILE* res = _wfopen(filename, mode);
  return res;
}
