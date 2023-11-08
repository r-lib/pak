/* IdnToAscii() requires at least vista to build */
#define _WIN32_WINNT 0x0600
#define WINVER 0x0600
#define IDN_MAX_LENGTH 255

#ifdef _WIN32
#include <Windows.h>

wchar_t * jeroen_convert_UTF8_to_wchar(const char *str_utf8){
  wchar_t *str_w = NULL;

  if(str_utf8) {
    int str_w_len = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS,
                                        str_utf8, -1, NULL, 0);
    if(str_w_len > 0) {
      str_w = malloc(str_w_len * sizeof(wchar_t));
      if(str_w) {
        if(MultiByteToWideChar(CP_UTF8, 0, str_utf8, -1, str_w,
                               str_w_len) == 0) {
          free(str_w);
          return NULL;
        }
      }
    }
  }

  return str_w;
}

char *jeroen_convert_wchar_to_UTF8(const wchar_t *str_w){
  char *str_utf8 = NULL;

  if(str_w) {
    int str_utf8_len = WideCharToMultiByte(CP_UTF8, 0, str_w, -1, NULL,
                                           0, NULL, NULL);
    if(str_utf8_len > 0) {
      str_utf8 = malloc(str_utf8_len * sizeof(wchar_t));
      if(str_utf8) {
        if(WideCharToMultiByte(CP_UTF8, 0, str_w, -1, str_utf8, str_utf8_len,
                               NULL, FALSE) == 0) {
          free(str_utf8);
          return NULL;
        }
      }
    }
  }

  return str_utf8;
}

int jeroen_win32_idn_to_ascii(const char *in, char **out){
  int success = FALSE;
  wchar_t *in_w = jeroen_convert_UTF8_to_wchar(in);
  if(in_w) {
    wchar_t punycode[IDN_MAX_LENGTH];
    int chars = IdnToAscii(0, in_w, -1, punycode, IDN_MAX_LENGTH);
    free(in_w);
    if(chars) {
      *out = jeroen_convert_wchar_to_UTF8(punycode);
      if(*out)
        success = TRUE;
    }
  }

  return success;
}

#else
void placeholder_to_avoid_stupid_warning(void){}
#endif
