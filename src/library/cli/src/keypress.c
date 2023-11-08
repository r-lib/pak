
#include "keypress.h"

const char *keypress_key_names[KEYPRESS_NAME_SIZE] = {
  "",				/* 0 */

  "enter",			/* 1 */
  "backspace",			/* 2 */
  "left",			/* 3 */
  "right",			/* 4 */
  "up",				/* 5 */
  "down",			/* 6 */
  "insert",			/* 7 */
  "delete",			/* 8 */
  "home",			/* 9 */
  "end",			/* 10 */

  "f1",				/* 11 */
  "f2",				/* 12 */
  "f3",				/* 13 */
  "f4",				/* 14 */
  "f5",				/* 15 */
  "f6",				/* 16 */
  "f7",				/* 17 */
  "f8",				/* 18 */
  "f9",				/* 19 */
  "f10",			/* 20 */
  "f11",			/* 21 */
  "f12",			/* 22 */

  "ctrl-a",			/* 23 */
  "ctrl-b",			/* 24 */
  "ctrl-c",			/* 25 */
  "ctrl-d",			/* 26 */
  "ctrl-e",			/* 27 */
  "ctrl-f",			/* 28 */
  "ctrl-h",			/* 29 */
  "ctrl-k",			/* 30 */
  "ctrl-l",			/* 31 */
  "ctrl-n",			/* 32 */
  "ctrl-p",			/* 33 */
  "ctrl-t",			/* 34 */
  "ctrl-u",			/* 35 */
  "ctrl-w",			/* 36 */

  "escape",			/* 37 */
  "tab",			/* 38 */

  "pageup",			/* 39 */
  "pagedown",			/* 40 */

  "none",			/* 41 */

  "unknown"			/* 42 */
};

keypress_key_t keypress_special(int key) {
  keypress_key_t result;
  result.code = key;
  result.ascii = '0';
  result.utf8[0] = '0';
  return result;
}

keypress_key_t keypress_utf8(const char *buf) {
  keypress_key_t result;
  result.code = KEYPRESS_CHAR;
  result.ascii = '0';
  strncpy(result.utf8, buf, KEYPRESS_UTF8_BUFFER_SIZE);
  result.utf8[KEYPRESS_UTF8_BUFFER_SIZE] = '0';
  return result;
}

SEXP cli_keypress(SEXP s_block) {
  SEXP result = NULL;
  int block = LOGICAL(s_block)[0];
  keypress_key_t key = keypress_read(block);

  if (key.code == KEYPRESS_CHAR) {
    return ScalarString(mkCharCE(key.utf8, CE_UTF8));
  } else {
    return ScalarString(mkCharCE(keypress_key_names[key.code], CE_UTF8));
  }

  return result;
}
