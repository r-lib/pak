
/* Avoid warning about empty compilation unit. */
void keypress_win_dummy(void) { }

#ifdef WIN32

#include "errors.h"
#include "keypress.h"
#include <windows.h>

static HANDLE console_in, console_out;

static int enableRawMode(void) {
  if (!console_in) {
    HANDLE hin, hout;
    hin = GetStdHandle(STD_INPUT_HANDLE);
    if (hin == INVALID_HANDLE_VALUE) {
      R_THROW_SYSTEM_ERROR("Cannot get standard input handle");
    }
    hout = GetStdHandle(STD_OUTPUT_HANDLE);
    if (hout == INVALID_HANDLE_VALUE) {
      R_THROW_SYSTEM_ERROR("Cannot get standard output handle");
    }
    console_in = hin;
    console_out = hout;
  }
  return 0;
}

static int disableRawMode(void) {
  /* Nothing to do yet */
  return 0;
}

keypress_key_t getWinChar(int block) {
  INPUT_RECORD rec;
  DWORD count;
  char buf[2] = { 0, 0 };
  int chr;

  for (;; Sleep(10)) {

    GetNumberOfConsoleInputEvents(console_in, &count);

    if ((count == 0) && (block == NON_BLOCKING)) {
      return keypress_special(KEYPRESS_NONE);
    }

    if (! ReadConsoleInputA(console_in, &rec, 1, &count)) {
      R_THROW_SYSTEM_ERROR("Cannot read from console");
    }
    if (rec.EventType != KEY_EVENT) continue;
    if (! rec.Event.KeyEvent.bKeyDown) continue;
    buf[0] = chr = rec.Event.KeyEvent.uChar.AsciiChar;

    switch (rec.Event.KeyEvent.wVirtualKeyCode) {

    case VK_RETURN: return keypress_special(KEYPRESS_ENTER);
    case VK_BACK:   return keypress_special(KEYPRESS_BACKSPACE);
    case VK_LEFT:   return keypress_special(KEYPRESS_LEFT);
    case VK_RIGHT:  return keypress_special(KEYPRESS_RIGHT);
    case VK_UP:     return keypress_special(KEYPRESS_UP);
    case VK_DOWN:   return keypress_special(KEYPRESS_DOWN);
    case VK_INSERT: return keypress_special(KEYPRESS_INSERT);
    case VK_DELETE: return keypress_special(KEYPRESS_DELETE);
    case VK_HOME:   return keypress_special(KEYPRESS_HOME);
    case VK_END:    return keypress_special(KEYPRESS_END);
    case VK_ESCAPE: return keypress_special(KEYPRESS_ESCAPE);

    case VK_F1:     return keypress_special(KEYPRESS_F1);
    case VK_F2:     return keypress_special(KEYPRESS_F2);
    case VK_F3:     return keypress_special(KEYPRESS_F3);
    case VK_F4:     return keypress_special(KEYPRESS_F4);
    case VK_F5:     return keypress_special(KEYPRESS_F5);
    case VK_F6:     return keypress_special(KEYPRESS_F6);
    case VK_F7:     return keypress_special(KEYPRESS_F7);
    case VK_F8:     return keypress_special(KEYPRESS_F8);
    case VK_F9:     return keypress_special(KEYPRESS_F9);
    case VK_F10:    return keypress_special(KEYPRESS_F10);
    case VK_F11:    return keypress_special(KEYPRESS_F11);
    case VK_F12:    return keypress_special(KEYPRESS_F12);

    default:
      if (rec.Event.KeyEvent.dwControlKeyState &
	  (LEFT_CTRL_PRESSED | RIGHT_CTRL_PRESSED)) {
	switch (chr) {
	case 1: return keypress_special(KEYPRESS_CTRL_A);
	case 2: return keypress_special(KEYPRESS_CTRL_B);
	case 3: return keypress_special(KEYPRESS_CTRL_C);
	case 4: return keypress_special(KEYPRESS_CTRL_D);
	case 5: return keypress_special(KEYPRESS_CTRL_E);
	case 6: return keypress_special(KEYPRESS_CTRL_F);
	case 8: return keypress_special(KEYPRESS_CTRL_H);
	case 9: return keypress_special(KEYPRESS_TAB);
	case 11: return keypress_special(KEYPRESS_CTRL_K);
	case 12: return keypress_special(KEYPRESS_CTRL_L);
	case 14: return keypress_special(KEYPRESS_CTRL_N);
	case 16: return keypress_special(KEYPRESS_CTRL_P);
	case 20: return keypress_special(KEYPRESS_CTRL_T);
	case 21: return keypress_special(KEYPRESS_CTRL_U);
	case 22: return keypress_special(KEYPRESS_CTRL_W);
	}
      } else if (buf[0]) {
	return keypress_utf8(buf);
      }
    }

  }
}

keypress_key_t keypress_read(int block) {

  keypress_key_t res;

  CONSOLE_SCREEN_BUFFER_INFO inf;

  enableRawMode();

  if (! GetConsoleScreenBufferInfo(console_out, &inf)) {
    R_THROW_SYSTEM_ERROR("Cannot query console information");
  }

  res = getWinChar(block);

  disableRawMode();

  return res;
}

#endif
