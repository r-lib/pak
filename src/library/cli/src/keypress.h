
#ifndef KEYPRESS_H
#define KEYPRESS_H

#include <R.h>
#include <Rinternals.h>

#define BLOCKING           1
#define NON_BLOCKING       0

#define KEYPRESS_CHAR      0

#define KEYPRESS_ENTER     1
#define KEYPRESS_BACKSPACE 2
#define KEYPRESS_LEFT      3
#define KEYPRESS_RIGHT     4
#define KEYPRESS_UP        5
#define KEYPRESS_DOWN      6
#define KEYPRESS_INSERT    7
#define KEYPRESS_DELETE    8
#define KEYPRESS_HOME      9
#define KEYPRESS_END      10

#define KEYPRESS_F1       11
#define KEYPRESS_F2       12
#define KEYPRESS_F3       13
#define KEYPRESS_F4       14
#define KEYPRESS_F5       15
#define KEYPRESS_F6       16
#define KEYPRESS_F7       17
#define KEYPRESS_F8       18
#define KEYPRESS_F9       19
#define KEYPRESS_F10      20
#define KEYPRESS_F11      21
#define KEYPRESS_F12      22

#define KEYPRESS_CTRL_A   23
#define KEYPRESS_CTRL_B   24
#define KEYPRESS_CTRL_C   25
#define KEYPRESS_CTRL_D   26
#define KEYPRESS_CTRL_E   27
#define KEYPRESS_CTRL_F   28
#define KEYPRESS_CTRL_H   29
#define KEYPRESS_CTRL_K   30
#define KEYPRESS_CTRL_L   31
#define KEYPRESS_CTRL_N   32
#define KEYPRESS_CTRL_P   33
#define KEYPRESS_CTRL_T   34
#define KEYPRESS_CTRL_U   35
#define KEYPRESS_CTRL_W   36

#define KEYPRESS_ESCAPE   37
#define KEYPRESS_TAB      38

#define KEYPRESS_PAGEUP   39
#define KEYPRESS_PAGEDOWN 40

/* Nothing read, for non-blocking reads */
#define KEYPRESS_NONE     41

/* Something, e.g. escape seq, but don't know what exactly */
#define KEYPRESS_UNKNOWN  42

#define KEYPRESS_NAME_SIZE 43

/* The longest UTF8 character in bytes */
#define KEYPRESS_UTF8_BUFFER_SIZE 4

typedef struct {
  int code;
  char ascii;
  char utf8[KEYPRESS_UTF8_BUFFER_SIZE + 1];
} keypress_key_t;

keypress_key_t keypress_read(int block);

keypress_key_t keypress_special(int key);
keypress_key_t keypress_utf8(const char *buf);

SEXP cli_keypress(SEXP s_block);

extern const char *keypress_key_names[KEYPRESS_NAME_SIZE];

#endif
