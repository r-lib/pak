
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../zip.h"

#ifdef _WIN32
#define CHAR wchar_t
#define MAIN wmain
#define CFMT "%ls"
#else
#define CHAR char
#define MAIN main
#define CFMT "%s"
#endif

#define ZERROR(x) while (1) { retval = (x); goto cleanup; }

static void cmd_zip_error_handler(const char *reason, const char *file,
                                  int line, int zip_errno, int eno) {
  fprintf(stderr, "zip error: `%s` in file `%s:%i`\n", reason, file, line);
  if (eno < 0) {
    eno = - eno;
  } else if (eno == 0) {
    eno = 1;
  }
  exit(eno);
}

static int hex_nibble(unsigned int c) {
  if (c >= '0' && c <= '9') return (int)(c - '0');
  if (c >= 'a' && c <= 'f') return (int)(c - 'a') + 10;
  if (c >= 'A' && c <= 'F') return (int)(c - 'A') + 10;
  return -1;
}

/* Decode a hex string (ASCII or wide-char with ASCII-range values) into bytes.
   Returns decoded length on success, -1 on error. Caller frees *out. */
static int decode_hex_password(const CHAR *hex, unsigned char **out) {
  size_t hlen = 0;
  const CHAR *p = hex;
  while (*p) { hlen++; p++; }
  if (hlen % 2 != 0) return -1;
  size_t outlen = hlen / 2;
  *out = (unsigned char *) malloc(outlen + 1);
  if (!*out) return -1;
  for (size_t i = 0; i < outlen; i++) {
    int hi = hex_nibble((unsigned int) hex[2 * i]);
    int lo = hex_nibble((unsigned int) hex[2 * i + 1]);
    if (hi < 0 || lo < 0) { free(*out); return -1; }
    (*out)[i] = (unsigned char)((hi << 4) | lo);
  }
  return (int) outlen;
}

int MAIN(int argc, CHAR* argv[]) {
  int retval = 0;
  if (argc == 2) {
#ifdef _WIN32
    if (wcscmp(argv[1], L"--test") == 0) return 0;
#else
    if (strcmp(argv[1], "--test") == 0) return 0;
#endif
  }
  if (argc != 3 && argc != 4) {
    fprintf(stderr, "Usage: " CFMT " zip-file target-dir [hex-password]\n", argv[0]);
    return 1;
  }

  unsigned char *cpassword = NULL;
  int cpassword_len = 0;

  if (argc == 4) {
    cpassword_len = decode_hex_password(argv[3], &cpassword);
    if (cpassword_len < 0) {
      fprintf(stderr, "Invalid hex password\n");
      return 1;
    }
  }

  zip_set_error_handler(cmd_zip_error_handler);

#ifdef _WIN32
  char *zipfile = 0;
  size_t zipfile_len = 0;
  char *exdir = 0;
  size_t exdir_len = 0;
  if (zip__utf16_to_utf8(argv[1], &zipfile, &zipfile_len)) ZERROR(2);
  if (zip__utf16_to_utf8(argv[2], &exdir, &exdir_len)) ZERROR(3);

#else
  char *zipfile = argv[1];
  char *exdir = argv[2];
#endif

  zip_unzip(zipfile, /* cfiles= */ 0, /* num_files= */ 0,
	    /* coverwrite= */ 1, /* cjunkpaths= */ 0, /* exdir= */ exdir,
	    /* decode_fn= */ NULL, /* decode_data= */ NULL,
	    /* entry_fn= */ NULL, /* entry_data= */ NULL,
	    cpassword, (size_t) cpassword_len);

#ifdef _WIN32
cleanup:
#endif

  if (retval != 0) {
    fprintf(stderr, "Failed to extract zip archive " CFMT, argv[1]);
  }

  return retval;
}
