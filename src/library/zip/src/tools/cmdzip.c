
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

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

void cmd_zip_error_handler(const char *reason, const char *file,
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

static int parse_int_arg(const CHAR *s) {
  int val = 0;
  while (*s >= '0' && *s <= '9') {
    val = val * 10 + (int)(*s - '0');
    s++;
  }
  return (*s == '\0') ? val : -1;
}

int MAIN(int argc, CHAR* argv[]) {
  int i, num_files = 0;
  int ckeysbytes, cfilesbytes;
  char *keysbuffer = 0, *filesbuffer = 0;
  const char **ckeys = 0, **cfiles = 0;
  int *cdirs = 0;
  double *ctimes = 0;
  int fd = -1;
  int retval = 0;
  char *ptr;

  if (argc != 3 && argc != 5) {
    fprintf(stderr, "Usage: " CFMT " zip-file input-file [hex-password encryption-code]\n", argv[0]);
    return 1;
  }

#ifdef _WIN32
  fd = _wopen(argv[2], O_RDONLY | O_BINARY);
#else
  fd = open(argv[2], O_RDONLY);
#endif
  if (fd == -1) ZERROR(1);

  /* Number of keys */
  if (read(fd, &num_files, sizeof(num_files)) != sizeof(num_files)) {
    ZERROR(2);
  }

  ckeys  = calloc(num_files, sizeof(const char*));
  cfiles = calloc(num_files, sizeof(const char*));
  cdirs  = calloc(num_files, sizeof(int));
  ctimes = calloc(num_files, sizeof(double));

  if (!ckeys || !cfiles || !cdirs || !ctimes) ZERROR(3);

  /* keys first the total size of the buffer in bytes */
  if (read(fd, &ckeysbytes, sizeof(ckeysbytes)) != sizeof(ckeysbytes)) {
    ZERROR(4);
  }
  keysbuffer = malloc(ckeysbytes);
  if (!keysbuffer) ZERROR(5);
  if (read(fd, keysbuffer, ckeysbytes) != ckeysbytes) ZERROR(6);
  for (i = 0, ptr = keysbuffer; i < num_files; ptr++, i++) {
    ckeys[i] = ptr;
    while (*ptr != '\0') ++ptr;
  }

  /* file names next */
  if (read(fd, &cfilesbytes, sizeof(cfilesbytes)) != sizeof(cfilesbytes)) {
    ZERROR(4);
  }
  filesbuffer = malloc(cfilesbytes);
  if (!filesbuffer) ZERROR(7);
  if (read(fd, filesbuffer, cfilesbytes) != cfilesbytes) ZERROR(8);
  for (i = 0, ptr = filesbuffer; i < num_files; ptr++, i++) {
    cfiles[i] = ptr;
    while (*ptr != '\0') ++ptr;
  }

  /* dirs */
  if (read(fd, cdirs, num_files * sizeof(int)) != num_files * sizeof(int)) {
    ZERROR(9);
  }

  /* mtimes */
  if (read(fd, ctimes, num_files * sizeof(double)) !=
      num_files * sizeof(double)) {
    ZERROR(10);
  }

  unsigned char *cpassword = NULL;
  int cpassword_len = 0;
  int cencryption = ZIP_ENCRYPTION_NONE;

  if (argc == 5) {
    cpassword_len = decode_hex_password(argv[3], &cpassword);
    if (cpassword_len < 0) ZERROR(12);
    cencryption = parse_int_arg(argv[4]);
    if (cencryption < 0) ZERROR(13);
  }

  zip_set_error_handler(cmd_zip_error_handler);

#ifdef _WIN32
  char *fn = 0;
  size_t fnlen = 0;
  if (zip__utf16_to_utf8(argv[1], &fn, &fnlen)) ZERROR(11);
#else
  char *fn = argv[1];
#endif

  if (zip_zip(fn, num_files, ckeys, cfiles, cdirs, ctimes,
	      /* compression_level= */ 9, /* cappend= */ 0,
	      cpassword, (size_t) cpassword_len, cencryption,
	      /* progress_fn= */ NULL, /* progress_data= */ NULL)) {
    ZERROR(11);
  }

 cleanup:
  /* No need to clean up, we are exiting */

  if (retval != 0) {
    fprintf(stderr, "Failed to create zip archive " CFMT, argv[1]);
  }

  return retval;
}
