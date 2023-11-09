
#include <fcntl.h>
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

  if (argc != 3) {
    fprintf(stderr, "Usage: " CFMT " zip-file input-file\n", argv[0]);
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

  zip_set_error_handler(cmd_zip_error_handler);

#ifdef _WIN32
  char *fn = 0;
  size_t fnlen = 0;
  if (zip__utf16_to_utf8(argv[1], &fn, &fnlen)) ZERROR(11);
#else
  char *fn = argv[1];
#endif

  if (zip_zip(fn, num_files, ckeys, cfiles, cdirs, ctimes,
	      /* compression_level= */ 9, /* cappend= */ 0)) {
    ZERROR(11);
  }

 cleanup:
  /* No need to clean up, we are exiting */

  if (retval != 0) {
    fprintf(stderr, "Failed to create zip archive " CFMT, argv[1]);
  }

  return retval;
}
