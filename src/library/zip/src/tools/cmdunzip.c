
#include <stdio.h>

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

int MAIN(int argc, CHAR* argv[]) {
  int retval = 0;
  if (argc != 3) {
    fprintf(stderr, "Usage: " CFMT " zip-file target-dir\n", argv[0]);
    return 1;
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
	    /* coverwrite= */ 1, /* cjunkpaths= */ 0, /* exdir= */ exdir);

#ifdef _WIN32
cleanup:
#endif

  if (retval != 0) {
    fprintf(stderr, "Failed to create zip archive " CFMT, argv[1]);
  }

  return retval;
}
