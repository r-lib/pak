
#include <string.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <errno.h>

#include "zip.h"

/* -------------------------------------------------------------- */

FILE *zip_open_utf8(const char *filename, const char *mode,
                    char **buffer, size_t *buffer_size) {
  FILE *fh = fopen(filename, mode);
  return fh;
}


int zip_str_file_path(const char *cexdir, const char *key,
                      char **buffer, size_t *buffer_size,
                      int cjunkpaths) {

  size_t len1 = strlen(cexdir);
  size_t need_size, len2;
  char *newbuffer;

  if (cjunkpaths) {
    char *base = strrchr(key, '/');
    if (base) key = base;
  }

  len2 = strlen(key);
  need_size = len1 + len2 + 2;

  if (*buffer_size < need_size) {
    newbuffer = realloc((void*) *buffer, need_size);
    if (!newbuffer) return 1;

    *buffer = newbuffer;
    *buffer_size = need_size;
  }

  strcpy(*buffer, cexdir);
  (*buffer)[len1] = '/';
  strcpy(*buffer + len1 + 1, key);

  return 0;
}

int zip_mkdirp(char *path, int complete)  {
  char *p;
  int status;

  errno = 0;

  /* Iterate the string */
  for (p = path + 1; *p; p++) {
    if (*p == '/') {
      *p = '\0';
      status = mkdir(path, S_IRWXU);
      *p = '/';
      if (status && errno != EEXIST) {
        return 1;
      }
    }
  }

  if (complete) {
    status = mkdir(path, S_IRWXU);
    if ((status && errno != EEXIST)) return 1;
  }

  return 0;
}

int zip_file_exists(char *filename) {
  struct stat st;
  return ! stat(filename, &st);
}

int zip_set_mtime(const char *filename, time_t mtime) {
  struct timeval times[2];
  times[0].tv_sec  = times[1].tv_sec = mtime;
  times[0].tv_usec = times[1].tv_usec = 0;
  return utimes(filename, times);
}

int zip_file_size(FILE *fh, mz_uint64 *size) {
  if (fseek(fh, 0, SEEK_END)) return 1;
  *size = ftello(fh);
  if (*size == -1) return 2;
  if (fseek(fh, 0, SEEK_SET)) return 3;
  return 0;
}
