
#include <string.h>
#include <stdint.h>

#define MD5_STATIC static
#include "md5.h"

#include "errors.h"
#include "winfiles.h"

#include <Rinternals.h>

#include <fcntl.h>
#include <unistd.h>

static void bin2str(char *to, const unsigned char *p, size_t len) {
  static const char *hex = "0123456789abcdef";
  for (; len--; p++) {
    *to++ = hex[p[0] >> 4];
    *to++ = hex[p[0] & 0x0f];
  }
}

SEXP clic_md5(SEXP strs) {
  md5_byte_t hash[16];
  char hexhash[32];
  md5_state_t ctx;
  R_xlen_t i, len = XLENGTH(strs);
  SEXP result = PROTECT(Rf_allocVector(STRSXP, len));

  for (i = 0; i < len; i++) {
    const char *s = CHAR(STRING_ELT(strs, i));
    md5_init(&ctx);
    md5_append(&ctx, (const md5_byte_t*) s, strlen(s));
    md5_finish(&ctx, hash);
    bin2str(hexhash, hash, sizeof(hash));
    SET_STRING_ELT(
      result,
      i,
      Rf_mkCharLenCE((const char*) hexhash, sizeof(hexhash), CE_UTF8)
    );
  }

  UNPROTECT(1);
  return result;
}

SEXP clic_md5_raw(SEXP r) {
  Rbyte *ptr = RAW(r);
  Rbyte *end = ptr + XLENGTH(r);
  size_t step = SIZE_MAX < 0x40000000 ? SIZE_MAX & ~63 : 0x40000000;

  md5_state_t ctx;
  md5_byte_t hash[16];
  char hexhash[32];

  md5_init(&ctx);

  while (ptr < end) {
    Rbyte *nxt = ptr + step;
    if (nxt > end) nxt = end;
    md5_append(&ctx, (const md5_byte_t*) ptr, nxt - ptr);
    ptr = nxt;
  }

  md5_finish(&ctx, hash);
  bin2str(hexhash, hash, sizeof(hash));

  return Rf_ScalarString(Rf_mkCharLenCE(
    (const char*) hexhash,
    sizeof(hexhash),
    CE_UTF8
  ));
}

SEXP clic_md5_file(SEXP paths) {
  md5_byte_t hash[16];
  char hexhash[32];
  md5_state_t ctx;
  R_xlen_t i, len = XLENGTH(paths);
#define BUFSIZE (1 * 1024*1024)
  char *buffer = R_alloc(1, BUFSIZE);
  SEXP result = PROTECT(Rf_allocVector(STRSXP, len));

  for (i = 0; i < len; i++) {
    const char *cpath = CHAR(STRING_ELT(paths, i));
    int fd = open_file(cpath, O_RDONLY);
    if (fd == -1) {
      R_THROW_SYSTEM_ERROR("Cannot open file `%s`", cpath);
    }
    md5_init(&ctx);

    ssize_t got = read(fd, buffer, BUFSIZE);
    if (got == -1) {
      close(fd);
      R_THROW_SYSTEM_ERROR("Cannot read from file `%s`", cpath);
    }

    while (got > 0) {
      md5_append(&ctx, (const md5_byte_t*) buffer, got);
      got = read(fd, buffer, BUFSIZE);
      if (got == -1) {
        close(fd);
        R_THROW_SYSTEM_ERROR("Cannot read from file `%s`", cpath);
      }
    }

    close(fd);

    md5_finish(&ctx, hash);
    bin2str(hexhash, hash, sizeof(hash));
    SET_STRING_ELT(
      result,
      i,
      Rf_mkCharLenCE((const char*) hexhash, sizeof(hexhash), CE_UTF8)
    );
  }

  UNPROTECT(1);
  return result;
}
