#include <string.h>
#include <inttypes.h>

#include <Rinternals.h>

#include "errors.h"

// must match xxhash.c
#define XXH_STATIC_LINKING_ONLY   /* access advanced declarations */
#define XXH_IMPLEMENTATION   /* access definitions */
#define XXH_INLINE_ALL
#include "xxhash.h"

SEXP clic_xxhash(SEXP strs) {
  XXH128_hash_t hash;
  char str[32 + 1];
  R_xlen_t i, len = XLENGTH(strs);
  SEXP result = PROTECT(Rf_allocVector(STRSXP, len));

  for (i = 0; i < len; i++) {
    const char *s = CHAR(STRING_ELT(strs, i));
    hash = XXH3_128bits_withSeed(s, strlen(s), 0);
    snprintf(str, sizeof(str), "%016" PRIx64 "%016" PRIx64, hash.high64, hash.low64);
    SET_STRING_ELT(result, i, Rf_mkCharLenCE(str, 32, CE_UTF8));
  }

  UNPROTECT(1);
  return result;
}

SEXP clic_xxhash64(SEXP strs) {
  XXH64_hash_t hash;
  char str[16 + 1];
  R_xlen_t i, len = XLENGTH(strs);
  SEXP result = PROTECT(Rf_allocVector(STRSXP, len));

  for (i = 0; i < len; i++) {
    const char *s = CHAR(STRING_ELT(strs, i));
    hash = XXH3_64bits_withSeed(s, strlen(s), 0);
    snprintf(str, sizeof(str), "%016" PRIx64, hash);
    SET_STRING_ELT(result, i, Rf_mkCharLenCE(str, 16, CE_UTF8));
  }

  UNPROTECT(1);
  return result;
}

SEXP clic_xxhash_raw(SEXP r) {
  XXH128_hash_t hash = XXH3_128bits_withSeed(
    RAW(r), Rf_length(r), 0
  );
  char str[32 + 1];
  snprintf(str, sizeof(str), "%016" PRIx64 "%016" PRIx64, hash.high64, hash.low64);
  return Rf_mkString(str);
}

SEXP clic_xxhash64_raw(SEXP r) {
  XXH64_hash_t hash = XXH3_64bits_withSeed(
    RAW(r), Rf_length(r), 0
  );
  char str[16 + 1];
  snprintf(str, sizeof(str), "%016" PRIx64, hash);
  return Rf_mkString(str);
}

#include "winfiles.h"

#include <fcntl.h>
#include <unistd.h>

SEXP clic_xxhash_file(SEXP paths) {
  R_xlen_t i, len = XLENGTH(paths);
  size_t const bufferSize = 1 * 1024 * 1024;
  char str[32 + 1];
  char *buffer = R_alloc(1, bufferSize);
  SEXP result = PROTECT(Rf_allocVector(STRSXP, len));
  XXH128_hash_t hash;
  XXH3_state_t* const state = XXH3_createState();
  if (state == NULL) {
    R_THROW_ERROR("Failed to init xx hash state");
  }

  for (i = 0; i < len; i++) {
    const char *cpath = CHAR(STRING_ELT(paths, i));
    int fd = open_file(cpath, O_RDONLY);
    if (fd == -1) {
      R_THROW_SYSTEM_ERROR("Cannot open file `%s`", cpath);
    }
    if (XXH3_128bits_reset(state) == XXH_ERROR) {
      close(fd);
      R_THROW_ERROR("Could not initialize xxhash");
    }

    ssize_t got = read(fd, buffer, bufferSize);
    if (got == -1) {
      close(fd);
      R_THROW_SYSTEM_ERROR("Cannot read from file `%s`", cpath);
    }

    while (got > 0) {
      if (XXH3_128bits_update(state, buffer, got) == XXH_ERROR) {
        close(fd);
        R_THROW_ERROR("Failed to calcu;late xxhash");
      }
      got = read(fd, buffer, bufferSize);
      if (got == -1) {
        close(fd);
        R_THROW_SYSTEM_ERROR("Cannot read from file `%s`", cpath);
      }
    }

    close(fd);

    hash = XXH3_128bits_digest(state);
    snprintf(str, sizeof(str), "%016" PRIx64 "%016" PRIx64, hash.high64, hash.low64);
    SET_STRING_ELT(result, i, Rf_mkCharLen(str, 32));
  }

  UNPROTECT(1);
  return result;
}

SEXP clic_xxhash64_file(SEXP paths) {
  R_xlen_t i, len = XLENGTH(paths);
  size_t const bufferSize = 1 * 1024 * 1024;
  char str[16 + 1];
  char *buffer = R_alloc(1, bufferSize);
  SEXP result = PROTECT(Rf_allocVector(STRSXP, len));
  XXH64_hash_t hash;
  XXH3_state_t* const state = XXH3_createState();
  if (state == NULL) {
    R_THROW_ERROR("Failed to init xx hash state");
  }

  for (i = 0; i < len; i++) {
    const char *cpath = CHAR(STRING_ELT(paths, i));
    int fd = open_file(cpath, O_RDONLY);
    if (fd == -1) {
      R_THROW_SYSTEM_ERROR("Cannot open file `%s`", cpath);
    }
    if (XXH3_64bits_reset(state) == XXH_ERROR) {
      close(fd);
      R_THROW_ERROR("Could not initialize xxhash");
    }

    ssize_t got = read(fd, buffer, bufferSize);
    if (got == -1) {
      close(fd);
      R_THROW_SYSTEM_ERROR("Cannot read from file `%s`", cpath);
    }

    while (got > 0) {
      if (XXH3_64bits_update(state, buffer, got) == XXH_ERROR) {
        close(fd);
        R_THROW_ERROR("Failed to calcu;late xxhash");
      }
      got = read(fd, buffer, bufferSize);
      if (got == -1) {
        close(fd);
        R_THROW_SYSTEM_ERROR("Cannot read from file `%s`", cpath);
      }
    }

    close(fd);

    hash = XXH3_64bits_digest(state);
    snprintf(str, sizeof(str), "%016" PRIx64, hash);
    SET_STRING_ELT(result, i, Rf_mkCharLen(str, 16));
  }

  UNPROTECT(1);
  return result;
}
