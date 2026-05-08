#include "pkgcache.h"

#include <string.h>

#if defined(_WIN32)
# include <windows.h>
# define RtlGenRandom SystemFunction036
# ifdef __cplusplus
extern "C"
# endif
BOOLEAN NTAPI RtlGenRandom(PVOID RandomBuffer, ULONG RandomBufferLength);
# pragma comment(lib, "advapi32.lib")
#elif defined(__APPLE__) || defined(__FreeBSD__) || defined(__OpenBSD__) || \
      defined(__NetBSD__) || defined(__DragonFly__)
# include <stdlib.h>
#else
# include <errno.h>
# include <fcntl.h>
# include <unistd.h>
# if defined(__linux__)
#  include <sys/syscall.h>
# endif
#endif

SEXP pkgcache_rand_bytes(SEXP n) {
  int size = Rf_asInteger(n);
  if (size == NA_INTEGER || size < 0) {
    Rf_error("Invalid number of random bytes requested");
  }
  SEXP res = PROTECT(Rf_allocVector(RAWSXP, size));
  if (size == 0) {
    UNPROTECT(1);
    return res;
  }
  unsigned char *buf = RAW(res);

#if defined(_WIN32)
  if (!RtlGenRandom((PVOID) buf, (ULONG) size)) {
    Rf_error("Failed to obtain random bytes from RtlGenRandom");
  }

#elif defined(__APPLE__) || defined(__FreeBSD__) || defined(__OpenBSD__) || \
      defined(__NetBSD__) || defined(__DragonFly__)
  arc4random_buf(buf, (size_t) size);

#else
  size_t off = 0;
# if defined(__linux__) && defined(SYS_getrandom)
  while (off < (size_t) size) {
    long r = syscall(SYS_getrandom, buf + off, (size_t) size - off, 0);
    if (r > 0) {
      off += (size_t) r;
    } else if (r < 0 && (errno == EINTR || errno == EAGAIN)) {
      continue;
    } else {
      break; /* fall through to /dev/urandom */
    }
  }
# endif
  if (off < (size_t) size) {
    int fd;
    do {
      fd = open("/dev/urandom", O_RDONLY);
    } while (fd < 0 && errno == EINTR);
    if (fd < 0) {
      Rf_error("Failed to open /dev/urandom: %s", strerror(errno));
    }
    while (off < (size_t) size) {
      ssize_t r = read(fd, buf + off, (size_t) size - off);
      if (r > 0) {
        off += (size_t) r;
      } else if (r < 0 && errno == EINTR) {
        continue;
      } else {
        close(fd);
        Rf_error("Failed to read from /dev/urandom: %s", strerror(errno));
      }
    }
    close(fd);
  }
#endif

  UNPROTECT(1);
  return res;
}
