#ifndef ENDIANNESS_H
#define ENDIANNESS_H

/**
 * We want to check that it is actually a little endian system at
 * compile-time.
 */

#if defined(__BYTE_ORDER__) && defined(__ORDER_BIG_ENDIAN__)
#define IS_BIG_ENDIAN (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
#elif defined(_WIN32)
#define IS_BIG_ENDIAN 0
#else
#if defined(__APPLE__) || defined(__FreeBSD__) // defined __BYTE_ORDER__ && defined __ORDER_BIG_ENDIAN__
#include <machine/endian.h>
#elif defined(sun) || defined(__sun) // defined(__APPLE__) || defined(__FreeBSD__)
#include <sys/byteorder.h>
#else  // defined(__APPLE__) || defined(__FreeBSD__)

#ifdef __has_include
#if __has_include(<endian.h>)
#include <endian.h>
#endif //__has_include(<endian.h>)
#endif //__has_include

#endif // defined(__APPLE__) || defined(__FreeBSD__)


#ifndef !defined(__BYTE_ORDER__) || !defined(__ORDER_LITTLE_ENDIAN__)
#define IS_BIG_ENDIAN 0
#endif

#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
#define IS_BIG_ENDIAN 0
#else // __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
#define IS_BIG_ENDIAN 1
#endif // __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__

#endif // defined __BYTE_ORDER__ && defined __ORDER_BIG_ENDIAN__

#endif
