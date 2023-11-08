/*********************************************************************
* Filename:   sha1.h
* Author:     Brad Conte (brad AT bradconte.com)
* Copyright:
* Disclaimer: This code is presented "as is" without any guarantees.
* Details:    Defines the API for the corresponding SHA1 implementation.
*********************************************************************/

#ifndef SHA1_H
#define SHA1_H

/*************************** HEADER FILES ***************************/
#include <stddef.h>

/****************************** MACROS ******************************/
#define SHA1_BLOCK_SIZE 20              // SHA1 outputs a 20 byte digest

/**************************** DATA TYPES ****************************/
typedef unsigned char BYTE;             // 8-bit byte
typedef unsigned int  WORD32;             // 32-bit word, change to "long" for 16-bit machines

typedef struct {
	BYTE data[64];
	WORD32 datalen;
	unsigned long long bitlen;
	WORD32 state[5];
	WORD32 k[4];
} SHA1_CTX;

/*********************** FUNCTION DECLARATIONS **********************/
void sha1_init(SHA1_CTX *ctx);
void sha1_update(SHA1_CTX *ctx, const BYTE data[], size_t len);
void sha1_final(SHA1_CTX *ctx, BYTE hash[]);

#endif   // SHA1_H

/*********************************************************************
* Filename:   sha1.c
* Author:     Brad Conte (brad AT bradconte.com)
* Copyright:
* Disclaimer: This code is presented "as is" without any guarantees.
* Details:    Implementation of the SHA1 hashing algorithm.
              Algorithm specification can be found here:
               * http://csrc.nist.gov/publications/fips/fips180-2/fips180-2withchangenotice.pdf
              This implementation uses little endian byte order.
*********************************************************************/

/*************************** HEADER FILES ***************************/
#include <stdlib.h>
#include <memory.h>

/****************************** MACROS ******************************/
#define ROTLEFT(a, b) ((a << b) | (a >> (32 - b)))

/*********************** FUNCTION DEFINITIONS ***********************/
void sha1_transform(SHA1_CTX *ctx, const BYTE data[])
{
	WORD32 a, b, c, d, e, i, j, t, m[80];

	for (i = 0, j = 0; i < 16; ++i, j += 4)
          m[i] = ((WORD32)data[j] << 24) + ((WORD32)data[j + 1] << 16) + ((WORD32)data[j + 2] << 8) + ((WORD32)data[j + 3]);
	for ( ; i < 80; ++i) {
		m[i] = (m[i - 3] ^ m[i - 8] ^ m[i - 14] ^ m[i - 16]);
		m[i] = (m[i] << 1) | (m[i] >> 31);
	}

	a = ctx->state[0];
	b = ctx->state[1];
	c = ctx->state[2];
	d = ctx->state[3];
	e = ctx->state[4];

	for (i = 0; i < 20; ++i) {
		t = ROTLEFT(a, 5) + ((b & c) ^ (~b & d)) + e + ctx->k[0] + m[i];
		e = d;
		d = c;
		c = ROTLEFT(b, 30);
		b = a;
		a = t;
	}
	for ( ; i < 40; ++i) {
		t = ROTLEFT(a, 5) + (b ^ c ^ d) + e + ctx->k[1] + m[i];
		e = d;
		d = c;
		c = ROTLEFT(b, 30);
		b = a;
		a = t;
	}
	for ( ; i < 60; ++i) {
		t = ROTLEFT(a, 5) + ((b & c) ^ (b & d) ^ (c & d))  + e + ctx->k[2] + m[i];
		e = d;
		d = c;
		c = ROTLEFT(b, 30);
		b = a;
		a = t;
	}
	for ( ; i < 80; ++i) {
		t = ROTLEFT(a, 5) + (b ^ c ^ d) + e + ctx->k[3] + m[i];
		e = d;
		d = c;
		c = ROTLEFT(b, 30);
		b = a;
		a = t;
	}

	ctx->state[0] += a;
	ctx->state[1] += b;
	ctx->state[2] += c;
	ctx->state[3] += d;
	ctx->state[4] += e;
}

void sha1_init(SHA1_CTX *ctx)
{
	ctx->datalen = 0;
	ctx->bitlen = 0;
	ctx->state[0] = 0x67452301;
	ctx->state[1] = 0xEFCDAB89;
	ctx->state[2] = 0x98BADCFE;
	ctx->state[3] = 0x10325476;
	ctx->state[4] = 0xc3d2e1f0;
	ctx->k[0] = 0x5a827999;
	ctx->k[1] = 0x6ed9eba1;
	ctx->k[2] = 0x8f1bbcdc;
	ctx->k[3] = 0xca62c1d6;
}

void sha1_update(SHA1_CTX *ctx, const BYTE data[], size_t len)
{
	size_t i;

	for (i = 0; i < len; ++i) {
		ctx->data[ctx->datalen] = data[i];
		ctx->datalen++;
		if (ctx->datalen == 64) {
			sha1_transform(ctx, ctx->data);
			ctx->bitlen += 512;
			ctx->datalen = 0;
		}
	}
}

void sha1_final(SHA1_CTX *ctx, BYTE hash[])
{
	WORD32 i;

	i = ctx->datalen;

	// Pad whatever data is left in the buffer.
	if (ctx->datalen < 56) {
		ctx->data[i++] = 0x80;
		while (i < 56)
			ctx->data[i++] = 0x00;
	}
	else {
		ctx->data[i++] = 0x80;
		while (i < 64)
			ctx->data[i++] = 0x00;
		sha1_transform(ctx, ctx->data);
		memset(ctx->data, 0, 56);
	}

	// Append to the padding the total message's length in bits and transform.
	ctx->bitlen += ctx->datalen * 8;
	ctx->data[63] = ctx->bitlen;
	ctx->data[62] = ctx->bitlen >> 8;
	ctx->data[61] = ctx->bitlen >> 16;
	ctx->data[60] = ctx->bitlen >> 24;
	ctx->data[59] = ctx->bitlen >> 32;
	ctx->data[58] = ctx->bitlen >> 40;
	ctx->data[57] = ctx->bitlen >> 48;
	ctx->data[56] = ctx->bitlen >> 56;
	sha1_transform(ctx, ctx->data);

	// Since this implementation uses little endian byte ordering and MD uses big endian,
	// reverse all the bytes when copying the final state to the output hash.
	for (i = 0; i < 4; ++i) {
		hash[i]      = (ctx->state[0] >> (24 - i * 8)) & 0x000000ff;
		hash[i + 4]  = (ctx->state[1] >> (24 - i * 8)) & 0x000000ff;
		hash[i + 8]  = (ctx->state[2] >> (24 - i * 8)) & 0x000000ff;
		hash[i + 12] = (ctx->state[3] >> (24 - i * 8)) & 0x000000ff;
		hash[i + 16] = (ctx->state[4] >> (24 - i * 8)) & 0x000000ff;
	}
}

static void bin2str(char *to, const unsigned char *p, size_t len) {
  static const char *hex = "0123456789abcdef";
  for (; len--; p++) {
    *to++ = hex[p[0] >> 4];
    *to++ = hex[p[0] & 0x0f];
  }
}

#include "errors.h"

#include <stdint.h>

#include <Rinternals.h>

SEXP clic_sha1(SEXP strs) {
  BYTE hash[20];
  char hex[40];
  SHA1_CTX ctx;
  R_xlen_t i, len = XLENGTH(strs);
  SEXP result = PROTECT(Rf_allocVector(STRSXP, len));

  for (i = 0; i < len; i++) {
    const char *s = CHAR(STRING_ELT(strs, i));
    sha1_init(&ctx);
    sha1_update(&ctx, (const BYTE*) s, strlen(s));
    sha1_final(&ctx, hash);
    bin2str(hex, hash, sizeof(hash));
    SET_STRING_ELT(
      result,
      i,
      Rf_mkCharLenCE((const char*) hex, sizeof(hex), CE_UTF8)
    );
  }

  UNPROTECT(1);
  return result;
}

SEXP clic_sha1_raw(SEXP r) {
  Rbyte *ptr = RAW(r);
  Rbyte *end = ptr + XLENGTH(r);
  size_t step = SIZE_MAX < 0x40000000 ? SIZE_MAX & ~63 : 0x40000000;

  SHA1_CTX ctx;
  BYTE hash[20];
  char hex[40];

  sha1_init(&ctx);

  while (ptr < end) {
    Rbyte *nxt = ptr + step;
    if (nxt > end) nxt = end;
    sha1_update(&ctx, ptr, nxt - ptr);
    ptr = nxt;
  }

  sha1_final(&ctx, hash);
  bin2str(hex, hash, sizeof(hash));

  return Rf_ScalarString(Rf_mkCharLenCE(
    (const char*) hex,
    sizeof(hex),
    CE_UTF8
  ));
}

#include "winfiles.h"

#include <fcntl.h>
#include <unistd.h>

SEXP clic_sha1_file(SEXP paths) {
  BYTE hash[20];
  char hex[40];
  SHA1_CTX ctx;
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
    sha1_init(&ctx);

    ssize_t got = read(fd, buffer, BUFSIZE);
    if (got == -1) {
      close(fd);
      R_THROW_SYSTEM_ERROR("Cannot read from file `%s`", cpath);
    }

    while (got > 0) {
      sha1_update(&ctx, (const BYTE*) buffer, got);
      got = read(fd, buffer, BUFSIZE);
      if (got == -1) {
        close(fd);
        R_THROW_SYSTEM_ERROR("Cannot read from file `%s`", cpath);
      }
    }

    close(fd);

    sha1_final(&ctx, hash);
    bin2str(hex, hash, sizeof(hash));
    SET_STRING_ELT(
      result,
      i,
      Rf_mkCharLenCE((const char*) hex, sizeof(hex), CE_UTF8)
    );
  }

  UNPROTECT(1);
  return result;
}
